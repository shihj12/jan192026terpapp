# =========================================================
# R/engines/stats/1dgofcs.R — 1D GO Functional Class Scoring Engine
#
# Perseus-style 1D FCS on a named score vector (proteinID).
# Uses rank-based scoring (Perseus method):
#   1. Convert raw scores to ranks (1 to N)
#   2. Wilcoxon test on ranks (not raw values)
#   3. Score = 2 * (mean_rank_in - mean_rank_out) / N
#      -> Normalized to [-1, 1] range
#
# Contract v1.3:
#  - data$terms: term_id, term_name, fdr, score, neglog10_fdr, n_genes, protein_ids
#  - Input is named score vector from PCA loadings or FC
#  - Output scores are in range [-1, 1] (rank-normalized)
#  - protein_ids: comma-separated list of protein IDs in term (for gene modal)
# =========================================================

#' Execute 1dgofcs engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_1dgofcs_run <- function(payload, params = NULL, context = NULL) {
  # Track engine start time
  engine_start <- Sys.time()

  params <- params %||% payload$params %||% list()

  # Initialize log entries
  log_entries <- list()
  add_log <- function(level, msg) {
    log_entries <<- c(log_entries, list(list(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      level = level,
      message = msg
    )))
  }

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", "Initiating 1D GO-FCS analysis")

  # Get parameters
  ontology <- params$ontology %||% "all"
  fdr_cutoff <- params$fdr_cutoff %||% 0.03
  min_term_size <- params$min_term_size %||% 5
  max_terms <- params$max_terms %||% 20
  # FIX: score_label provides axis context (e.g., "PC1" or "log2(BafA1/Control)")
  score_label <- params$score_label %||% payload$score_label %||% context$score_label %||% "Score"

  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, ontology=%s",
                          fdr_cutoff, min_term_size, ontology))

  # Get score vector (from parent PCA loadings or FC computation)
  # Priority: payload$scores > params > context > standalone FC mode
  scores <- payload$scores %||% params$scores %||% context$scores %||% NULL

  # Multi-analysis results for standalone FC mode
  analyses <- list()
  fc_vectors <- list()  # Initialize for standalone FC mode

  if (is.null(scores) || length(scores) == 0) {
    # Try standalone FC mode: compute FC vectors from groups
    mat <- payload$mat
    samples <- payload$samples
    groups <- payload$groups
    ids <- payload$ids

    if (!is.null(mat) && !is.null(groups) && length(groups) >= 2) {
      add_log("INFO", "Standalone FC mode: computing fold-change vectors")

      # Get protein IDs
      protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
      protein_ids <- if (nzchar(protein_col) && protein_col %in% names(ids)) {
        as.character(ids[[protein_col]])
      } else {
        rownames(mat)
      }

      # Get control info from metadata
      meta_groups <- payload$metadata$groups
      is_control <- if (!is.null(meta_groups$is_control)) {
        tolower(as.character(meta_groups$is_control)) %in% c("true", "t", "1", "yes")
      } else {
        rep(FALSE, nrow(meta_groups %||% data.frame()))
      }
      group_names <- as.character(meta_groups$group_name %||% character(0))

      # Match is_control to our groups vector
      is_control_map <- rep(FALSE, length(groups))
      for (idx in seq_along(groups)) {
        match_idx <- match(groups[idx], group_names)
        if (!is.na(match_idx)) is_control_map[idx] <- is_control[match_idx]
      }

      control_only <- isTRUE(params$control_only)
      control_idx <- which(is_control_map)
      has_control <- length(control_idx) == 1

      if (control_only && has_control) {
        add_log("INFO", sprintf("Control-only mode: generating FC vectors against control (%s)",
                                groups[control_idx]))
      }

      # Compute FC vectors for each comparison
      n_groups <- length(groups)
      fc_vectors <- list()

      for (i in seq_len(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          # Skip non-control comparisons if control_only is enabled
          if (control_only && has_control) {
            if (!i %in% control_idx && !j %in% control_idx) {
              next
            }
          }

          grp_a <- groups[i]
          grp_b <- groups[j]

          # Control orientation: control should always be the denominator/baseline (grp_a)
          # FC = log2(grp_b/grp_a), so positive FC means higher in grp_b
          # Only swap when control is at j position (grp_b) and i is not control
          if (has_control && j %in% control_idx && !(i %in% control_idx)) {
            tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
          }
          # If i is control, grp_a is already control (baseline) - no swap needed

          # Compute FC vector
          cols_a <- samples$sample_col[samples$group_name == grp_a]
          cols_b <- samples$sample_col[samples$group_name == grp_b]
          cols_a <- intersect(cols_a, colnames(mat))
          cols_b <- intersect(cols_b, colnames(mat))

          if (length(cols_a) > 0 && length(cols_b) > 0) {
            mat_a <- mat[, cols_a, drop = FALSE]
            mat_b <- mat[, cols_b, drop = FALSE]
            mean_a <- rowMeans(mat_a, na.rm = TRUE)
            mean_b <- rowMeans(mat_b, na.rm = TRUE)
            fc <- log2(mean_b + 1) - log2(mean_a + 1)
            names(fc) <- protein_ids

            comp_key <- paste0(grp_b, "_vs_", grp_a)
            fc_vectors[[comp_key]] <- fc
            add_log("INFO", sprintf("  %s: computed", comp_key))
          }
        }
      }

      # FIX: Store all fc_vectors for multi-comparison analysis when control_only is off
      # If only one vector, use it as scores; if multiple, we'll run GO-FCS for each
      if (length(fc_vectors) > 1) {
        add_log("INFO", sprintf("Standalone mode: %d pairwise comparisons to analyze", length(fc_vectors)))
      } else if (length(fc_vectors) == 1) {
        scores <- fc_vectors[[1]]
        score_label <- paste0("log2(", names(fc_vectors)[1], ")")
        add_log("INFO", sprintf("Using %s as primary score vector", score_label))
      }
    }
  }

  if ((is.null(scores) || length(scores) == 0) && length(fc_vectors) == 0) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No score vector provided for 1D GO-FCS",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Get terpbase mappings
  terpbase <- payload$terpbase %||% context$terpbase

  if (is.null(terpbase)) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "1D GO-FCS requires terpbase with GO annotations",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Extract GO mappings (same logic as goora)
  protein_to_go <- terpbase$protein_to_go %||% terpbase$protein_terms %||% NULL
  go_terms <- terpbase$go_terms %||% terpbase$terms %||% NULL

  if (is.null(protein_to_go)) {
    annotations <- terpbase$annotations %||% NULL
    if (!is.null(annotations) && "go_ids" %in% names(annotations)) {
      protein_to_go <- annotations
    }
  }

  if (is.null(protein_to_go)) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Terpbase missing protein_to_go mapping",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build term-to-protein mapping
  term_proteins <- list()

  if (is.data.frame(protein_to_go)) {
    pid_col_candidates <- intersect(c("protein_id", "uniprot_id", "id"), names(protein_to_go))
    go_col_candidates <- intersect(c("go_id", "go_ids", "term_id"), names(protein_to_go))
    pid_col <- if (length(pid_col_candidates) > 0) pid_col_candidates[1] else NA_character_
    go_col <- if (length(go_col_candidates) > 0) go_col_candidates[1] else NA_character_

    if (!is.na(pid_col) && !is.na(go_col)) {
      for (i in seq_len(nrow(protein_to_go))) {
        pid <- as.character(protein_to_go[[pid_col]][i])
        go_ids <- protein_to_go[[go_col]][i]

        if (is.character(go_ids)) {
          go_ids <- unlist(strsplit(go_ids, "[,;]"))
        }

        for (go_id in go_ids) {
          go_id <- trimws(go_id)
          if (nzchar(go_id)) {
            term_proteins[[go_id]] <- c(term_proteins[[go_id]], pid)
          }
        }
      }
    }
  } else if (is.list(protein_to_go)) {
    for (pid in names(protein_to_go)) {
      go_ids <- protein_to_go[[pid]]
      for (go_id in go_ids) {
        if (nzchar(go_id)) {
          term_proteins[[go_id]] <- c(term_proteins[[go_id]], pid)
        }
      }
    }
  }

  if (length(term_proteins) == 0) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No valid GO term mappings found",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", sprintf("Loaded %d GO term mappings", length(term_proteins)))

  # NOTE: As of v1.5, GO engines always compute ALL ontologies (BP, CC, MF).
  # The params$ontology setting is now deprecated in favor of style$ontology_filter
  # which filters at render/viewer time. This allows users to change ontology filter
  # without re-running the analysis.
  filter_ontology <- NULL

  # FIX: Multi-comparison mode for standalone FC analysis
  # When control_only is off and multiple FC vectors exist, run GO-FCS for each
  # Uses Perseus-style rank-based scoring for each comparison
  if (length(fc_vectors) > 1) {
    add_log("INFO", "Running GO-FCS for all pairwise comparisons (rank-based)")

    analyses <- list()
    for (comp_name in names(fc_vectors)) {
      comp_scores <- fc_vectors[[comp_name]]
      comp_label <- paste0("log2(", gsub("_vs_", "/", comp_name), ")")
      add_log("INFO", sprintf("  Analyzing %s", comp_name))

      # Convert this comparison's scores to ranks (Perseus-style)
      comp_vals <- as.numeric(comp_scores)
      comp_pids <- names(comp_scores)
      valid_mask <- !is.na(comp_vals)
      valid_vals <- comp_vals[valid_mask]
      valid_pids <- comp_pids[valid_mask]

      N <- length(valid_vals)
      comp_ranks <- rank(valid_vals, ties.method = "average")
      names(comp_ranks) <- valid_pids

      # Run GO-FCS for this comparison
      comp_results <- list()

      for (term_id in names(term_proteins)) {
        term_pids <- unique(term_proteins[[term_id]])
        term_pids_in_data <- intersect(term_pids, valid_pids)

        if (length(term_pids_in_data) < min_term_size) next

        # Get term info
        term_name <- term_id
        term_ont <- "unknown"

        if (!is.null(go_terms)) {
          if (is.data.frame(go_terms)) {
            term_row <- go_terms[go_terms$term_id == term_id | go_terms$go_id == term_id, , drop = FALSE]
            if (nrow(term_row) > 0) {
              name_col_candidates <- intersect(c("term_name", "name", "description"), names(term_row))
              ont_col_candidates <- intersect(c("ontology", "namespace", "ont"), names(term_row))
              name_col <- if (length(name_col_candidates) > 0) name_col_candidates[1] else NA_character_
              ont_col <- if (length(ont_col_candidates) > 0) ont_col_candidates[1] else NA_character_
              if (!is.na(name_col)) term_name <- as.character(term_row[[name_col]][1])
              if (!is.na(ont_col)) term_ont <- as.character(term_row[[ont_col]][1])
            }
          } else if (is.list(go_terms) && !is.null(go_terms[[term_id]])) {
            ti <- go_terms[[term_id]]
            term_name <- ti$name %||% ti$term_name %||% term_id
            term_ont <- ti$ontology %||% ti$namespace %||% "unknown"
          }
        }

        # Filter by ontology
        if (!is.null(filter_ontology)) {
          if (!grepl(filter_ontology, term_ont, ignore.case = TRUE)) next
        }

        # Get ranks for term vs others (Perseus-style)
        ranks_in <- comp_ranks[term_pids_in_data]
        other_pids <- setdiff(valid_pids, term_pids_in_data)
        ranks_out <- comp_ranks[other_pids]

        # Edge case: need enough proteins in "out" group
        if (length(ranks_out) < min_term_size) next

        # Wilcoxon test on ranks
        pval <- tryCatch({
          wt <- suppressWarnings(wilcox.test(ranks_in, ranks_out,
                                              alternative = "two.sided",
                                              exact = FALSE))
          wt$p.value
        }, error = function(e) 1)

        # Perseus-style score: 2 * (mean_rank_in - mean_rank_out) / N
        mean_rank_in <- mean(ranks_in, na.rm = TRUE)
        mean_rank_out <- mean(ranks_out, na.rm = TRUE)
        score <- 2 * (mean_rank_in - mean_rank_out) / N

        comp_results <- c(comp_results, list(list(
          term_id = term_id,
          term_name = term_name,
          ontology = term_ont,
          pval = pval,
          score = score,
          n_genes = length(term_pids_in_data),
          protein_ids = paste(term_pids_in_data, collapse = ",")
        )))
      }

      # Build terms data.frame for this comparison
      if (length(comp_results) > 0) {
        terms_df <- do.call(rbind, lapply(comp_results, function(r) {
          data.frame(
            term_id = r$term_id,
            term_name = r$term_name,
            ontology = r$ontology,
            pval = r$pval,
            score = r$score,
            n_genes = r$n_genes,
            protein_ids = r$protein_ids,
            stringsAsFactors = FALSE
          )
        }))
        terms_df$fdr <- p.adjust(terms_df$pval, method = "BH")
        terms_df$neglog10_fdr <- -log10(pmax(terms_df$fdr, 1e-300))
        terms_df <- terms_df[terms_df$fdr <= fdr_cutoff, , drop = FALSE]
        terms_df <- terms_df[order(terms_df$fdr, -abs(terms_df$score)), , drop = FALSE]
        # Limit to max_terms PER ONTOLOGY (not globally)
        if ("ontology" %in% names(terms_df) && nrow(terms_df) > 0) {
          ontologies <- unique(terms_df$ontology)
          terms_list <- lapply(ontologies, function(ont) {
            ont_df <- terms_df[terms_df$ontology == ont, , drop = FALSE]
            if (nrow(ont_df) > max_terms) {
              ont_df <- ont_df[1:max_terms, , drop = FALSE]
            }
            ont_df
          })
          terms_df <- do.call(rbind, terms_list)
          terms_df <- terms_df[order(terms_df$fdr, -abs(terms_df$score)), , drop = FALSE]
        } else if (nrow(terms_df) > max_terms) {
          terms_df <- terms_df[1:max_terms, , drop = FALSE]
        }
        terms_df <- terms_df[, c("term_id", "term_name", "ontology", "fdr", "neglog10_fdr", "score", "n_genes", "protein_ids"), drop = FALSE]
        rownames(terms_df) <- NULL

        analyses[[comp_name]] <- list(
          terms = terms_df,
          score_label = comp_label,
          comparison = comp_name
        )
      }
    }

    # Log engine runtime
    engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
    add_log("INFO", sprintf("1D GO-FCS multi-comparison completed in %.2f seconds (%d comparisons)",
                            engine_duration, length(analyses)))

    # Build log data.frame
    log_df <- do.call(rbind, lapply(log_entries, function(e) {
      data.frame(
        time = e$time,
        level = e$level,
        message = e$message,
        stringsAsFactors = FALSE
      )
    }))

    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        analyses = analyses,  # Multi-comparison structure
        log = log_df
      )
    ))
  }

  # Single comparison mode (original path)
  add_log("INFO", sprintf("Score vector: %d proteins", length(scores)))

  # 1D FCS (Perseus-style rank-based):
  #   1. Convert raw scores to ranks once (before term loop)
  #   2. For each term, do Wilcoxon test on ranks
  #   3. Score = 2 * (mean_rank_in - mean_rank_out) / N

  protein_ids <- names(scores)
  all_scores <- as.numeric(scores)

  # Filter out NA values before ranking
  valid_mask <- !is.na(all_scores)
  valid_scores <- all_scores[valid_mask]
  valid_pids <- protein_ids[valid_mask]

  # Convert scores to ranks (1 to N, higher rank = higher score)
  N <- length(valid_scores)
  ranks <- rank(valid_scores, ties.method = "average")
  names(ranks) <- valid_pids

  add_log("INFO", sprintf("Rank conversion: %d proteins, range [%.1f, %.1f]",
                          N, min(ranks), max(ranks)))

  results <- list()

  for (term_id in names(term_proteins)) {
    term_pids <- unique(term_proteins[[term_id]])
    term_pids_in_data <- intersect(term_pids, valid_pids)

    if (length(term_pids_in_data) < min_term_size) next

    # Get term info
    term_name <- term_id
    term_ont <- "unknown"

    if (!is.null(go_terms)) {
      if (is.data.frame(go_terms)) {
        term_row <- go_terms[go_terms$term_id == term_id | go_terms$go_id == term_id, , drop = FALSE]
        if (nrow(term_row) > 0) {
          name_col_candidates <- intersect(c("term_name", "name", "description"), names(term_row))
          ont_col_candidates <- intersect(c("ontology", "namespace", "ont"), names(term_row))
          name_col <- if (length(name_col_candidates) > 0) name_col_candidates[1] else NA_character_
          ont_col <- if (length(ont_col_candidates) > 0) ont_col_candidates[1] else NA_character_
          if (!is.na(name_col)) term_name <- as.character(term_row[[name_col]][1])
          if (!is.na(ont_col)) term_ont <- as.character(term_row[[ont_col]][1])
        }
      } else if (is.list(go_terms) && !is.null(go_terms[[term_id]])) {
        ti <- go_terms[[term_id]]
        term_name <- ti$name %||% ti$term_name %||% term_id
        term_ont <- ti$ontology %||% ti$namespace %||% "unknown"
      }
    }

    # Filter by ontology
    if (!is.null(filter_ontology)) {
      if (!grepl(filter_ontology, term_ont, ignore.case = TRUE)) next
    }

    # Get ranks for term proteins vs others (Perseus-style)
    ranks_in <- ranks[term_pids_in_data]
    other_pids <- setdiff(valid_pids, term_pids_in_data)
    ranks_out <- ranks[other_pids]

    # Edge case: need enough proteins in "out" group
    if (length(ranks_out) < min_term_size) next

    # Wilcoxon test on ranks (not raw scores)
    pval <- tryCatch({
      wt <- suppressWarnings(wilcox.test(ranks_in, ranks_out,
                                          alternative = "two.sided",
                                          exact = FALSE))
      wt$p.value
    }, error = function(e) 1)

    # Perseus-style score: 2 * (mean_rank_in - mean_rank_out) / N
    # This normalizes to [-1, 1] range
    mean_rank_in <- mean(ranks_in, na.rm = TRUE)
    mean_rank_out <- mean(ranks_out, na.rm = TRUE)
    score <- 2 * (mean_rank_in - mean_rank_out) / N

    results <- c(results, list(list(
      term_id = term_id,
      term_name = term_name,
      ontology = term_ont,
      pval = pval,
      score = score,
      n_genes = length(term_pids_in_data),
      protein_ids = paste(term_pids_in_data, collapse = ",")
    )))
  }

  if (length(results) == 0) {
    return(list(
      engine_id = "1dgofcs",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          score = numeric(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "INFO",
                         message = "1D GO-FCS: No significant terms found",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build data.frame
  terms_df <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      term_id = r$term_id,
      term_name = r$term_name,
      ontology = r$ontology,
      pval = r$pval,
      score = r$score,
      n_genes = r$n_genes,
      protein_ids = r$protein_ids,
      stringsAsFactors = FALSE
    )
  }))

  # Compute FDR
  terms_df$fdr <- p.adjust(terms_df$pval, method = "BH")

  # Add neglog10_fdr column for visualization
  terms_df$neglog10_fdr <- -log10(pmax(terms_df$fdr, 1e-300))

  # Filter and sort
  terms_df <- terms_df[terms_df$fdr <= fdr_cutoff, , drop = FALSE]
  terms_df <- terms_df[order(terms_df$fdr, -abs(terms_df$score)), , drop = FALSE]

  # Limit to max_terms PER ONTOLOGY (not globally)
  if ("ontology" %in% names(terms_df) && nrow(terms_df) > 0) {
    ontologies <- unique(terms_df$ontology)
    terms_list <- lapply(ontologies, function(ont) {
      ont_df <- terms_df[terms_df$ontology == ont, , drop = FALSE]
      if (nrow(ont_df) > max_terms) {
        ont_df <- ont_df[1:max_terms, , drop = FALSE]
      }
      ont_df
    })
    terms_df <- do.call(rbind, terms_list)
    terms_df <- terms_df[order(terms_df$fdr, -abs(terms_df$score)), , drop = FALSE]
  } else if (nrow(terms_df) > max_terms) {
    terms_df <- terms_df[1:max_terms, , drop = FALSE]
  }

  terms_df <- terms_df[, c("term_id", "term_name", "ontology", "fdr", "neglog10_fdr", "score", "n_genes", "protein_ids"), drop = FALSE]
  rownames(terms_df) <- NULL

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("1D GO-FCS completed in %.2f seconds (%d terms)",
                          engine_duration, nrow(terms_df)))

  # Build log data.frame from accumulated entries
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  list(
    engine_id = "1dgofcs",
    params = params,
    data = list(
      terms = terms_df,
      score_label = score_label,  # FIX: Include for axis labeling
      log = log_df
    )
  )
}

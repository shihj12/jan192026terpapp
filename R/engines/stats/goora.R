# =========================================================
# R/engines/stats/goora.R — GO Over-Representation Analysis Engine
#
# Performs GO-ORA on proteinID sets with required fold_enrichment.
#
# Contract v1.1:
#  - data$terms: term_id, term_name, fdr, fold_enrichment, n_genes
#  - Input is proteinID sets from volcano/pca
# =========================================================

#' Execute goora engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with terpbase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_goora_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", "Initiating GO-ORA analysis")

  # Get parameters
  ontology <- params$ontology %||% "all"
  fdr_cutoff <- params$fdr_cutoff %||% 0.03
  min_term_size <- params$min_term_size %||% 5
  min_overlap <- params$min_overlap %||% 1
  max_terms <- params$max_terms %||% 20

  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, min_overlap=%d, ontology=%s",
                          fdr_cutoff, min_term_size, min_overlap, ontology))

  # Get query proteins (from frozen parent sets)
  # Priority: payload$query_proteins > params > context
  query_proteins <- payload$query_proteins %||% params$query_proteins %||% context$query_proteins %||% character(0)

  if (length(query_proteins) == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No query proteins provided for GO-ORA",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Get terpbase mappings
  terpbase <- payload$terpbase %||% context$terpbase

  if (is.null(terpbase)) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "GO-ORA requires terpbase with GO annotations",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Extract GO mappings from terpbase
  # Expected structure: terpbase$go_terms (term annotations) and
  # terpbase$protein_to_go (protein to term mapping)

  go_terms <- terpbase$go_terms %||% terpbase$terms %||% NULL
  protein_to_go <- terpbase$protein_to_go %||% terpbase$protein_terms %||% NULL
  background <- terpbase$background %||% terpbase$universe %||% NULL

  if (is.null(protein_to_go)) {
    # Try to build from annotations
    annotations <- terpbase$annotations %||% NULL
    if (!is.null(annotations) && "go_ids" %in% names(annotations)) {
      protein_to_go <- annotations
    }
  }

  if (is.null(protein_to_go)) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = "Terpbase missing protein_to_go mapping",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build term-to-protein mapping
  # Assume protein_to_go is a data.frame with protein_id and go_id columns
  # or a list mapping proteins to GO terms

  term_proteins <- list()

  if (is.data.frame(protein_to_go)) {
    # Data.frame format
    pid_col_candidates <- intersect(c("protein_id", "uniprot_id", "id"), names(protein_to_go))
    go_col_candidates <- intersect(c("go_id", "go_ids", "term_id"), names(protein_to_go))
    pid_col <- if (length(pid_col_candidates) > 0) pid_col_candidates[1] else NA_character_
    go_col <- if (length(go_col_candidates) > 0) go_col_candidates[1] else NA_character_

    if (!is.na(pid_col) && !is.na(go_col)) {
      for (i in seq_len(nrow(protein_to_go))) {
        pid <- as.character(protein_to_go[[pid_col]][i])
        go_ids <- protein_to_go[[go_col]][i]

        if (is.character(go_ids)) {
          # May be comma-separated
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
    # List format: names are proteins, values are GO term vectors
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
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No valid GO term mappings found in terpbase",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  add_log("INFO", sprintf("Loaded %d GO term mappings", length(term_proteins)))

  # Define background universe
  if (is.null(background)) {
    background <- unique(unlist(term_proteins))
  }
  N <- length(background)  # Total proteins in universe

  # Query set
  query_set <- intersect(query_proteins, background)
  n <- length(query_set)  # Proteins in query set

  add_log("INFO", sprintf("Query: %d proteins, Background: %d proteins",
                          n, N))

  if (n == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "WARN",
                         message = "No query proteins found in background universe",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # NOTE: As of v1.5, GO engines always compute ALL ontologies (BP, CC, MF).
  # The params$ontology setting is now deprecated in favor of style$ontology_filter
  # which filters at render/viewer time. This allows users to change ontology filter
  # without re-running the analysis.
  # Keeping filter_ontology = NULL to ensure all terms are computed.
  filter_ontology <- NULL

  # Perform hypergeometric test for each term
  results <- list()

  for (term_id in names(term_proteins)) {
    term_pids <- unique(term_proteins[[term_id]])
    M <- length(term_pids)  # Proteins annotated to this term

    # Skip small terms
    if (M < min_term_size) next

    # Overlap with query
    k <- length(intersect(query_set, term_pids))
    if (k == 0) next

    # Hypergeometric test (over-representation)
    # P(X >= k) where X ~ Hypergeometric(N, M, n)
    pval <- phyper(k - 1, M, N - M, n, lower.tail = FALSE)

    # Fold enrichment = (k/n) / (M/N)
    expected <- (n * M) / N
    fold_enrichment <- k / expected

    # Get term name and ontology
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

    results <- c(results, list(list(
      term_id = term_id,
      term_name = term_name,
      ontology = term_ont,
      pval = pval,
      fold_enrichment = fold_enrichment,
      n_genes = k,
      n_term = M,
      protein_ids = paste(intersect(query_set, term_pids), collapse = ",")
    )))
  }

  if (length(results) == 0) {
    return(list(
      engine_id = "goora",
      params = params,
      data = list(
        terms = data.frame(
          term_id = character(0),
          term_name = character(0),
          ontology = character(0),
          fdr = numeric(0),
          fold_enrichment = numeric(0),
          n_genes = integer(0),
          protein_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = data.frame(time = format(Sys.time()), level = "INFO",
                         message = sprintf("GO-ORA: No enriched terms found (query: %d proteins)", n),
                         stringsAsFactors = FALSE)
      )
    ))
  }

  # Build results data.frame
  terms_df <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      term_id = r$term_id,
      term_name = r$term_name,
      ontology = r$ontology,
      pval = r$pval,
      fold_enrichment = r$fold_enrichment,
      n_genes = r$n_genes,
      n_term = r$n_term,
      protein_ids = r$protein_ids,
      stringsAsFactors = FALSE
    )
  }))

  # Compute FDR (BH adjustment)
  terms_df$fdr <- p.adjust(terms_df$pval, method = "BH")

  # Filter by FDR cutoff
  terms_df <- terms_df[terms_df$fdr <= fdr_cutoff, , drop = FALSE]

  # Filter by min_overlap (minimum query proteins overlapping with term)
  if (min_overlap > 1 && "n_genes" %in% names(terms_df) && nrow(terms_df) > 0) {
    before_count <- nrow(terms_df)
    terms_df <- terms_df[terms_df$n_genes >= min_overlap, , drop = FALSE]
    add_log("INFO", sprintf("Min overlap filter (n_genes >= %d): %d -> %d terms",
                            min_overlap, before_count, nrow(terms_df)))
  }

  # Sort by FDR, then fold_enrichment
  terms_df <- terms_df[order(terms_df$fdr, -terms_df$fold_enrichment), , drop = FALSE]

  # Limit to max_terms PER ONTOLOGY (not globally)
  # This ensures we get up to max_terms for BP, MF, and CC each
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
    # Re-sort after combining
    terms_df <- terms_df[order(terms_df$fdr, -terms_df$fold_enrichment), , drop = FALSE]
  } else if (nrow(terms_df) > max_terms) {
    # Fallback for data without ontology column
    terms_df <- terms_df[1:max_terms, , drop = FALSE]
  }

  # Final column order
  terms_df <- terms_df[, c("term_id", "term_name", "ontology", "fdr",
                           "fold_enrichment", "n_genes", "n_term", "protein_ids"), drop = FALSE]
  rownames(terms_df) <- NULL

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("GO-ORA completed in %.2f seconds: %d enriched terms (FDR <= %.3f)",
                          engine_duration,
                          nrow(terms_df),
                          fdr_cutoff))

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
    engine_id = "goora",
    params = params,
    data = list(
      terms = terms_df,
      query_info = list(
        n_query = n,
        n_background = N,
        direction = params$direction %||% "unknown"
      ),
      log = log_df
    )
  )
}

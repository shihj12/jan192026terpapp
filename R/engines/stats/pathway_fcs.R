# =========================================================
# R/engines/stats/pathway_fcs.R - 1D Pathway Functional Class Scoring Engine
#
# Performs rank-based functional class scoring on metabolite pathways
# (similar to 1D GO-FCS but for KEGG/Reactome pathways).
#
# Uses the Perseus method: Wilcoxon rank-sum test on ranked metabolites.
# Score = 2 * (mean_rank_in - mean_rank_out) / N, range [-1, 1]
#
# Contract:
#  - data$terms: pathway_id, pathway_name, pathway_type, fdr, score, n_metabolites, metabolite_ids
#  - Input is ranked metabolite scores (e.g., from volcano fold-change or PCA loadings)
# =========================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Execute 1D Pathway FCS engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context with metabobase mappings
#' @return Contract-compliant results: list(engine_id, params, data)
stats_pathway_fcs_run <- function(payload, params = NULL, context = NULL) {
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

  # Empty result helper
  empty_result <- function(error_msg = NULL, warn_msg = NULL) {
    log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))
    if (is.null(log_df) || nrow(log_df) == 0) {
      log_df <- data.frame(
        time = format(Sys.time()),
        level = if (!is.null(error_msg)) "ERROR" else "WARN",
        message = error_msg %||% warn_msg %||% "No results",
        stringsAsFactors = FALSE
      )
    }
    list(
      engine_id = "pathway_fcs",
      params = params,
      data = list(
        terms = data.frame(
          pathway_id = character(0),
          pathway_name = character(0),
          pathway_type = character(0),
          fdr = numeric(0),
          score = numeric(0),
          n_metabolites = integer(0),
          metabolite_ids = character(0),
          stringsAsFactors = FALSE
        ),
        log = log_df
      )
    )
  }

  if (!isTRUE(payload$ok)) {
    return(empty_result(error_msg = payload$error %||% "Invalid payload"))
  }

  add_log("INFO", "Initiating 1D Pathway Functional Class Scoring")

  # Get parameters
  pathway_db <- params$pathway_db %||% "all"
  fdr_cutoff <- params$fdr_cutoff %||% 0.03
  min_pathway_size <- params$min_pathway_size %||% 5
  min_overlap <- params$min_overlap %||% 3
  max_terms <- params$max_terms %||% 20

  add_log("INFO", sprintf("Parameters: FDR <= %.3f, min_size=%d, min_overlap=%d, pathway_db=%s",
                          fdr_cutoff, min_pathway_size, min_overlap, pathway_db))

  # Get metabolite scores
  # Priority: payload$scores > params$scores > context$scores
  scores <- payload$scores %||% params$scores %||% context$scores %||% NULL

  # If no scores, try to compute from fold-change vectors
  if (is.null(scores)) {
    fc_vectors <- payload$fc_vectors %||% context$fc_vectors %||% NULL
    if (!is.null(fc_vectors) && length(fc_vectors) > 0) {
      # Use first FC vector if multiple
      scores <- fc_vectors[[1]]
      add_log("INFO", sprintf("Using fold-change vector: %s", names(fc_vectors)[1]))
    }
  }

  if (is.null(scores) || length(scores) == 0) {
    add_log("ERROR", "No scores provided for pathway FCS")
    return(empty_result(error_msg = "No metabolite scores provided. Requires ranked scores from parent analysis."))
  }

  # Ensure scores is a named numeric vector
  if (!is.numeric(scores)) {
    add_log("ERROR", "Scores must be numeric")
    return(empty_result(error_msg = "Scores must be a named numeric vector"))
  }

  if (is.null(names(scores))) {
    add_log("ERROR", "Scores must have metabolite ID names")
    return(empty_result(error_msg = "Scores must be a named vector with metabolite IDs"))
  }

  # Remove NA scores
  scores <- scores[!is.na(scores)]
  n_scores <- length(scores)

  if (n_scores < 10) {
    add_log("ERROR", sprintf("Too few scored metabolites: %d", n_scores))
    return(empty_result(error_msg = sprintf("Too few scored metabolites (%d). Need at least 10.", n_scores)))
  }

  add_log("INFO", sprintf("Input: %d metabolites with scores", n_scores))

  # Get metabobase mappings
  metabobase <- payload$metabobase %||% context$metabobase

  if (is.null(metabobase)) {
    add_log("ERROR", "Pathway FCS requires metabobase with pathway annotations")
    return(empty_result(error_msg = "Pathway FCS requires metabobase with pathway annotations"))
  }

  # Extract pathway mappings from metabobase
  annot_long <- metabobase$annot_long
  terms_by_id <- metabobase$terms_by_id

  if (is.null(annot_long) || !is.data.frame(annot_long) || nrow(annot_long) == 0) {
    add_log("ERROR", "MetaboBase missing pathway annotations")
    return(empty_result(error_msg = "MetaboBase missing pathway annotations"))
  }

  # Filter by pathway database if specified
  if (pathway_db != "all") {
    annot_long <- annot_long[tolower(annot_long$pathway_type) == tolower(pathway_db), , drop = FALSE]
    if (!is.null(terms_by_id)) {
      terms_by_id <- terms_by_id[tolower(terms_by_id$pathway_type) == tolower(pathway_db), , drop = FALSE]
    }
    add_log("INFO", sprintf("Filtered to %s pathways", pathway_db))
  }

  # Build term-to-metabolite mapping
  term_metabolites <- split(annot_long$metabolite_id, annot_long$pathway_id)

  # Get pathway names
  pathway_names <- stats::setNames(rep(NA_character_, length(term_metabolites)), names(term_metabolites))
  pathway_types <- stats::setNames(rep(NA_character_, length(term_metabolites)), names(term_metabolites))

  if (!is.null(terms_by_id) && is.data.frame(terms_by_id)) {
    if ("pathway_name" %in% names(terms_by_id) && "pathway_id" %in% names(terms_by_id)) {
      idx <- match(names(term_metabolites), terms_by_id$pathway_id)
      pathway_names[!is.na(idx)] <- terms_by_id$pathway_name[idx[!is.na(idx)]]
    }
    if ("pathway_type" %in% names(terms_by_id) && "pathway_id" %in% names(terms_by_id)) {
      idx <- match(names(term_metabolites), terms_by_id$pathway_id)
      pathway_types[!is.na(idx)] <- terms_by_id$pathway_type[idx[!is.na(idx)]]
    }
  }

  # Also try to get pathway type from annot_long
  for (pw_id in names(term_metabolites)) {
    if (is.na(pathway_types[pw_id])) {
      pt <- annot_long$pathway_type[annot_long$pathway_id == pw_id][1]
      if (!is.na(pt)) pathway_types[pw_id] <- pt
    }
    if (is.na(pathway_names[pw_id])) {
      pn <- annot_long$pathway_name[annot_long$pathway_id == pw_id][1]
      if (!is.na(pn)) pathway_names[pw_id] <- pn
    }
  }

  # Compute ranks (Perseus method: rank by score descending, ties averaged)
  score_order <- order(scores, decreasing = TRUE)
  ranks <- numeric(length(scores))
  ranks[score_order] <- seq_along(scores)

  # Use average ranks for ties
  for (s in unique(scores)) {
    tie_idx <- which(scores == s)
    if (length(tie_idx) > 1) {
      ranks[tie_idx] <- mean(ranks[tie_idx])
    }
  }

  names(ranks) <- names(scores)
  N <- length(ranks)

  # Perform Wilcoxon rank-sum test for each pathway
  results <- list()

  for (pathway_id in names(term_metabolites)) {
    pathway_mets <- unique(term_metabolites[[pathway_id]])

    # Filter to metabolites with scores
    mets_in <- intersect(pathway_mets, names(ranks))
    n_in <- length(mets_in)

    # Skip small pathways or insufficient overlap
    if (n_in < min_overlap) next
    if (length(pathway_mets) < min_pathway_size) next

    # Get ranks for metabolites in vs out of pathway
    ranks_in <- ranks[mets_in]
    ranks_out <- ranks[setdiff(names(ranks), mets_in)]
    n_out <- length(ranks_out)

    if (n_out == 0) next

    # Wilcoxon rank-sum test
    wt <- tryCatch(
      stats::wilcox.test(ranks_in, ranks_out, alternative = "two.sided", exact = FALSE),
      error = function(e) NULL
    )

    if (is.null(wt)) next

    pval <- wt$p.value

    # Calculate FCS score: 2 * (mean_rank_in - mean_rank_out) / N
    # Range: [-1, 1], positive = enriched at top of ranked list
    mean_rank_in <- mean(ranks_in)
    mean_rank_out <- mean(ranks_out)
    fcs_score <- 2 * (mean_rank_out - mean_rank_in) / N  # Inverted: higher score = enriched at top

    results <- c(results, list(data.frame(
      pathway_id = pathway_id,
      pathway_name = pathway_names[pathway_id] %||% pathway_id,
      pathway_type = pathway_types[pathway_id] %||% "unknown",
      pval = pval,
      score = fcs_score,
      n_metabolites = n_in,
      pathway_size = length(pathway_mets),
      metabolite_ids = paste(mets_in, collapse = ";"),
      stringsAsFactors = FALSE
    )))
  }

  if (length(results) == 0) {
    add_log("WARN", "No pathways passed filtering criteria")
    return(empty_result(warn_msg = "No pathways passed filtering criteria"))
  }

  # Combine results
  results_df <- do.call(rbind, results)

  # FDR correction
  results_df$fdr <- stats::p.adjust(results_df$pval, method = "BH")

  # Filter by FDR
  results_df <- results_df[results_df$fdr <= fdr_cutoff, , drop = FALSE]

  if (nrow(results_df) == 0) {
    add_log("INFO", sprintf("No pathways significant at FDR <= %.3f", fdr_cutoff))
    return(empty_result(warn_msg = sprintf("No pathways significant at FDR <= %.3f", fdr_cutoff)))
  }

  # Sort by absolute score (most enriched/depleted first), then by FDR
  results_df <- results_df[order(-abs(results_df$score), results_df$fdr), , drop = FALSE]

  # Limit results per pathway type if "all" databases selected
  if (pathway_db == "all") {
    final_results <- list()
    for (pt in unique(results_df$pathway_type)) {
      pt_df <- results_df[results_df$pathway_type == pt, , drop = FALSE]
      pt_df <- head(pt_df, max_terms)
      final_results <- c(final_results, list(pt_df))
    }
    results_df <- do.call(rbind, final_results)
  } else {
    results_df <- head(results_df, max_terms)
  }

  add_log("INFO", sprintf("Pathway FCS complete: %d significant pathways found", nrow(results_df)))

  # Build output
  terms_out <- data.frame(
    pathway_id = results_df$pathway_id,
    pathway_name = results_df$pathway_name,
    pathway_type = results_df$pathway_type,
    fdr = results_df$fdr,
    score = results_df$score,
    n_metabolites = results_df$n_metabolites,
    metabolite_ids = results_df$metabolite_ids,
    stringsAsFactors = FALSE
  )

  # Build log dataframe
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  # Record runtime
  runtime_sec <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Pathway FCS engine completed in %.2f seconds", runtime_sec))
  log_df <- do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE))

  list(
    engine_id = "pathway_fcs",
    params = params,
    data = list(
      terms = terms_out,
      n_scored = n_scores,
      log = log_df
    )
  )
}

# =========================================================
# R/engines/stats/rankplot.R - Rank Plot Engine
#
# Computes ranked values for scatter plot visualization.
# Lowest value = rank 1 (ascending order).
#
# Contract:
#  - data$points: data.frame with protein_id, gene_id, group, value, rank
#  - data$groups: unique group names
#  - data$id_cols: list(protein, gene) column names
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute rankplot engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters (value_transform)
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
stats_rankplot_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()

  if (!isTRUE(payload$ok)) {
    return(list(
      engine_id = "rankplot",
      params = params,
      data = list(
        points = data.frame(),
        groups = character(0),
        error = payload$error %||% "Invalid payload"
      )
    ))
  }

  mat <- payload$mat
  samples <- payload$samples
  ids <- payload$ids
  groups <- payload$groups

  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    return(list(
      engine_id = "rankplot",
      params = params,
      data = list(
        points = data.frame(),
        groups = character(0),
        error = "Payload matrix is missing or empty"
      )
    ))
  }

  # Get ID columns from metadata
  protein_col <- as.character(payload$metadata$id_protein_col %||% "")[1]
  gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]

  # Extract protein IDs
  protein_ids <- if (nzchar(protein_col) && protein_col %in% names(ids)) {
    as.character(ids[[protein_col]])
  } else {
    rownames(mat)
  }
  if (is.null(protein_ids)) {
    protein_ids <- paste0("row_", seq_len(nrow(mat)))
  }

  # Extract gene IDs
  gene_ids <- if (nzchar(gene_col) && gene_col %in% names(ids)) {
    as.character(ids[[gene_col]])
  } else {
    rep(NA_character_, nrow(mat))
  }

  # Get transform parameter
  value_transform <- params$value_transform %||% "none"
  if (!value_transform %in% c("none", "log2", "log10")) {
    value_transform <- "none"
  }

  # Handle case where no groups are defined
  if (is.null(groups) || length(groups) == 0) {
    groups <- "All"
  }

  # Build results per group (average across replicates)
  results_list <- list()
  for (grp in groups) {
    # Find columns belonging to this group
    if (grp == "All" && !"group_name" %in% names(samples)) {
      cols_g <- colnames(mat)
    } else {
      cols_g <- samples$sample_col[samples$group_name == grp]
      cols_g <- intersect(cols_g, colnames(mat))
    }

    if (length(cols_g) == 0) next

    # Compute mean value and replicate count per protein across replicates
    submat <- mat[, cols_g, drop = FALSE]
    mean_vals <- if (length(cols_g) == 1) {
      submat[, 1]
    } else {
      rowMeans(submat, na.rm = TRUE)
    }
    # Count non-NA replicates per protein
    n_reps <- if (length(cols_g) == 1) {
      as.integer(!is.na(submat[, 1]))
    } else {
      rowSums(!is.na(submat))
    }

    results_list[[grp]] <- data.frame(
      protein_id = protein_ids,
      gene_id = gene_ids,
      group = grp,
      value_raw = mean_vals,
      n_reps = n_reps,
      stringsAsFactors = FALSE
    )
  }

  if (length(results_list) == 0) {
    return(list(
      engine_id = "rankplot",
      params = params,
      data = list(
        points = data.frame(),
        groups = character(0),
        error = "No valid groups found"
      )
    ))
  }

  results <- do.call(rbind, results_list)

  # Apply transform
  results$value <- switch(
    value_transform,
    "log2" = {
      vals <- results$value_raw
      vals[vals <= 0] <- NA_real_
      log2(vals)
    },
    "log10" = {
      vals <- results$value_raw
      vals[vals <= 0] <- NA_real_
      log10(vals)
    },
    results$value_raw
  )

  # Remove rows with invalid values
  valid_idx <- is.finite(results$value)
  results <- results[valid_idx, , drop = FALSE]

  if (nrow(results) == 0) {
    return(list(
      engine_id = "rankplot",
      params = params,
      data = list(
        points = data.frame(),
        groups = unique(groups),
        error = "All values became invalid after transform"
      )
    ))
  }

  # Compute ranks per group (ascending - lowest value = rank 1)
  results <- do.call(rbind, lapply(split(results, results$group), function(df) {
    df$rank <- rank(df$value, ties.method = "average", na.last = "keep")
    df
  }))
  rownames(results) <- NULL

  list(
    engine_id = "rankplot",
    params = params,
    data = list(
      points = results,
      groups = unique(results$group),
      id_cols = list(protein = protein_col, gene = gene_col)
    )
  )
}

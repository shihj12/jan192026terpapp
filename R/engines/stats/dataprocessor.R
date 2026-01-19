# =========================================================
# R/engines/stats/dataprocessor.R — Data Processor Engine
#
# Preprocessing utilities including:
#  - Contaminant tagging and removal
#  - Filtering operations (threshold, prefix, keyword, non-numeric)
#  - Row aggregation/averaging by identifier
#  - Imputation
#
# Contract v1.1: Must output data$log and data$summary
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Execute dataprocessor engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters (overrides payload$params)
#' @param context Execution context with contaminants list
#' @return Contract-compliant results: list(engine_id, params, data)
#'   - data$log: data.frame with time, level, message
#'   - data$summary: data.frame with metric, value
stats_dataprocessor_run <- function(payload, params = NULL, context = NULL) {
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

  # Validate payload
  if (!isTRUE(payload$ok)) {
    add_log("ERROR", payload$error %||% "Invalid payload")
    return(list(
      engine_id = "dataprocessor",
      params = params,
      data = list(
        log = do.call(rbind, lapply(log_entries, as.data.frame, stringsAsFactors = FALSE)),
        summary = data.frame(metric = "error", value = 1, stringsAsFactors = FALSE)
      )
    ))
  }

  mat <- payload$mat
  ids <- payload$ids

  add_log("INFO", sprintf("Data loaded: %d rows x %d columns", nrow(mat), ncol(mat)))
  add_log("INFO", sprintf("Groups: %s", paste(payload$groups, collapse = ", ")))
  add_log("INFO", sprintf("Analysis level: %s", payload$metadata$analysis_level))

  # Initialize summary metrics
  summary_metrics <- list(
    initial_rows = nrow(mat),
    initial_cols = ncol(mat),
    groups = payload$n_groups,
    cells_before_non_na = sum(!is.na(mat))
  )

  # =========================================================
  # Parse plan_json and execute substeps
  # =========================================================
  plan <- NULL
  plan_json <- params$plan_json %||% NULL

  if (!is.null(plan_json) && nzchar(plan_json)) {
    plan <- tryCatch({
      jsonlite::fromJSON(plan_json, simplifyVector = FALSE)
    }, error = function(e) {
      add_log("WARN", sprintf("Failed to parse plan_json: %s", conditionMessage(e)))
      NULL
    })
  }

  substeps <- plan$substeps %||% list()

  if (length(substeps) == 0) {
    add_log("INFO", "No substeps defined in plan_json, skipping processing")
  } else {
    add_log("INFO", sprintf("Executing %d substep(s) from plan_json", length(substeps)))
  }

  # Helper to resolve ID column name
  resolve_id_col <- function(id_col_key) {
    if (id_col_key == "protein") {
      return(as.character(payload$metadata$id_protein_col %||% "ProteinID")[1])
    } else if (id_col_key == "gene") {
      return(as.character(payload$metadata$id_gene_col %||% "GeneID")[1])
    } else if (id_col_key == "peptide") {
      # For peptide-level aggregation (inside Peptide Analysis), use primary ID column
      return(as.character(payload$metadata$id_primary_col %||% payload$metadata$id_peptide_col %||% "PeptideID")[1])
    }
    id_col_key
  }

  # Get contaminants from context for tag_remove_contaminants

  contaminant_ids <- context$contaminants %||% character(0)

  # Track contaminant stats
  contaminants_loaded <- 0
  contaminants_found <- 0
  contaminants_removed <- 0

  # Execute each substep
  substep_summary <- list()
  substep_states <- list(list(
    Step = 0L,
    Operation = "Start",
    mat = mat,
    ids = ids
  ))

  for (i in seq_along(substeps)) {
    ss <- substeps[[i]]
    op <- ss$op %||% ""
    opts <- ss$opts %||% list()

    add_log("INFO", sprintf("--- Substep %d: %s ---", i, op))
    mat_before <- mat
    rows_before <- nrow(mat_before)
    cols_before <- ncol(mat_before)

    # =====================================================
    # filter_threshold: Set cells below/above threshold to NA
    # =====================================================
    if (op == "filter_threshold") {
      threshold <- opts$threshold %||% 0
      direction <- opts$direction %||% "below"

      # Count cells before
      n_valid_before <- sum(!is.na(mat))

      # Use which() to avoid NA issues in logical comparisons
      if (direction == "below") {
        mat[which(mat < threshold)] <- NA
        add_log("INFO", sprintf("Filtered cells below %s to NA", threshold))
      } else {
        mat[which(mat > threshold)] <- NA
        add_log("INFO", sprintf("Filtered cells above %s to NA", threshold))
      }

      n_valid_after <- sum(!is.na(mat))
      add_log("INFO", sprintf("Cells affected: %d", n_valid_before - n_valid_after))
    }

    # =====================================================
    # filter_prefix: Remove rows where ID starts with prefix
    # =====================================================
    else if (op == "filter_prefix") {
      id_col <- resolve_id_col(opts$id_col %||% "protein")
      prefix <- opts$prefix %||% ""

      if (nzchar(prefix) && id_col %in% names(ids)) {
        id_vals <- as.character(ids[[id_col]])
        # Handle NA values - treat NA as non-match
        id_vals[is.na(id_vals)] <- ""
        matches <- startsWith(id_vals, prefix)
        n_match <- sum(matches, na.rm = TRUE)

        if (n_match > 0) {
          keep_idx <- which(!matches)
          mat <- mat[keep_idx, , drop = FALSE]
          ids <- ids[keep_idx, , drop = FALSE]
          add_log("INFO", sprintf("Removed %d rows with prefix '%s' in column '%s'", n_match, prefix, id_col))
        } else {
          add_log("INFO", sprintf("No rows found with prefix '%s'", prefix))
        }
      } else {
        add_log("WARN", sprintf("Column '%s' not found or empty prefix", id_col))
      }
    }

    # =====================================================
    # filter_keyword: Remove rows containing keyword
    # =====================================================
    else if (op == "filter_keyword") {
      id_col <- resolve_id_col(opts$id_col %||% "protein")
      keyword <- opts$keyword %||% ""

      if (nzchar(keyword) && id_col %in% names(ids)) {
        id_vals <- as.character(ids[[id_col]])
        # Handle NA values - treat NA as non-match
        id_vals[is.na(id_vals)] <- ""
        matches <- grepl(keyword, id_vals, fixed = TRUE, ignore.case = TRUE)
        n_match <- sum(matches, na.rm = TRUE)

        if (n_match > 0) {
          keep_idx <- which(!matches)
          mat <- mat[keep_idx, , drop = FALSE]
          ids <- ids[keep_idx, , drop = FALSE]
          add_log("INFO", sprintf("Removed %d rows containing keyword '%s' in column '%s'", n_match, keyword, id_col))
        } else {
          add_log("INFO", sprintf("No rows found containing keyword '%s'", keyword))
        }
      } else {
        add_log("WARN", sprintf("Column '%s' not found or empty keyword", id_col))
      }
    }

    # =====================================================
    # filter_non_numeric: Set non-numeric cells to NA
    # =====================================================
    else if (op == "filter_non_numeric") {
      n_valid_before <- sum(!is.na(mat))

      # mat should already be numeric, but ensure
      if (!is.numeric(mat)) {
        mat <- apply(mat, 2, function(col) {
          suppressWarnings(as.numeric(col))
        })
      }

      # Check for infinite values and set to NA
      mat[!is.finite(mat)] <- NA

      n_valid_after <- sum(!is.na(mat))
      add_log("INFO", sprintf("Non-numeric/infinite cells set to NA: %d", n_valid_before - n_valid_after))
    }

    # =====================================================
    # aggregate_rows: Sum rows by identifier (for precursor→protein)
    # =====================================================
    else if (op == "aggregate_rows") {
      id_col <- resolve_id_col(opts$id_col %||% "protein")

      if (id_col %in% names(ids)) {
        id_vals <- as.character(ids[[id_col]])
        # Replace NA with placeholder to avoid matching issues
        id_vals[is.na(id_vals)] <- "__NA__"
        unique_ids <- unique(id_vals)
        n_before <- nrow(mat)

        # Aggregate by summing
        agg_mat <- matrix(NA_real_, nrow = length(unique_ids), ncol = ncol(mat))
        colnames(agg_mat) <- colnames(mat)
        agg_ids <- data.frame(matrix(NA, nrow = length(unique_ids), ncol = ncol(ids)))
        names(agg_ids) <- names(ids)

        for (j in seq_along(unique_ids)) {
          uid <- unique_ids[j]
          rows <- which(id_vals == uid)

          if (length(rows) == 1) {
            agg_mat[j, ] <- mat[rows, ]
            agg_ids[j, ] <- ids[rows, ]
          } else {
            # Sum numeric values, handling NA
            # If all values in a column are NA, keep NA (don't convert to 0)
            row_mat <- mat[rows, , drop = FALSE]
            agg_mat[j, ] <- vapply(seq_len(ncol(row_mat)), function(col_idx) {
              col_vals <- row_mat[, col_idx]
              if (all(is.na(col_vals))) NA_real_ else sum(col_vals, na.rm = TRUE)
            }, numeric(1))
            # For IDs, take first row
            agg_ids[j, ] <- ids[rows[1], ]
          }
        }

        mat <- agg_mat
        ids <- agg_ids

        add_log("INFO", sprintf("Aggregated (sum) from %d to %d rows by column '%s'", n_before, nrow(mat), id_col))
      } else {
        add_log("WARN", sprintf("Column '%s' not found for aggregation", id_col))
      }
    }

    # =====================================================
    # average_rows: Average rows by identifier
    # =====================================================
    else if (op == "average_rows") {
      id_col <- resolve_id_col(opts$id_col %||% "protein")
      mean_type <- opts$mean_type %||% "arithmetic"

      if (id_col %in% names(ids)) {
        id_vals <- as.character(ids[[id_col]])
        # Replace NA with placeholder to avoid matching issues
        id_vals[is.na(id_vals)] <- "__NA__"
        unique_ids <- unique(id_vals)
        n_before <- nrow(mat)

        agg_mat <- matrix(NA_real_, nrow = length(unique_ids), ncol = ncol(mat))
        colnames(agg_mat) <- colnames(mat)
        agg_ids <- data.frame(matrix(NA, nrow = length(unique_ids), ncol = ncol(ids)))
        names(agg_ids) <- names(ids)

        for (j in seq_along(unique_ids)) {
          uid <- unique_ids[j]
          rows <- which(id_vals == uid)

          if (length(rows) == 1) {
            agg_mat[j, ] <- mat[rows, ]
            agg_ids[j, ] <- ids[rows, ]
          } else {
            if (mean_type == "harmonic") {
              # Harmonic mean: n / sum(1/x)
              agg_mat[j, ] <- apply(mat[rows, , drop = FALSE], 2, function(x) {
                x <- x[!is.na(x) & x > 0]
                if (length(x) == 0) return(NA_real_)
                length(x) / sum(1/x)
              })
            } else {
              # Arithmetic mean - if all values in a column are NA, keep NA (not NaN)
              row_mat <- mat[rows, , drop = FALSE]
              agg_mat[j, ] <- vapply(seq_len(ncol(row_mat)), function(col_idx) {
                col_vals <- row_mat[, col_idx]
                if (all(is.na(col_vals))) NA_real_ else mean(col_vals, na.rm = TRUE)
              }, numeric(1))
            }
            agg_ids[j, ] <- ids[rows[1], ]
          }
        }

        mat <- agg_mat
        ids <- agg_ids

        add_log("INFO", sprintf("Averaged (%s) from %d to %d rows by column '%s'", mean_type, n_before, nrow(mat), id_col))
      } else {
        add_log("WARN", sprintf("Column '%s' not found for averaging", id_col))
      }
    }

    # =====================================================
    # tag_remove_contaminants: Remove contaminant proteins
    # =====================================================
    else if (op == "tag_remove_contaminants") {
      if (length(contaminant_ids) > 0) {
        contaminants_loaded <- length(contaminant_ids)
        add_log("INFO", sprintf("Contaminants list: %d IDs loaded", contaminants_loaded))

        protein_col <- resolve_id_col("protein")

        if (protein_col %in% names(ids)) {
          protein_ids <- as.character(ids[[protein_col]])

          # Find matches (case-insensitive, trimmed)
          protein_ids_clean <- trimws(tolower(protein_ids))
          contaminant_ids_clean <- trimws(tolower(contaminant_ids))

          is_contaminant <- protein_ids_clean %in% contaminant_ids_clean
          contaminants_found <- sum(is_contaminant)

          if (contaminants_found > 0) {
            add_log("INFO", sprintf("Contaminants found in data: %d", contaminants_found))

            keep_idx <- which(!is_contaminant)
            mat <- mat[keep_idx, , drop = FALSE]
            ids <- ids[keep_idx, , drop = FALSE]
            contaminants_removed <- contaminants_found

            add_log("INFO", sprintf("Contaminants removed: %d", contaminants_removed))
          } else {
            add_log("INFO", "No contaminants found in data")
          }
        } else {
          add_log("WARN", sprintf("Protein ID column '%s' not found; contaminant removal skipped", protein_col))
        }
      } else {
        add_log("INFO", "No contaminants list provided in context")
      }
    }

    # =====================================================
    # impute: Fill NA values
    # =====================================================
    else if (op == "impute") {
      method <- opts$method %||% "flat"
      n_na_before <- sum(is.na(mat))

      if (method == "flat") {
        flat_value <- opts$flat_value %||% 10
        mat[is.na(mat)] <- flat_value
        add_log("INFO", sprintf("Imputed %d NA values with flat value %s", n_na_before, flat_value))
      } else if (method == "min_mult") {
        multiplier <- opts$multiplier %||% 0.1
        min_val <- min(mat, na.rm = TRUE)
        if (is.finite(min_val) && min_val > 0) {
          impute_val <- min_val * multiplier
          mat[is.na(mat)] <- impute_val
          add_log("INFO", sprintf("Imputed %d NA values with min * %.2f = %.4f", n_na_before, multiplier, impute_val))
        } else {
          add_log("WARN", "Cannot compute min value for imputation (no positive values)")
        }
      } else {
        add_log("WARN", sprintf("Unknown imputation method: %s", method))
      }
    }

    # =====================================================
    # Unknown operation
    # =====================================================
    else {
      add_log("WARN", sprintf("Unknown substep operation: %s", op))
    }

    cells_before_non_na <- sum(!is.na(mat_before))
    cells_after_non_na <- sum(!is.na(mat))
    cells_changed_step <- as.integer(abs(cells_after_non_na - cells_before_non_na))
    add_log("INFO", sprintf(
      "Cells before: %d, Cells after: %d, Changed: %d",
      cells_before_non_na, cells_after_non_na, cells_changed_step
    ))

    substep_summary[[length(substep_summary) + 1]] <- data.frame(
      Step = as.integer(i),
      Operation = as.character(op),
      rows_before = as.integer(rows_before),
      cols_before = as.integer(cols_before),
      rows_after = as.integer(nrow(mat)),
      cols_after = as.integer(ncol(mat)),
      cells_before_non_na = as.integer(cells_before_non_na),
      cells_after_non_na = as.integer(cells_after_non_na),
      cells_changed = as.integer(cells_changed_step),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    substep_states[[length(substep_states) + 1]] <- list(
      Step = as.integer(i),
      Operation = as.character(op),
      mat = mat,
      ids = ids
    )

    # Log current state after each substep
    add_log("INFO", sprintf("After substep %d: %d rows x %d columns", i, nrow(mat), ncol(mat)))
  }

  # Silent zero-to-NA replacement: replace exact zeros with NA (not logged)
  mat[mat == 0] <- NA_real_

  # Update summary metrics
  summary_metrics$contaminants_loaded <- contaminants_loaded
  summary_metrics$contaminants_found <- contaminants_found
  summary_metrics$contaminants_removed <- contaminants_removed
  summary_metrics$final_rows <- nrow(mat)
  summary_metrics$final_cols <- ncol(mat)
  summary_metrics$cells_after_non_na <- sum(!is.na(mat))
  summary_metrics$cells_changed <- abs(summary_metrics$cells_after_non_na - summary_metrics$cells_before_non_na)

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Dataprocessor completed in %.2f seconds", engine_duration))

  # Build log data.frame
  log_df <- do.call(rbind, lapply(log_entries, function(e) {
    data.frame(
      time = e$time,
      level = e$level,
      message = e$message,
      stringsAsFactors = FALSE
    )
  }))

  # Build summary data.frame
  summary_df <- data.frame(
    metric = names(summary_metrics),
    value = as.numeric(unlist(summary_metrics)),
    stringsAsFactors = FALSE
  )

  substep_summary_df <- if (length(substep_summary) > 0) {
    do.call(rbind, substep_summary)
  } else {
    data.frame(
      Step = integer(0),
      Operation = character(0),
      rows_before = integer(0),
      cols_before = integer(0),
      rows_after = integer(0),
      cols_after = integer(0),
      cells_before_non_na = integer(0),
      cells_after_non_na = integer(0),
      cells_changed = integer(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Contract-compliant results.rds structure
  list(
    engine_id = "dataprocessor",
    params = params,
    data = list(
      log = log_df,
      summary = summary_df,
      substep_summary = substep_summary_df,
      substep_states = substep_states,
      # Also store processed data for downstream engines
      mat = mat,
      ids = ids,
      # Store design info for re-runnable Excel export
      metadata = payload$metadata,
      samples = payload$samples,
      groups = payload$groups
    )
  )
}

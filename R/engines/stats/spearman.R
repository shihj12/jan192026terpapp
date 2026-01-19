# =========================================================
# R/engines/stats/spearman.R — Spearman Correlation Engine
#
# Computes pairwise Spearman correlations between samples
# with precomputed line options and equations for viewer.
#
# Contract v1.1: No p-value, no R2; precompute line variants
# =========================================================

#' Execute spearman engine
#'
#' @param payload Payload from nr_build_step_payload
#' @param params Engine-specific parameters
#' @param context Execution context
#' @return Contract-compliant results: list(engine_id, params, data)
#'   - data$points: data.frame with x, y, pair_id
#'   - data$correlations: data.frame with pair_id, rho
#'   - data$precomputed: list with line coefficients and equations
stats_spearman_run <- function(payload, params = NULL, context = NULL) {
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
      engine_id = "spearman",
      params = params,
      data = list(
        points = data.frame(x = numeric(0), y = numeric(0), pair_id = character(0),
                            stringsAsFactors = FALSE),
        correlations = data.frame(pair_id = character(0), rho = numeric(0),
                                  stringsAsFactors = FALSE),
        log = data.frame(time = format(Sys.time()), level = "ERROR",
                         message = payload$error %||% "Invalid payload",
                         stringsAsFactors = FALSE)
      )
    ))
  }

  mat <- payload$mat
  samples <- payload$samples
  groups <- payload$groups

  add_log("INFO", sprintf("Initiating spearman: %d proteins x %d samples",
                          nrow(mat), ncol(mat)))

  # Get parameters
  compare_mode <- params$compare_mode %||% "avg_groups"
  log_transform <- params$log_transform %||% "log10"

  # Apply log transform
  if (log_transform == "log10") {
    mat[mat <= 0] <- NA
    mat <- log10(mat)
    add_log("INFO", "Applied log10 transformation")
  } else if (log_transform == "log2") {
    mat[mat <= 0] <- NA
    mat <- log2(mat)
    add_log("INFO", "Applied log2 transformation")
  }

  add_log("INFO", sprintf("Compare mode: %s", compare_mode))

  # Get control group info from metadata
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

  if (has_control) {
    add_log("INFO", sprintf("Control group identified: %s", groups[control_idx]))
  }
  if (control_only && has_control) {
    add_log("INFO", "Control-only mode: generating comparisons against control only")
  }

  # Build comparison pairs based on mode
  pairs <- list()

  if (compare_mode == "avg_groups" && length(groups) >= 2) {
    # Average within groups, then compare groups
    group_means <- list()
    for (grp in groups) {
      grp_cols <- samples$sample_col[samples$group_name == grp]
      grp_cols <- intersect(grp_cols, colnames(mat))
      if (length(grp_cols) > 0) {
        group_means[[grp]] <- rowMeans(mat[, grp_cols, drop = FALSE], na.rm = TRUE)
      }
    }

    # Pairwise group comparisons
    for (i in seq_len(length(groups) - 1)) {
      for (j in (i + 1):length(groups)) {
        # Skip non-control comparisons if control_only is enabled
        if (control_only && has_control) {
          if (!i %in% control_idx && !j %in% control_idx) {
            next
          }
        }

        g1 <- groups[i]
        g2 <- groups[j]

        # Control orientation: control should always be on x-axis
        if (has_control) {
          if (j %in% control_idx) {
            # Swap so control is g1 (x-axis)
            tmp <- g1; g1 <- g2; g2 <- tmp
          }
        }

        if (!is.null(group_means[[g1]]) && !is.null(group_means[[g2]])) {
          pairs <- c(pairs, list(list(
            x = group_means[[g1]],
            y = group_means[[g2]],
            label_x = g1,
            label_y = g2,
            pair_id = sprintf("%s_vs_%s", g2, g1)
          )))
        }
      }
    }
  } else if (compare_mode == "within_groups") {
    # Compare replicates within each group
    for (grp in groups) {
      grp_cols <- samples$sample_col[samples$group_name == grp]
      grp_cols <- intersect(grp_cols, colnames(mat))
      if (length(grp_cols) >= 2) {
        for (i in seq_len(length(grp_cols) - 1)) {
          for (j in (i + 1):length(grp_cols)) {
            pairs <- c(pairs, list(list(
              x = mat[, grp_cols[i]],
              y = mat[, grp_cols[j]],
              label_x = grp_cols[i],
              label_y = grp_cols[j],
              pair_id = sprintf("%s_%s_vs_%s", grp, grp_cols[i], grp_cols[j])
            )))
          }
        }
      }
    }
  } else {
    # Default: all pairwise sample comparisons
    all_cols <- colnames(mat)
    if (length(all_cols) >= 2) {
      for (i in seq_len(length(all_cols) - 1)) {
        for (j in (i + 1):length(all_cols)) {
          pairs <- c(pairs, list(list(
            x = mat[, all_cols[i]],
            y = mat[, all_cols[j]],
            label_x = all_cols[i],
            label_y = all_cols[j],
            pair_id = sprintf("%s_vs_%s", all_cols[i], all_cols[j])
          )))
        }
      }
    }
  }

  add_log("INFO", sprintf("Built %d comparison pairs", length(pairs)))

  # Compute correlations and build output
  points_list <- list()
  corr_list <- list()
  precomputed <- list()

  for (pair in pairs) {
    x <- pair$x
    y <- pair$y
    pair_id <- pair$pair_id

    # Remove NA pairs
    valid <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
    x_clean <- x[valid]
    y_clean <- y[valid]

    if (length(x_clean) < 3) next

    # Compute Spearman rho
    rho <- cor(x_clean, y_clean, method = "spearman")

    # Compute linear regression for line
    fit <- lm(y_clean ~ x_clean)
    intercept <- coef(fit)[1]
    slope <- coef(fit)[2]

    # Build equation string
    equation <- sprintf("y = %.3fx %s %.3f",
                        slope,
                        if (intercept >= 0) "+" else "-",
                        abs(intercept))

    # Store points
    points_list <- c(points_list, list(
      data.frame(
        x = x_clean,
        y = y_clean,
        pair_id = pair_id,
        stringsAsFactors = FALSE
      )
    ))

    # Store correlation
    corr_list <- c(corr_list, list(
      data.frame(
        pair_id = pair_id,
        label_x = pair$label_x,
        label_y = pair$label_y,
        rho = rho,
        n = length(x_clean),
        stringsAsFactors = FALSE
      )
    ))

    # Store precomputed line info
    precomputed[[pair_id]] <- list(
      intercept = intercept,
      slope = slope,
      equation = equation,
      rho = rho
    )
  }

  # Combine results
  points_df <- if (length(points_list) > 0) {
    do.call(rbind, points_list)
  } else {
    data.frame(x = numeric(0), y = numeric(0), pair_id = character(0),
               stringsAsFactors = FALSE)
  }

  corr_df <- if (length(corr_list) > 0) {
    do.call(rbind, corr_list)
  } else {
    data.frame(pair_id = character(0), rho = numeric(0), stringsAsFactors = FALSE)
  }

  # Log engine runtime
  engine_duration <- as.numeric(difftime(Sys.time(), engine_start, units = "secs"))
  add_log("INFO", sprintf("Spearman completed in %.2f seconds (%d pairs)",
                          engine_duration, length(pairs)))

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
    engine_id = "spearman",
    params = params,
    data = list(
      points = points_df,
      correlations = corr_df,
      precomputed = precomputed,
      log = log_df
    )
  )
}

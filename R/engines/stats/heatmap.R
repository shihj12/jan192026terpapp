# =========================================================
# R/engines/stats/heatmap.R — Heatmap Engine
#
# Computes log-scale and z-score matrices for a user-provided
# gene list, with optional row clustering and group annotations.
# =========================================================

stats_heatmap_run <- function(payload, params = NULL, context = NULL) {
  params <- params %||% payload$params %||% list()

  if (!isTRUE(payload$ok)) {
    stop(payload$error %||% "Invalid payload")
  }

  mat <- payload$mat
  samples <- payload$samples
  ids <- payload$ids

  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    stop("Heatmap: payload matrix is missing or empty")
  }
  if (is.null(samples) || !is.data.frame(samples) || nrow(samples) == 0) {
    stop("Heatmap: payload samples are missing or empty")
  }
  if (is.null(ids) || !is.data.frame(ids) || nrow(ids) == 0) {
    stop("Heatmap: payload ids are missing or empty")
  }

  parse_gene_list <- function(x) {
    x <- as.character(x %||% "")[1]
    x <- trimws(x)
    if (!nzchar(x)) {
      stop("Heatmap: gene list is empty (paste one gene per line)")
    }
    genes <- unlist(strsplit(x, "\\r?\\n", perl = TRUE), use.names = FALSE)
    genes <- trimws(genes)
    genes <- genes[nzchar(genes)]
    genes <- genes[!duplicated(genes)]
    if (length(genes) == 0) {
      stop("Heatmap: gene list is empty (paste one gene per line)")
    }
    if (length(genes) > 2000) {
      stop(sprintf("Heatmap: gene list too large (%d). Maximum is 2000 genes.", length(genes)))
    }
    genes
  }

  gene_list <- parse_gene_list(params$gene_list)

  id_gene_col <- as.character(payload$metadata$id_gene_col %||% "")[1]
  if (!nzchar(id_gene_col) || !id_gene_col %in% names(ids)) {
    stop("Heatmap: cannot match genes (payload$metadata$id_gene_col is missing or not in payload$ids)")
  }

  gene_ref <- as.character(ids[[id_gene_col]])
  row_idx <- match(gene_list, gene_ref)

  matched_genes <- gene_list[!is.na(row_idx)]
  unmatched_genes <- gene_list[is.na(row_idx)]

  if (length(matched_genes) == 0) {
    stop(sprintf(
      "Heatmap: none of the provided genes matched (n=%d).",
      length(gene_list)
    ))
  }

  mat_sub <- mat[row_idx[!is.na(row_idx)], , drop = FALSE]
  rownames(mat_sub) <- matched_genes

  sample_col <- as.character(payload$metadata$sample_col %||% "sample_col")[1]
  if (!nzchar(sample_col) || !sample_col %in% names(samples)) {
    sample_col <- "sample_col"
  }
  if (!sample_col %in% names(samples)) {
    stop("Heatmap: payload$samples must include `sample_col`")
  }
  if (!"group_name" %in% names(samples)) {
    stop("Heatmap: payload$samples must include `group_name`")
  }
  if (!"replicate" %in% names(samples)) {
    stop("Heatmap: payload$samples must include `replicate`")
  }

  rep_num <- suppressWarnings(as.numeric(samples$replicate))
  if (all(!is.finite(rep_num))) rep_num <- seq_len(nrow(samples))
  ord <- order(as.character(samples$group_name), rep_num, na.last = TRUE)
  sample_order <- as.character(samples[[sample_col]][ord])
  sample_order <- sample_order[sample_order %in% colnames(mat_sub)]
  if (length(sample_order) == 0) {
    stop("Heatmap: no sample columns matched between payload$mat and payload$samples")
  }
  mat_sub <- mat_sub[, sample_order, drop = FALSE]

  log_transform <- params$log_transform %||% "log10"
  if (!log_transform %in% c("log10", "none")) {
    log_transform <- "log10"
  }

  mat_log <- mat_sub
  if (identical(log_transform, "log10")) {
    mat_log[!is.finite(mat_log) | mat_log <= 0] <- NA_real_
    mat_log <- log10(mat_log)
  } else {
    mat_log[!is.finite(mat_log)] <- NA_real_
  }

  scale_row <- function(x) {
    if (all(is.na(x))) return(x)
    mu <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(s) || s == 0) {
      y <- x
      y[!is.na(y)] <- 0
      return(y)
    }
    (x - mu) / s
  }

  mat_zscore <- t(apply(mat_log, 1, scale_row))
  dimnames(mat_zscore) <- dimnames(mat_log)

  exclude_na_rows <- isTRUE(params$exclude_na_rows %||% FALSE)
  if (exclude_na_rows) {
    keep <- rowSums(is.na(mat_log)) == 0
    mat_log <- mat_log[keep, , drop = FALSE]
    mat_zscore <- mat_zscore[keep, , drop = FALSE]
    matched_genes <- rownames(mat_log)
  }

  compute_z_dendro <- function(m) {
    if (nrow(m) < 2) return(NULL)

    clust_rows <- which(rowSums(!is.na(m)) >= 2)
    if (length(clust_rows) < 2) return(NULL)

    cmat <- suppressWarnings(stats::cor(t(m[clust_rows, , drop = FALSE]), use = "pairwise.complete.obs"))
    cmat[is.na(cmat)] <- 0
    diag(cmat) <- 1

    d <- stats::as.dist(1 - cmat)
    stats::hclust(d, method = "average")
  }

  compute_abund_dendro <- function(m) {
    if (nrow(m) < 2) return(NULL)

    clust_rows <- which(rowSums(is.na(m)) == 0)
    if (length(clust_rows) < 2) return(NULL)

    d <- stats::dist(m[clust_rows, , drop = FALSE])
    stats::hclust(d, method = "ward.D2")
  }

  dendro_zscore <- compute_z_dendro(mat_zscore)
  dendro_abundance <- compute_abund_dendro(mat_log)

  group_vec <- samples$group_name[match(sample_order, samples[[sample_col]])]
  group_annotations <- data.frame(
    sample = sample_order,
    group = as.character(group_vec),
    stringsAsFactors = FALSE
  )

  groups <- unique(group_annotations$group)

  # Try to use colors from formatted Excel file (metadata$groups$color)
  # Fall back to auto-generated colors if not available
  meta_groups <- payload$metadata$groups
  if (!is.null(meta_groups) && is.data.frame(meta_groups) &&
      "group_name" %in% names(meta_groups) && "color" %in% names(meta_groups)) {
    # Use colors from formatted Excel
    color_map <- stats::setNames(
      as.character(meta_groups$color),
      as.character(meta_groups$group_name)
    )
    group_colors <- color_map[groups]
    # Fill in any missing colors with auto-generated ones
    missing <- is.na(group_colors) | !nzchar(group_colors)
    if (any(missing)) {
      auto_colors <- grDevices::hcl.colors(sum(missing), palette = "Dark 3")
      group_colors[missing] <- auto_colors
    }
    names(group_colors) <- groups
  } else {
    # Fall back to auto-generated colors
    group_colors <- grDevices::hcl.colors(max(1, length(groups)), palette = "Dark 3")
    group_colors <- group_colors[seq_along(groups)]
    names(group_colors) <- groups
  }

  list(
    engine_id = "heatmap",
    params = params,
    data = list(
      mat_log = mat_log,
      mat_zscore = mat_zscore,
      dendro_zscore = dendro_zscore,
      dendro_abundance = dendro_abundance,
      gene_order = matched_genes,
      sample_order = sample_order,
      group_annotations = group_annotations,
      group_colors = group_colors,
      matched_genes = matched_genes,
      unmatched_genes = unmatched_genes
    )
  )
}


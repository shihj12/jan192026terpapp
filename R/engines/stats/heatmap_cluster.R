# =========================================================
# R/engines/stats/heatmap_cluster.R — Heatmap Cluster Cutting
#
# Functions for cutting heatmap dendrograms into clusters
# and preparing gene lists for per-cluster GO-ORA analysis.
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Cut heatmap dendrogram into k clusters
#'
#' @param results Heatmap engine results (list with $data containing dendrograms)
#' @param dendro_type Which dendrogram to cut: "zscore" or "abundance"
#' @param k Number of clusters
#' @return List with cluster_assignments, cluster_genes, cluster_sizes, dendro_type, k
heatmap_cut_clusters <- function(results, dendro_type, k) {
  data <- results$data

  # Select dendrogram

  dendro <- if (identical(dendro_type, "abundance")) {
    data$dendro_abundance
  } else {
    data$dendro_zscore
  }

  if (is.null(dendro) || !inherits(dendro, "hclust")) {
    stop("No valid dendrogram available for cutting")
  }

  # Get gene order from dendrogram labels or fallback to data
  gene_order <- dendro$labels
  if (is.null(gene_order) || length(gene_order) == 0) {
    gene_order <- data$gene_order %||% data$matched_genes %||% rownames(data$mat_zscore)
  }

  if (is.null(gene_order) || length(gene_order) == 0) {
    stop("Cannot determine gene order for cluster cutting")
  }

  n_genes <- length(gene_order)

  # Validate k

  if (!is.numeric(k) || length(k) != 1 || is.na(k)) {
    stop("k must be a single numeric value")
  }
  k <- as.integer(k)
  if (k < 2) {
    stop("k must be at least 2")
  }
  if (k > n_genes) {
    stop(sprintf("k (%d) cannot exceed number of genes (%d)", k, n_genes))
  }

  # Cut the tree
  clusters <- stats::cutree(dendro, k = k)

  # clusters is a named integer vector if dendro had labels,

  # otherwise it's just integers in dendrogram order
  if (is.null(names(clusters))) {
    names(clusters) <- gene_order
  }

  # Build cluster gene lists
  cluster_genes <- lapply(seq_len(k), function(i) {
    names(clusters)[clusters == i]
  })
  names(cluster_genes) <- paste0("Cluster ", seq_len(k))

  cluster_sizes <- vapply(cluster_genes, length, integer(1))
  names(cluster_sizes) <- names(cluster_genes)

  list(
    cluster_assignments = clusters,
    cluster_genes = cluster_genes,
    cluster_sizes = cluster_sizes,
    dendro_type = dendro_type,
    k = k
  )
}

#' Preview cluster sizes without running full analysis
#'
#' @param results Heatmap engine results
#' @param dendro_type Which dendrogram: "zscore" or "abundance"
#' @param k Number of clusters
#' @return Named integer vector of cluster sizes, or NULL on error
heatmap_cluster_preview <- function(results, dendro_type, k) {
  tryCatch({
    info <- heatmap_cut_clusters(results, dendro_type, k)
    info$cluster_sizes
  }, error = function(e) {
    NULL
  })
}

#' Get maximum valid k for a dendrogram
#'
#' @param results Heatmap engine results
#' @param dendro_type Which dendrogram: "zscore" or "abundance"
#' @return Maximum k value, or 0 if dendrogram unavailable
heatmap_cluster_max_k <- function(results, dendro_type) {
  data <- results$data %||% list()

  dendro <- if (identical(dendro_type, "abundance")) {
    data$dendro_abundance
  } else {
    data$dendro_zscore
  }

  if (is.null(dendro) || !inherits(dendro, "hclust")) {
    return(0L)
  }

  # Number of leaves in the dendrogram
  n <- length(dendro$order)
  if (n < 2) return(0L)

  # Cap at 20 for usability

  min(n, 20L)
}

#' Check which dendrograms are available
#'
#' @param results Heatmap engine results
#' @return Named logical vector with zscore and abundance availability
heatmap_dendro_available <- function(results) {
  data <- results$data %||% list()

  c(
    zscore = !is.null(data$dendro_zscore) && inherits(data$dendro_zscore, "hclust"),
    abundance = !is.null(data$dendro_abundance) && inherits(data$dendro_abundance, "hclust")
  )
}

#' Convert cluster gene list to GO-ORA input (protein IDs)
#'
#' @param cluster_genes Character vector of gene symbols
#' @param ids_df Data frame with ID mappings
#' @param id_protein_col Column name for protein IDs
#' @param id_gene_col Column name for gene symbols
#' @return Character vector of protein IDs for GO-ORA query
heatmap_cluster_to_goora_input <- function(cluster_genes, ids_df, id_protein_col, id_gene_col) {
  if (is.null(cluster_genes) || length(cluster_genes) == 0) {
    return(character(0))
  }

  # If no mapping available, return gene symbols directly
  if (is.null(ids_df) || !is.data.frame(ids_df)) {
    return(cluster_genes)
  }

  if (!id_gene_col %in% names(ids_df) || !id_protein_col %in% names(ids_df)) {
    return(cluster_genes)
  }

  # Build gene -> protein mapping
  gene_to_protein <- stats::setNames(
    as.character(ids_df[[id_protein_col]]),
    as.character(ids_df[[id_gene_col]])
  )

  # Map gene symbols to protein IDs
  protein_ids <- unname(gene_to_protein[cluster_genes])
  protein_ids <- protein_ids[!is.na(protein_ids) & nzchar(protein_ids)]

  unique(protein_ids)
}

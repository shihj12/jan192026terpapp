# =========================================================
# R/pages/page_results.R — Results Viewer (tree-aware)
# - Tree navigation (step/view nesting)
# - Master style at step node + sparse overrides at view nodes
# - Renders plots via terpbook.R from results$data + effective style
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

.has_colourpicker <- requireNamespace("colourpicker", quietly = TRUE)
.has_dt <- requireNamespace("DT", quietly = TRUE)
.has_zip_pkg <- requireNamespace("zip", quietly = TRUE)

# ---- Cross-platform zip helper -----------------------------------------------
# Tries multiple methods to create a zip archive:
# 1. 'zip' package (pure R, most reliable)
# 2. utils::zip (requires zip CLI on PATH)
# 3. PowerShell Compress-Archive (Windows fallback)
res_safe_zip <- function(zipfile, files, root = ".") {
  # Normalize paths
  zipfile <- normalizePath(zipfile, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)

  # Remove existing zip file if it exists
  if (file.exists(zipfile)) {
    file.remove(zipfile)
  }

  # Method 1: Use 'zip' package if available (pure R, most reliable)
  if (.has_zip_pkg) {
    tryCatch({
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(root)
      zip::zip(zipfile, files = files, recurse = TRUE, compression_level = 9)
      if (file.exists(zipfile) && file.info(zipfile)$size > 0) {
        return(TRUE)
      }
    }, error = function(e) NULL)
  }

  # Method 2: Try utils::zip (works if 'zip' CLI is on PATH)
  tryCatch({
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(root)
    result <- utils::zip(zipfile = zipfile, files = files)
    if (file.exists(zipfile) && file.info(zipfile)$size > 0) {
      return(TRUE)
    }
  }, error = function(e) NULL, warning = function(w) NULL)

  # Method 3: PowerShell fallback for Windows
  if (.Platform$OS.type == "windows") {
    tryCatch({
      # Build full paths for PowerShell
      source_path <- normalizePath(file.path(root, files[1]), winslash = "\\", mustWork = TRUE)
      dest_path <- normalizePath(zipfile, winslash = "\\", mustWork = FALSE)

      # Use PowerShell Compress-Archive
      ps_cmd <- sprintf(
        "powershell -NoProfile -Command \"Compress-Archive -Path '%s\\*' -DestinationPath '%s' -Force\"",
        source_path, dest_path
      )
      system(ps_cmd, intern = TRUE, ignore.stderr = TRUE)

      if (file.exists(zipfile) && file.info(zipfile)$size > 0) {
        return(TRUE)
      }
    }, error = function(e) NULL)
  }

  stop("Failed to create zip archive. Install the 'zip' R package for best compatibility.")
}


# ---- Toggle switch (Shiny-native checkbox styled as a switch) ---------------
res_switch_input <- function(id, left_label, right_label, value = FALSE, help_title = NULL) {
  div(
    class = "res-switch-row",
    title = help_title %||% NULL,
    div(class = "res-switch-left", left_label),
    tags$label(
      class = "res-switch",
      tags$input(
        id = id, type = "checkbox",
        checked = if (isTRUE(value)) "checked" else NULL
      ),
      tags$span(class = "res-switch-track"),
      tags$span(class = "res-switch-knob")
    ),
    div(class = "res-switch-right", right_label)
  )
}

# ---- Collapsible section UI helper -----------------------------------------
res_collapse_section_ui <- function(id, title, badge_text = NULL, open = FALSE, ...) {
  div(
    class = paste("collapse-section", if (open) "open" else ""),
    id = id,
    div(
      class = "collapse-header",
      div(
        class = "collapse-title",
        span(class = "collapse-icon", HTML("&#9654;")),
        title
      ),
      if (!is.null(badge_text)) span(class = "collapse-badge", badge_text) else NULL
    ),
    div(class = "collapse-body", ...)
  )
}

# ---- Style field grouping for collapsible sections -------------------------
# Returns section name for a given style field
# Returns NULL for selectors that should stay at top (not in accordion)
#
# IMPORTANT: This uses EXPLICIT field lists, not regex matching.
# When adding new fields to the registry, add them to the appropriate list here.
get_style_section <- function(field_name) {

  # =====================================================
  # SELECTORS - NOT in any accordion (return NULL)
  # Mode/type selectors that change graph type or major behavior
  # These remain always-visible at the top of the style panel
  # =====================================================
  selector_fields <- c(
    # Graph type selectors
    "plot_type", "overlap_plot_type", "view_mode",
    # Layout and orientation
    "layout", "orientation",
    # Data transformation and mode selectors
    "transform", "acquisition_mode", "label_mode",
    # Color mode selectors (control downstream color field visibility)
    "color_mode", "bar_color_mode", "point_color_mode",
    # Filter selectors
    "ontology_filter", "pathway_db", "class_level",
    # Group/comparison selectors
    "selected_group",
    # Line type selector
    "line_type",
    # CV plot mode
    "cv_plot_mode", "x_axis_mode"
  )
  if (field_name %in% selector_fields) return(NULL)

  # =====================================================
  # LABELS section
  # Text content, font sizes, show/hide toggles for text annotations
  # =====================================================
  label_fields <- c(
    # Show/hide toggles for text elements
    "show_rho", "show_equation", "show_n", "show_percentage", "show_group_names",
    "show_sig_labels", "show_summary_cards", "show_values", "show_count_labels",
    "show_row_labels", "show_labels", "venn_show_percentage",
    # Text sizes (font sizes for various text elements)
    "rho_text_size", "font_size", "label_font_size", "row_font_size",
    "value_label_size", "count_labels_size", "count_label_size",
    "summary_text_size", "venn_text_size", "venn_set_name_size",
    "mean_text_size", "scree_text_size",
    # Text positions
    "rho_position_x", "rho_position_y",
    # Axis titles (text content, not axis styling)
    "axis_title", "x_axis_title", "y_axis_title",
    # Label count and rotation
    "n_labels", "label_rotation",
    # Mean display toggles and type (must stay together for conditionalPanel)
    "show_mean", "show_mean_value", "show_global_mean", "mean_type",
    # Reference line/guide toggles and sizes (must stay together for conditionalPanel)
    "show_ref_lines", "ref_line_size", "show_diagonal_guides", "diagonal_guide_size",
    # Threshold toggle (keep with threshold styling in graph elements? No - it's a show/hide)
    "threshold_show"
  )
  if (field_name %in% label_fields) return("labels")

  # =====================================================
  # AXIS ELEMENTS section
  # Axis styling, range modes, min/max values, plot dimensions
  # =====================================================
  axis_fields <- c(
    # Range mode selectors
    "x_range_mode", "y_range_mode", "xy_range_mode", "y_limit_mode",
    # Min/max values
    "x_min", "x_max", "y_min", "y_max", "xy_min", "xy_max", "ymax_protein",
    # Axis styling
    "axis_style", "axis_text_size",
    # Plot dimensions (width/height of the plot itself, in inches)
    "width", "height",
    # Threshold/pool values (axis-related cutoffs)
    "pool_value", "cv_threshold", "pool_above"
  )
  if (field_name %in% axis_fields) return("axis_elements")

  # =====================================================
  # GRAPH ELEMENTS section (default catchall)
  # Colors, point/line sizes, opacity/alpha, fills, outlines, visual styling
  #
  # Includes (non-exhaustive):
  # - Point styling: point_size, point_alpha, dot_alpha
  # - Line styling: line_size, line_color, mean_line_size, mean_line_color
  # - Fill colors: flat_color, bar_color, bar_fill_color, scree_bar_color
  # - Outline styling: bar_outline_color, bar_outline_width, venn_outline_*
  # - Significance colors: col_sig_up, col_sig_down, col_nonsig
  # - ID/Quant colors: color_quantified, color_identified
  # - Opacity: alpha, ellipse_alpha, venn_fill_alpha
  # - Global mean styling: global_mean_color, global_mean_size
  # - Threshold styling: threshold_color, threshold_width, threshold_linetype
  # - CV bin colors: cv_bin_1 through cv_bin_6, cv_bin_*_color
  # - Other toggles: show_ellipse, show_cut_lines, show_bar_outline, show_go_id
  # - Misc: na_color, color_palette, fdr_palette, rho_color, count_labels_color
  # =====================================================
  return("graph_elements")
}

# Section display names and default open states
# Grouped into 3 accordion sections; selectors (returning NULL) stay at top
STYLE_SECTIONS <- list(
  labels = list(title = "Labels", open = TRUE),
  graph_elements = list(title = "Graph Elements", open = FALSE),
  axis_elements = list(title = "Axis Elements", open = FALSE)
)

# ---- GO table helper functions (visibility checkbox + copy genes button) ----------

# Generate HTML checkbox for visibility toggle in GO enrichment tables
# checked = visible (shown on plot), unchecked = hidden (not on plot but in table)
res_make_visibility_checkboxes <- function(term_names, hidden_terms = character()) {
  vapply(seq_along(term_names), function(i) {
    term <- term_names[i]
    is_visible <- !(term %in% hidden_terms)
    checked_attr <- if (is_visible) " checked" else ""
    # Escape HTML special chars in term name for safe embedding
    safe_term <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", term)))))
    sprintf(
      '<input type="checkbox" class="go-term-visibility-cb" data-term="%s"%s title="%s" />',
      safe_term,
      checked_attr,
      if (is_visible) "Click to hide this term from plot" else "Click to show this term on plot"
    )
  }, character(1))
}

# Generate HTML button for copying gene list to clipboard
# Generate HTML button for searching genes via UniProt
res_make_search_gene_buttons <- function(term_names, gene_strings) {
  vapply(seq_along(term_names), function(i) {
    term <- term_names[i]
    genes <- gene_strings[i]
    # Convert gene string (comma/semicolon/pipe separated) to newline-separated
    genes_clean <- gsub("[,;|/]", "\n", as.character(genes))
    genes_clean <- gsub("\n+", "\n", genes_clean)
    genes_clean <- trimws(genes_clean)
    # Escape for safe HTML embedding
    safe_term <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", term)))))
    safe_genes <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", genes_clean)))))
    sprintf(
      '<button type="button" class="btn btn-xs btn-info go-search-genes-btn" data-term="%s" data-genes="%s" title="Search genes in UniProt"><i class="fa fa-search"></i></button>',
      safe_term, safe_genes
    )
  }, character(1))
}

# Add Visible (visibility) column and Search button to a GO enrichment table dataframe
# hidden_terms: character vector of terms that are currently hidden from the plot
# term_labels: named list mapping original term names to custom display labels
# include_search: if TRUE, add a Search button column for UniProt lookup
res_add_go_table_actions <- function(df, hidden_terms = character(), term_labels = list(), include_search = TRUE) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  # Need term or term_name column - prioritize "term" if both exist
  if ("term" %in% names(df)) {
    term_col <- "term"
  } else if ("term_name" %in% names(df)) {
    term_col <- "term_name"
  } else {
    return(df)
  }

  # Add visibility checkbox column (checked = visible on plot)
  df$Visible <- res_make_visibility_checkboxes(df[[term_col]], hidden_terms)

  # Store original term for internal reference
  df$.original_term <- df[[term_col]]

  # Add Search button column for UniProt lookup (GO-ORA and 1dGOFCS only)
  if (isTRUE(include_search)) {
    # Get gene list from available columns
    gene_col <- intersect(c("genes", "geneID", "protein_ids", "core_enrichment", "Genes"), names(df))[1]
    if (!is.na(gene_col)) {
      df$Search <- res_make_search_gene_buttons(df[[term_col]], df[[gene_col]])
    }
  }

  # Apply custom labels if any exist
  if (length(term_labels) > 0) {
    for (i in seq_len(nrow(df))) {
      orig <- df$.original_term[i]  # Use original term for lookup
      if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
        df[[term_col]][i] <- term_labels[[orig]]
      }
    }
  }

  # Add hidden status for row styling (used by DT rowCallback)
  df$.hidden <- df$.original_term %in% hidden_terms

  # Reorder columns to put action columns first (but keep internal cols at end)
  cols <- names(df)
  action_cols <- intersect(c("Visible", "Search"), cols)
  internal_cols <- c(".hidden", ".original_term")
  other_cols <- setdiff(cols, c(action_cols, internal_cols))
  df <- df[, c(action_cols, other_cols, internal_cols), drop = FALSE]

  df
}

# Get columns to hide for GO enrichment tables based on engine type
# Returns column names that should be removed from display (but kept in Excel export)
res_go_hidden_cols <- function(engine_id) {
  engine_id <- tolower(as.character(engine_id %||% ""))

  # Base hidden columns for all GO engines
  hide_cols <- c(
    "genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",  # Gene lists (Search button provides access)
    "neglog10_fdr", "neglog10fdr",  # Redundant with FDR
    "n_term",  # Internal metadata
    "n",  # Hide normalized n (use n_genes instead)
    "term"  # Hide duplicate term column (use term_name)
  )

  # Engine-specific: hide fold_enrichment for FCS engines, hide score for ORA

  if (engine_id == "goora") {
    hide_cols <- c(hide_cols, "score", "score_x", "score_y")
  } else if (engine_id %in% c("1dgofcs", "2dgofcs")) {
    hide_cols <- c(hide_cols, "fold_enrichment")
  }

  unique(hide_cols)
}

# Editable GO terms table (Shiny inputs; used by GO-ORA/1D/2D GO viewers)
res_safe_input_id <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+", "", x)
  if (!nzchar(x) || grepl("^[0-9]", x)) x <- paste0("id_", x)
  x
}

res_editable_go_table_ui <- function(
    id_prefix,
    df,
    term_id_col = "term_id",
    term_name_col = "term_name",
    hidden_term_ids = character(),
    term_labels_by_id = list(),
    gene_col_data = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(div(class = "text-muted", "No terms to display."))
  }
  if (!(term_id_col %in% names(df)) || !(term_name_col %in% names(df))) {
    return(div(class = "text-danger", "Table is missing required columns for editing."))
  }

  term_ids <- as.character(df[[term_id_col]])
  cols <- names(df)

  # Build header: Visible, term_id, term_name, Search, then other columns
  other_cols <- setdiff(cols, c(term_id_col, term_name_col))
  has_search <- !is.null(gene_col_data) && length(gene_col_data) == nrow(df)
  header_cols <- c("Visible", term_id_col, term_name_col, if (has_search) "Search" else NULL, other_cols)

  rows <- lapply(seq_len(nrow(df)), function(i) {
    term_id <- as.character(term_ids[[i]] %||% "")
    safe_term_id <- res_safe_input_id(term_id)

    vis_id <- paste0(id_prefix, "_vis_", safe_term_id)
    name_id <- paste0(id_prefix, "_name_", safe_term_id)

    default_name <- as.character(df[[term_name_col]][[i]] %||% "")
    display_name <- as.character(term_labels_by_id[[term_id]] %||% default_name)

    visible <- !(term_id %in% hidden_term_ids)

    # Build Search button HTML if gene data available
    search_btn <- NULL
    if (has_search) {
      genes <- as.character(gene_col_data[[i]] %||% "")
      genes_clean <- gsub("[,;|/]", "\n", genes)
      genes_clean <- gsub("\n+", "\n", genes_clean)
      genes_clean <- trimws(genes_clean)
      safe_term <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", display_name)))))
      safe_genes <- gsub("'", "&#39;", gsub('"', "&quot;", gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", genes_clean)))))
      search_btn <- tags$td(
        style = "text-align: center;",
        HTML(sprintf(
          '<button type="button" class="btn btn-xs btn-info go-search-genes-btn" data-term="%s" data-genes="%s" title="Search genes in UniProt"><i class="fa fa-search"></i></button>',
          safe_term, safe_genes
        ))
      )
    }

    tags$tr(
      tags$td(
        style = "width: 40px; text-align: center;",
        tags$div(
          style = "display: inline-block;",
          checkboxInput(vis_id, label = NULL, value = visible, width = "20px")
        )
      ),
      tags$td(tags$code(term_id)),
      tags$td(
        textInput(
          name_id,
          label = NULL,
          value = display_name,
          width = "100%"
        )
      ),
      search_btn,
      lapply(other_cols, function(col) {
        val <- df[[col]][[i]]
        tags$td(htmltools::htmlEscape(as.character(val %||% "")))
      })
    )
  })

  div(
    class = "res-editable-go-table",
    # Add CSS to constrain checkbox width
    tags$style(HTML("
      .res-editable-go-table .form-group { margin-bottom: 0; }
      .res-editable-go-table .checkbox { margin: 0; padding: 0; min-height: 0; }
      .res-editable-go-table input[type='checkbox'] { margin: 0; }
    ")),
    tags$div(style = "overflow-x: auto;",
             tags$table(
               class = "table table-sm table-striped",
               tags$thead(tags$tr(lapply(header_cols, function(h) {
                 if (h == "Visible") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else if (h == "Search") {
                   tags$th(style = "width: 50px; text-align: center;", h)
                 } else {
                   tags$th(h)
                 }
               }))),
               tags$tbody(rows)
             ))
  )
}

res_bind_editable_go_table <- function(
    id_prefix,
    df,
    term_id_col = "term_id",
    input,
    session,
    on_term_name_change = NULL,
    on_visibility_change = NULL
) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
  if (!(term_id_col %in% names(df))) return(invisible(NULL))

  if (is.null(session$userData$res_editable_go_table_bound)) {
    session$userData$res_editable_go_table_bound <- new.env(parent = emptyenv())
  }
  bound <- session$userData$res_editable_go_table_bound

  term_ids <- unique(as.character(df[[term_id_col]]))
  term_ids <- term_ids[nzchar(term_ids)]

  for (term_id in term_ids) {
    local({
      term_id_local <- term_id
      safe_term_id <- res_safe_input_id(term_id_local)
      vis_id <- paste0(id_prefix, "_vis_", safe_term_id)
      name_id <- paste0(id_prefix, "_name_", safe_term_id)

      vis_key <- paste0("vis|", vis_id)
      if (!exists(vis_key, envir = bound, inherits = FALSE)) {
        assign(vis_key, TRUE, envir = bound)
        observeEvent(input[[vis_id]], {
          if (is.function(on_visibility_change)) {
            on_visibility_change(term_id_local, isTRUE(input[[vis_id]]))
          }
        }, ignoreInit = TRUE)
      }

      name_key <- paste0("name|", name_id)
      if (!exists(name_key, envir = bound, inherits = FALSE)) {
        assign(name_key, TRUE, envir = bound)
        observeEvent(input[[name_id]], {
          if (is.function(on_term_name_change)) {
            on_term_name_change(term_id_local, as.character(input[[name_id]] %||% ""))
          }
        }, ignoreInit = TRUE)
      }
    })
  }

  invisible(NULL)
}

# ---- Registry helpers --------------------------------------------------------

res_registry <- function() {
  if (!exists("msterp_engine_registry", mode = "function")) return(NULL)
  tryCatch(msterp_engine_registry(), error = function(e) NULL)
}

res_engine_def <- function(engine_id, registry) {
  if (is.null(registry) || is.null(registry$engines)) return(NULL)
  key <- tolower(as.character(engine_id %||% ""))
  if (exists("migrate_legacy_engine_name", mode = "function")) {
    key <- tolower(migrate_legacy_engine_name(key))
  }
  registry$engines[[key]] %||% NULL
}

res_flatten_schema <- function(schema) {
  if (is.null(schema) || length(schema) == 0) return(list())
  out <- list()
  
  walk <- function(x) {
    if (!is.list(x)) return()
    if (!is.null(x$name) && !is.null(x$type)) {
      out[[length(out) + 1L]] <<- x
      return()
    }
    for (i in seq_along(x)) walk(x[[i]])
  }
  
  walk(schema)
  
  # Guard against duplicate fields (e.g., accidental duplicate schema_field definitions).
  # Keep the first occurrence of each field name.
  field_names <- vapply(out, function(f) as.character(f$name %||% ""), character(1))
  out[!duplicated(field_names)]
}

res_extract_ggplots <- function(rendered) {
  if (is.null(rendered)) return(list())
  
  plots <- rendered$plots %||% list()
  
  # If a single ggplot was returned directly
  if (inherits(plots, "ggplot")) plots <- list(plot = plots)
  
  # If it's NULL or not a list, normalize
  if (is.null(plots)) plots <- list()
  if (!is.list(plots)) plots <- list()
  
  # Keep only ggplots (including patchwork objects which are ggplot compositions)
  if (length(plots) > 0) {
    keep <- vapply(plots, function(x) inherits(x, "ggplot") || inherits(x, "patchwork"), logical(1))
    plots <- plots[keep]
  }
  
  # Ensure names match length and are non-empty
  n <- length(plots)
  if (n == 0) return(list())
  
  nms <- names(plots)
  if (is.null(nms) || length(nms) != n) nms <- rep("", n)
  bad <- !nzchar(nms)
  if (any(bad)) nms[bad] <- paste0("plot_", which(bad))
  names(plots) <- nms
  
  plots
}

res_extract_tables <- function(rendered) {
  if (is.null(rendered)) return(list())
  
  tbls <- rendered$tables %||% list()
  
  # If a single data.frame was returned directly
  if (is.data.frame(tbls) || is.matrix(tbls)) tbls <- list(table = tbls)
  
  if (is.null(tbls)) tbls <- list()
  if (!is.list(tbls)) tbls <- list()
  
  if (length(tbls) > 0) {
    keep <- vapply(tbls, function(x) is.data.frame(x) || is.matrix(x), logical(1))
    tbls <- tbls[keep]
  }
  
  n <- length(tbls)
  if (n == 0) return(list())
  
  nms <- names(tbls)
  if (is.null(nms) || length(nms) != n) nms <- rep("", n)
  bad <- !nzchar(nms)
  if (any(bad)) nms[bad] <- paste0("table_", which(bad))
  names(tbls) <- nms
  
  tbls
}

res_pretty_label <- function(x, engine_id = "") {
  x <- as.character(x %||% "")
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- gsub("\\bvs\\b", "-", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  
  # Optional nicety for GO-ORA children labels like "GO up"/"GO down"
  eng <- tolower(as.character(engine_id %||% ""))
  if (eng == "goora") {
    if (grepl("^go\\s*-?\\s*up$|^go\\s*up$|^go\\s*up$", x, ignore.case = TRUE)) x <- "ORA Up"
    if (grepl("^go\\s*-?\\s*down$|^go\\s*down$|^go\\s*down$", x, ignore.case = TRUE)) x <- "ORA Down"
    if (grepl("^go\\s*up$", x, ignore.case = TRUE)) x <- "ORA Up"
    if (grepl("^go\\s*down$", x, ignore.case = TRUE)) x <- "ORA Down"
    if (grepl("^go up$", x, ignore.case = TRUE)) x <- "ORA Up"
    if (grepl("^go down$", x, ignore.case = TRUE)) x <- "ORA Down"
  }
  
  x
}

res_letters <- function(k) {
  # 1 -> a, 26 -> z, 27 -> aa ...
  k <- as.integer(k)
  if (!is.finite(k) || k < 1) return("a")
  out <- character()
  while (k > 0) {
    k <- k - 1L
    out <- c(letters[(k %% 26L) + 1L], out)
    k <- k %/% 26L
  }
  paste0(out, collapse = "")
}

# ---- Overview node renderer -------------------------------------------------
res_render_overview <- function(run_root, manifest, nodes_df, terpbook_filename = NULL, has_unsaved_changes = FALSE) {
  # Read log.txt from run_root
  log_content <- ""
  log_path <- file.path(run_root, "log.txt")
  if (file.exists(log_path)) {
    log_content <- tryCatch({
      paste(readLines(log_path, warn = FALSE), collapse = "\n")
    }, error = function(e) "Failed to read log.txt")
  }

  # Use terpbook filename if provided, otherwise fall back to run_root basename
  run_name <- if (!is.null(terpbook_filename) && nzchar(terpbook_filename)) {
    terpbook_filename
  } else {
    basename(run_root)
  }

  # Count ALL steps including Data Processor
  n_steps <- if (!is.null(nodes_df)) {
    sum(nodes_df$kind == "step")
  } else 0

  # Extract date from run log (first timestamp) or fall back to manifest
  run_date <- "Unknown"
  if (nzchar(log_content)) {
    # Try to extract first timestamp from log (common formats: YYYY-MM-DD, MM/DD/YYYY, etc.)
    date_match <- regmatches(log_content, regexpr("\\d{4}-\\d{2}-\\d{2}|\\d{2}/\\d{2}/\\d{4}", log_content))
    if (length(date_match) > 0 && nzchar(date_match[[1]])) {
      run_date <- date_match[[1]]
    }
  }
  # Fall back to manifest if no date found in log
  if (run_date == "Unknown") {
    run_date <- manifest$created_at %||% manifest$run_date %||% "Unknown"
  }

  # 60/40 split layout for Overview
  div(
    class = "res-overview-layout",
    style = paste0(
      "display: flex; ",
      "gap: 16px; ",
      "height: 100%; ",
      "min-height: 0; ",
      "overflow: hidden;"
    ),

    # LEFT SIDE (60%) - Run Overview + Run Log
    div(
      style = paste0(
        "flex: 0 0 60%; ",
        "display: flex; ",
        "flex-direction: column; ",
        "gap: 12px; ",
        "min-height: 0; ",
        "overflow: hidden;"
      ),

      # Run Overview panel
      div(
        class = "res-panel",
        style = "flex: 0 0 auto;",
        div(class = "res-panel-head"),
        div(
          class = "res-panel-body",
          style = "padding: 16px;",
          h4(style = "margin: 0 0 12px 0; font-weight: 800;", "Run Overview"),
          div(
            style = "display: grid; grid-template-columns: auto 1fr; gap: 8px 16px;",
            tags$span(style = "font-weight: 700;", "Run:"),
            tags$span(run_name),
            tags$span(style = "font-weight: 700;", "Date:"),
            tags$span(as.character(run_date)),
            tags$span(style = "font-weight: 700;", "Steps:"),
            tags$span(as.character(n_steps))
          )
        )
      ),

      # Run Log panel
      div(
        class = "res-panel",
        style = "flex: 1 1 auto; min-height: 0;",
        div(class = "res-panel-head"),
        div(
          class = "res-panel-body",
          style = "padding: 16px; display: flex; flex-direction: column; min-height: 0;",
          h4(style = "margin: 0 0 12px 0; font-weight: 800; flex: 0 0 auto;", "Run Log"),
          div(
            style = paste0(
              "flex: 1 1 auto; min-height: 0; overflow: auto; ",
              "background: #1a1a1a; color: #e8e8e8; ",
              "font-family: 'Consolas', 'Monaco', 'Courier New', monospace; ",
              "font-size: 13px; line-height: 1.5; ",
              "padding: 12px; border-radius: 6px; ",
              "white-space: pre-wrap; word-wrap: break-word;"
            ),
            if (nzchar(log_content)) log_content else "No log.txt found in run."
          )
        )
      )
    ),

    # RIGHT SIDE (40%) - Downloads + Update .terpbook
    div(
      style = paste0(
        "flex: 0 0 40%; ",
        "display: flex; ",
        "flex-direction: column; ",
        "gap: 12px; ",
        "min-height: 0; ",
        "overflow: hidden;"
      ),

      # Downloads panel
      div(
        class = "res-panel",
        style = "flex: 0 0 auto;",
        div(class = "res-panel-head"),
        div(
          class = "res-panel-body",
          style = "padding: 16px;",
          h4(style = "margin: 0 0 12px 0; font-weight: 800;", "Downloads"),
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            downloadButton("res_dl_log", "Log (.txt)", class = "btn-sm", style = "width: 100%;"),
            actionButton("res_bulk_download_modal", "Bulk Download Graphs...", class = "btn-sm", style = "width: 100%;"),
            actionButton("res_dl_terpbook_pdf_modal", "Terpbook (.pdf)", class = "btn-sm", style = "width: 100%;"),
            downloadButton("res_dl_processed", "Processed Data (.xlsx)", class = "btn-sm", style = "width: 100%;"),
            downloadButton("res_dl_markers", "Marker Sets (.xlsx)", class = "btn-sm", style = "width: 100%;")
          )
        )
      ),

      # Update .terpbook panel
      div(
        class = "res-panel",
        style = "flex: 0 0 auto;",
        div(class = "res-panel-head"),
        div(
          class = "res-panel-body",
          style = "padding: 16px;",
          h4(style = "margin: 0 0 12px 0; font-weight: 800;", "Update .terpbook"),
          p(style = "margin: 0 0 10px 0; font-size: 12px; color: #666;",
            "Edits are saved in this session. Download the updated .terpbook to keep them."),
          div(
            style = "display: flex; flex-direction: column; gap: 8px;",
            downloadButton(
              "res_save_terpbook",
              if (isTRUE(has_unsaved_changes)) "Download Updated .terpbook *" else "Download Updated .terpbook",
              class = if (isTRUE(has_unsaved_changes)) "btn-primary" else "",
              style = "width: 100%; font-weight: 700;"
            )
          )
        )
      )
    )
  )
}

res_compute_node_numbers <- function(nodes_df) {

  if (is.null(nodes_df) || nrow(nodes_df) == 0) {
    return(setNames(character(), character()))
  }

  ord_idx <- seq_len(nrow(nodes_df))
  get_children <- function(pid) {
    idx <- which(nodes_df$parent_id == pid)
    if (length(idx) == 0) return(character())
    nodes_df$node_id[idx][order(ord_idx[idx])]
  }

  # Include ALL steps in numbering (including Data Processor)
  step_idx <- which(nodes_df$kind == "step")
  if (length(step_idx) == 0) return(setNames(character(), character()))
  step_idx <- step_idx[order(nodes_df$step_index[step_idx], ord_idx[step_idx], na.last = TRUE)]

  num <- list()

  walk <- function(parent_id, parent_base, depth) {
    kids <- get_children(parent_id)
    if (length(kids) == 0) return()

    for (i in seq_along(kids)) {
      kid <- kids[[i]]

      # At depth 1, children use [N.i] format (e.g., [5.1], [5.2])
      # At depth 2, grandchildren use [N.i]a, [N.i]b format
      # At depth 3+, use [N.i]a.j format
      kid_num <- if (depth == 1L) {
        sprintf("[%s.%d]", parent_base, i)
      } else if (depth == 2L) {
        paste0(parent_base, res_letters(i))
      } else {
        paste0(parent_base, ".", i)
      }

      num[[kid]] <<- kid_num
      # For depth 1, pass the numeric base without brackets for grandchildren
      kid_base <- if (depth == 1L) {
        sprintf("%s.%d", parent_base, i)
      } else {
        kid_num
      }
      walk(kid, kid_base, depth + 1L)
    }
  }

  # Use [N] format starting from 1 (Overview is [0] and handled separately)
  for (i in seq_along(step_idx)) {
    nid <- nodes_df$node_id[step_idx[[i]]]
    step_num <- sprintf("[%d]", i)  # [1], [2], [3]...
    num[[nid]] <- step_num
    walk(nid, as.character(i), depth = 1L)  # Pass numeric base for children
  }

  # Add [0] for overview node if present
  overview_idx <- which(nodes_df$node_id == "overview")
  if (length(overview_idx) > 0) {
    num[["overview"]] <- "[0]"
  }

  unlist(num, use.names = TRUE)
}

res_should_lock_nodes <- function(nodes_df, registry) {
  if (is.null(nodes_df) || nrow(nodes_df) == 0) return(logical())
  if (is.null(registry) || !is.list(registry$engines)) return(rep(FALSE, nrow(nodes_df)))

  vapply(seq_len(nrow(nodes_df)), function(i) {
    eng_id <- tolower(nodes_df$engine_id[i] %||% "")
    is_step <- identical(nodes_df$kind[i] %||% "", "step") || identical(as.integer(nodes_df$depth[i] %||% 0L), 0L)
    is_step && isTRUE((registry$engines[[eng_id]] %||% list())$locked_parent)
  }, logical(1))
}

# Check if a node should be hidden from the results tree
# Engines with results_hidden_system_generated = TRUE are hidden when they appear as substeps (depth > 0)
res_should_hide_nodes <- function(nodes_df, registry) {
  if (is.null(nodes_df) || nrow(nodes_df) == 0) return(logical())
  if (is.null(registry) || !is.list(registry$engines)) return(rep(FALSE, nrow(nodes_df)))

  vapply(seq_len(nrow(nodes_df)), function(i) {
    eng_id <- tolower(nodes_df$engine_id[i] %||% "")
    depth <- as.integer(nodes_df$depth[i] %||% 0L)
    eng <- registry$engines[[eng_id]] %||% list()
    # Hide if engine has results_hidden_system_generated AND node is a substep (depth > 0)
    isTRUE(eng$results_hidden_system_generated) && depth > 0
  }, logical(1))
}


res_viewer_schema <- function(engine_def) {
  res_flatten_schema(c(engine_def$style_schema %||% list(), engine_def$viewer_schema %||% list()))
}

res_style_defaults <- function(engine_def) {
  schema <- res_viewer_schema(engine_def)
  out <- list()
  for (f in schema) out[[f$name]] <- f$default
  out
}

res_field_input_id <- function(node_id, field_name) {
  paste0("res_", gsub("[^A-Za-z0-9_]", "_", node_id), "_", field_name)
}

res_is_color_field <- function(field) {
  nm <- tolower(as.character(field$name %||% ""))
  lb <- tolower(as.character(field$label %||% ""))
  grepl("color|colour|hex", nm) ||
    grepl("color|colour|hex", lb) ||
    grepl("^col_", nm) ||
    nm %in% c("flat_color", "point_color", "line_color", "fill_color", "rho_color")
}

res_color_input <- function(id, label, value) {
  value <- as.character(value %||% "#B0B0B0")
  if (.has_colourpicker) {
    colourpicker::colourInput(id, label, value = value)
  } else {
    textInput(id, label, value = value, placeholder = "#RRGGBB")
  }
}

res_field_ui_core <- function(node_id, field, value_override = NULL, dynamic_choices = NULL) {
  nm <- field$name
  id <- res_field_input_id(node_id, nm)
  type <- field$type %||% "string"
  label <- field$label %||% nm
  val <- value_override %||% field$default

  is_multiline <- isTRUE(grepl("label|labels|json", nm, ignore.case = TRUE))

  if (identical(nm, "view_mode") && identical(type, "choice")) {
    choices <- field$choices %||% c("export_preview", "interactive")
    if (all(c("export_preview", "interactive") %in% choices)) {
      labels <- field$choice_labels %||% choices
      left_label <- labels[match("export_preview", choices)]
      right_label <- labels[match("interactive", choices)]
      if (!nzchar(left_label %||% "")) left_label <- "Export preview"
      if (!nzchar(right_label %||% "")) right_label <- "Interactive"
      is_interactive <- isTRUE(val) || identical(as.character(val %||% ""), "interactive")
      return(res_switch_input(id, left_label, right_label, value = is_interactive))
    }
  }

  # Special handling for selected_group: use dynamic choices from results data
  # This allows the group selector for within_groups mode to show actual groups
  if (identical(nm, "selected_group") && !is.null(dynamic_choices) && length(dynamic_choices) > 0) {
    return(selectInput(id, label, choices = dynamic_choices, selected = val %||% dynamic_choices[[1]]))
  }

  if (identical(type, "bool")) {
    return(checkboxInput(id, label, value = isTRUE(val)))
  }
  if (identical(type, "choice")) {
    ch <- field$choices %||% character()
    # Use choice_labels if provided, otherwise use choices as labels
    ch_labels <- field$choice_labels %||% ch
    if (length(ch_labels) != length(ch)) ch_labels <- ch  # Fallback if lengths don't match
    choices_vec <- setNames(ch, ch_labels)
    return(selectInput(id, label, choices = choices_vec, selected = val %||% (ch[[1]] %||% NULL)))
  }
  if (identical(type, "int")) {
    return(numericInput(
      id, label,
      value = suppressWarnings(as.integer(val %||% 0)),
      min = field$min %||% NA, max = field$max %||% NA, step = 1
    ))
  }
  if (identical(type, "num")) {
    return(numericInput(
      id, label,
      value = suppressWarnings(as.numeric(val %||% 0)),
      min = field$min %||% NA, max = field$max %||% NA, step = 0.1
    ))
  }
  if (identical(type, "range")) {
    v <- suppressWarnings(as.numeric(val %||% c(field$min %||% 0, field$max %||% 1)))
    if (length(v) != 2 || any(is.na(v))) v <- c(field$min %||% 0, field$max %||% 1)
    return(sliderInput(
      id, label,
      min = field$min %||% min(v),
      max = field$max %||% max(v),
      value = c(min(v), max(v))
    ))
  }

  if (res_is_color_field(field)) {
    return(res_color_input(id, label, val))
  }
  if (is_multiline) {
    return(textAreaInput(id, label, value = as.character(val %||% ""), rows = 6))
  }
  textInput(id, label, value = as.character(val %||% ""))
}

# Conditional visibility rules (PDF)
res_wrap_conditionals <- function(node_id, field, ui) {
  fname <- field$name
  
  cond <- NULL
  
  if (fname %in% c("ymax_protein")) {
    ctrl <- res_field_input_id(node_id, "y_limit_mode")
    cond <- sprintf("input['%s'] == 'manual'", ctrl)
  }
  
  if (fname %in% c("xy_min", "xy_max")) {
    ctrl <- res_field_input_id(node_id, "xy_range_mode")
    cond <- sprintf("input['%s'] == 'manual'", ctrl)
  }
  
  if (fname %in% c("x_min", "x_max")) {
    ctrl <- res_field_input_id(node_id, "x_range_mode")
    cond <- sprintf("(typeof input['%s'] === 'undefined') || (input['%s'] == 'manual')", ctrl, ctrl)
  }
  
  if (fname %in% c("y_min", "y_max")) {
    ctrl <- res_field_input_id(node_id, "y_range_mode")
    cond <- sprintf("input['%s'] == 'manual'", ctrl)
  }
  
  if (fname == "ellipse_alpha") {
    ctrl <- res_field_input_id(node_id, "show_ellipse")
    cond <- sprintf("input['%s'] == true", ctrl)
  }
  
  if (fname %in% c("guide_alpha", "guide_size")) {
    ctrl <- res_field_input_id(node_id, "show_guides")
    cond <- sprintf("input['%s'] == true", ctrl)
  }
  
  if (fname == "flat_color") {
    # Works for engines that use either `color_mode` or `point_color_mode`
    ctrl1 <- res_field_input_id(node_id, "color_mode")
    ctrl2 <- res_field_input_id(node_id, "point_color_mode")
    cond <- sprintf("(input['%s'] == 'flat') || (input['%s'] == 'flat')", ctrl1, ctrl2)
  }

  if (fname == "bar_color") {
    ctrl <- res_field_input_id(node_id, "bar_color_mode")
    cond <- sprintf("input['%s'] == 'flat'", ctrl)
  }

  # Spearman: show_equation only visible when a best-fit line is selected
  if (fname == "show_equation") {
    ctrl <- res_field_input_id(node_id, "line_type")
    cond <- sprintf("input['%s'] != 'none'", ctrl)
  }

  # Bars: outline controls only visible when show_bar_outline is TRUE
  if (fname %in% c("bar_outline_color", "bar_outline_width")) {
    ctrl <- res_field_input_id(node_id, "show_bar_outline")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # Subloc: global mean color/size only visible when show_global_mean is checked
  if (fname %in% c("global_mean_color", "global_mean_size")) {
    ctrl <- res_field_input_id(node_id, "show_global_mean")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # IDQuant CV%: threshold styling only visible when threshold line is enabled
  if (fname %in% c("threshold_color", "threshold_width", "threshold_linetype")) {
    ctrl <- res_field_input_id(node_id, "threshold_show")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # IDQuant CV% bins: show only thresholds and colors needed for selected bin count
  # cv_bin_1 through cv_bin_5 (thresholds) and cv_bin_1_color through cv_bin_6_color
  if (grepl("^cv_bin_[1-5]$", fname)) {
    ctrl <- res_field_input_id(node_id, "num_bins")
    bin_index <- suppressWarnings(as.integer(sub("^cv_bin_", "", fname)))
    if (is.finite(bin_index)) {
      cond <- sprintf("input['%s'] >= %d", ctrl, bin_index + 1L)
    }
  }
  if (grepl("^cv_bin_[1-6]_color$", fname)) {
    ctrl <- res_field_input_id(node_id, "num_bins")
    bin_index <- suppressWarnings(as.integer(sub("^cv_bin_([1-6])_color$", "\\1", fname)))
    if (is.finite(bin_index)) {
      # Color fields are visible when num_bins >= bin_index
      cond <- sprintf("input['%s'] >= %d", ctrl, bin_index)
    }
  }

  # IDQuant overlap: count label styling only visible when labels are enabled
  if (fname %in% c("count_labels_size", "count_labels_color")) {
    ctrl <- res_field_input_id(node_id, "count_labels_show")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # IDQuant: value label size only visible when show_values is enabled
  if (fname == "value_label_size") {
    ctrl <- res_field_input_id(node_id, "show_values")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # IDQuant overlap: Venn-only controls
  if (grepl("^venn_", fname) && is.null(cond)) {
    ctrl_plot <- res_field_input_id(node_id, "overlap_plot_type")
    cond <- sprintf("input['%s'] == 'venn'", ctrl_plot)
  }

  # IDQuant overlap: UpSet-only controls
  upset_only_fields <- c("count_labels_show", "show_bar_outline", "bar_outline_color", "bar_fill_color")
  if (fname %in% upset_only_fields && is.null(cond)) {
    ctrl_plot <- res_field_input_id(node_id, "overlap_plot_type")
    cond <- sprintf("input['%s'] == 'upset'", ctrl_plot)
  }

  # PCA: Scores-only controls (visible when NOT showing scree)
  pca_scores_fields <- c("point_size", "point_alpha", "show_ellipse", "ellipse_alpha")
  if (fname %in% pca_scores_fields && is.null(cond)) {
    cond <- "input['res_pca_is_scree'] != true"
  }

  # PCA: Scree-only controls (visible when showing scree)
  if (grepl("^scree_", fname) && is.null(cond)) {
    cond <- "input['res_pca_is_scree'] == true"
  }

  # Hor_dis: mean_type only visible when show_mean is true
  # Vert_dis: mean_type only visible when show_global_mean is true
  if (fname == "mean_type") {
    # Try show_mean first (hor_dis), fallback to show_global_mean (vert_dis)
    ctrl_show_mean <- res_field_input_id(node_id, "show_mean")
    ctrl_show_global_mean <- res_field_input_id(node_id, "show_global_mean")
    # Use OR condition: either show_mean or show_global_mean triggers visibility
    cond <- sprintf("input['%s'] == true || input['%s'] == true", ctrl_show_mean, ctrl_show_global_mean)
  }

  # 2DGOFCS: Reference line size only visible when show_ref_lines is enabled
  if (fname == "ref_line_size") {
    ctrl <- res_field_input_id(node_id, "show_ref_lines")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # 2DGOFCS: Diagonal guide size only visible when show_diagonal_guides is enabled
  if (fname == "diagonal_guide_size") {
    ctrl <- res_field_input_id(node_id, "show_diagonal_guides")
    cond <- sprintf("input['%s'] == true", ctrl)
  }

  # Hor_dis/Vert_dis: selected_group visibility is handled server-side (compare_mode is a params field, not a style input)
  # The selected_group_ui is only built when compare_mode == "within_groups" - see style panel renderUI

  if (!is.null(cond)) return(conditionalPanel(cond, ui))
  ui
}

res_field_ui <- function(node_id, field, value_override = NULL, dynamic_choices = NULL) {
  ui <- res_field_ui_core(node_id, field, value_override = value_override, dynamic_choices = dynamic_choices)
  res_wrap_conditionals(node_id, field, ui)
}

# ---- UI ----------------------------------------------------------------------
page_results_ui <- function() {
  tagList(
    tags$head(
      # FIX: JavaScript handler for modal close event (cancels UniProt fetch)
      tags$script(HTML("
        Shiny.addCustomMessageHandler('go_gene_modal_init', function(message) {
          // Bind modal close event to notify server
          $(document).off('hidden.bs.modal.go_gene').on('hidden.bs.modal.go_gene', '.modal', function() {
            Shiny.setInputValue('go_gene_modal_closed', Math.random());
          });
        });
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('copy_plot', function(payload) {
          var plotId = payload && payload.id ? payload.id : null;
          if (!plotId) return;

          var container = document.getElementById(plotId);
          var img = container ? container.querySelector('img') : null;

          function showNotification(msg, type) {
            // Use Shiny's notification system
            Shiny.notifications.show({
              html: '<span>' + msg + '</span>',
              type: type,
              duration: 3000
            });
          }

          if (!img || !img.src) {
            showNotification('Plot not ready yet. Try again.', 'error');
            return;
          }

          if (!navigator.clipboard || !window.ClipboardItem) {
            showNotification('Clipboard API not available in this browser.', 'error');
            return;
          }

          fetch(img.src)
            .then(function(res) { return res.blob(); })
            .then(function(blob) {
              var item = new ClipboardItem({ [blob.type]: blob });
              return navigator.clipboard.write([item]);
            })
            .then(function() {
              var w = img.naturalWidth || 0;
              var h = img.naturalHeight || 0;
              var label = (w && h) ? ('Copied plot (' + w + ' x ' + h + ' px).') : 'Copied plot to clipboard.';
              showNotification(label, 'message');
            })
            .catch(function(err) {
              showNotification('Copy failed. Check browser permissions.', 'error');
              if (window.console && console.error) console.error(err);
            });
        });
      ")),
      tags$script(HTML("
        (function() {
          function disableDtErrors() {
            if (window.jQuery && jQuery.fn && jQuery.fn.dataTable) {
              jQuery.fn.dataTable.ext.errMode = 'none';
            }
          }
          if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', disableDtErrors);
          } else {
            disableDtErrors();
          }
          if (window.jQuery) {
            jQuery(document).on('preInit.dt', function() { disableDtErrors(); });
          }
        })();
      ")),
      tags$script(HTML("
        // Collapsible section toggle handler - preserves state across re-renders
        $(document).on('click', '.collapse-header', function(e) {
          e.stopPropagation();
          var $section = $(this).closest('.collapse-section');
          $section.toggleClass('open');

          // Report accordion state to Shiny for persistence
          var sectionId = $section.attr('id');
          var isOpen = $section.hasClass('open');
          if (sectionId && Shiny.setInputValue) {
            Shiny.setInputValue('res_accordion_state', {id: sectionId, open: isOpen}, {priority: 'event'});
          }
        });
      ")),
      tags$style(HTML("
        .msterp-content.msterp-content-results {
          overflow: hidden !important;
          padding: 0 !important;
        }
        .msterp-content.msterp-content-results > #page_ui {
          height: 100% !important;
          min-height: 0 !important;
          overflow: hidden !important;
        }

        /* Box sizing so padding doesn't create scrollbars */
        .res-root, .res-root * { box-sizing: border-box; }

        /* Root fills shell content pane */
        .res-root {
          height: 100%;
          min-height: 0;
          padding: 16px;
          overflow: hidden;
          background: #ffffff;
        }

        /* Editable GO terms table: keep long term names usable */
        .res-editable-go-table input[type=\"text\"]{
          min-width: 260px;
          max-width: 100%;
        }
        .res-editable-go-table .form-control{
          padding: 4px 6px;
          height: 28px;
        }
        .res-editable-go-table td{ vertical-align: middle; }

        /* GO table Search button styling - prevent spill/overflow */
        .go-search-genes-btn {
          padding: 2px 6px !important;
          font-size: 11px !important;
          line-height: 1.2 !important;
          white-space: nowrap !important;
          min-width: auto !important;
          width: auto !important;
        }
        /* Ensure Search column cells don't expand unnecessarily */
        .dataTables_wrapper td:has(.go-search-genes-btn) {
          width: 40px !important;
          min-width: 40px !important;
          max-width: 50px !important;
          text-align: center !important;
          padding: 4px !important;
        }

                /* Shiny outputs inside flex need explicit flex:1 or they shrink-to-content */
        #res_root{
          height: 100%;
          min-height: 0;
          width: 100%;
          min-width: 0;
        }

        /* uiOutput(res_main) is the direct child of .res-center */
        #res_main{
          height: 100%;
          min-height: 0;
          flex: 1 1 0;
          width: 0;        /* key: prevents min-content shrink */
          min-width: 0;
        }

        /* Extra safety: any shiny-html-output inside center should behave the same */
        .res-center > .shiny-html-output{
          flex: 1 1 0;
          width: 0;
          min-width: 0;
          min-height: 0;
        }

        /* Ensure your returned UI fills the output div */
        .res-center-stack{ width: 100%; min-width: 0; }


        /* Layout: 3 columns, never page-scroll */
        .res-layout {
          height: 100%;
          display: flex;
          gap: 12px;
          align-items: stretch;
          overflow: hidden;
          min-height: 0;
          min-width: 0;
        }

        /* Card panels */
        .res-panel {
          background: var(--md-card-bg);
          border: 1px solid var(--md-card-border);
          border-radius: 14px;
          overflow: hidden;
          display: flex;
          flex-direction: column;
          min-height: 0;
          min-width: 0;
          box-shadow: var(--md-card-shadow);
        }

        .res-panel-head { height: 10px; background: var(--primary, #c9414d); flex: 0 0 auto; }

        /* === Collapsible Style Sections === */
        .collapse-section {
          border: 1px solid var(--border-light, #e8e4df);
          border-radius: 8px;
          margin-bottom: 8px;
          overflow: hidden;
          background: var(--bg-card, #ffffff);
        }
        .collapse-header {
          display: flex;
          align-items: center;
          justify-content: space-between;
          padding: 10px 12px;
          background: var(--bg-muted, #f5f3f0);
          cursor: pointer;
          user-select: none;
          transition: background var(--duration-fast, 150ms) ease;
        }
        .collapse-header:hover {
          background: var(--bg-hover, #f0eeeb);
        }
        .collapse-title {
          display: flex;
          align-items: center;
          gap: 8px;
          font-weight: 700;
          font-size: 13px;
          color: var(--text-primary, #1a1a1a);
        }
        .collapse-icon {
          display: inline-block;
          font-size: 10px;
          color: var(--text-secondary, #5a5a5a);
          transition: transform var(--duration-fast, 150ms) ease;
        }
        .collapse-section.open .collapse-icon {
          transform: rotate(90deg);
        }
        .collapse-badge {
          font-size: 11px;
          font-weight: 600;
          color: var(--text-muted, #8a8a8a);
          background: var(--bg-card, #ffffff);
          padding: 2px 8px;
          border-radius: 999px;
          border: 1px solid var(--border-light, #e8e4df);
        }
        .collapse-body {
          max-height: 0;
          overflow: hidden;
          transition: max-height var(--duration-normal, 250ms) ease, padding var(--duration-normal, 250ms) ease;
          padding: 0 12px;
        }
        .collapse-section.open .collapse-body {
          max-height: 800px;
          padding: 12px;
        }

        .res-panel-body {
          flex: 1 1 auto;
          min-height: 0;
          min-width: 0;
          overflow: hidden;
          display: flex;
          flex-direction: column;
        }

        /* Gate (centered) */
        .res-gate {
          height: 100%;
          width: 100%;
        }
        .res-gate-inner {
          display: flex;
          flex-direction: column;
          gap: 10px;
        }
        .res-gate-inner .btn { width: 100%; font-weight: 700; }

        .res-left  { flex: 0 1 clamp(240px, 24vw, 340px); width: clamp(240px, 24vw, 340px); }

        /* Critical: allow center to take ALL remaining space, not min-content */
        .res-center{
          flex: 1 1 0;
          width: 0;           /* key: prevents intrinsic sizing from narrowing center */
          min-width: 0;
          min-height: 0;
          display: flex;
          overflow: hidden;
        }

        .res-right { flex: 0 1 clamp(240px, 24vw, 340px); width: clamp(240px, 24vw, 340px); }

        .res-left-top {
          flex: 0 0 52px;
          min-height: 52px;
          padding: 10px;
          display: flex;
          gap: 10px;
          align-items: center;
          justify-content: flex-start; /* button stays left */
          overflow: hidden;
        }
        .res-left-title {
          font-weight: 800;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        /* Status indicator light */
        .res-status-light {
          width: 12px;
          height: 12px;
          border-radius: 50%;
          border: 2px solid #666;
          transition: all 0.3s ease;
        }
        .res-status-light.saved {
          background-color: #28a745;
          box-shadow: 0 0 8px rgba(40, 167, 69, 0.6);
        }
        .res-status-light.unsaved {
          background-color: #dc3545;
          box-shadow: 0 0 8px rgba(220, 53, 69, 0.6);
          animation: pulse-red 2s infinite;
        }
        .res-status-light.saving {
          background-color: #ffc107;
          box-shadow: 0 0 8px rgba(255, 193, 7, 0.6);
          animation: pulse-yellow 1s infinite;
        }
        .res-status-light.failed {
          background-color: #dc3545;
          box-shadow: 0 0 12px rgba(220, 53, 69, 0.8);
          border-color: #721c24;
        }
        @keyframes pulse-red {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.5; }
        }
        @keyframes pulse-yellow {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.6; }
        }

        .res-nav-list { flex: 1 1 auto; padding: 10px; overflow: auto; }

        /* Tree buttons */
        .res-node-btn {
          border: none !important;
          border-radius: 10px !important;
          margin-top: 6px !important;
          margin-bottom: 6px !important;
          text-align: left;
          color: #fff !important;
          font-weight: 800 !important;
          padding: 10px 12px !important;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
          transition: transform var(--duration-fast, 150ms) ease, box-shadow var(--duration-fast, 150ms) ease;
        }
        .res-node-btn:hover {
          transform: translateX(2px);
        }
        .res-node-depth-0 { background: var(--charcoal, #3d3d3d) !important; }
        .res-node-depth-1 { background: var(--primary, #c9414d) !important; }
        .res-node-depth-2, .res-node-depth-3, .res-node-depth-4 { background: var(--accent-gold, #d4a84b) !important; }
        .res-node-active {
          box-shadow: 0 0 0 2px var(--bg-card, #ffffff), 0 0 0 4px var(--text-primary, #1a1a1a);
        }
        .res-node-locked { pointer-events: none; opacity: 0.55 !important; filter: grayscale(100%); }
        .res-node-locked:hover, .res-node-locked:focus { box-shadow: none !important; outline: none !important; transform: none; }
        .res-step-num { display: inline-block; min-width: 52px; font-weight: 700; }
        .res-step-text { font-weight: 700; }

        /* RIGHT column stack */
        .res-right {
          min-height: 0;
          min-width: 0;
          display: flex;
          flex-direction: column;
          gap: 12px;
          overflow: hidden;
        }
        
        .res-right-download { flex: 0 0 140px; min-height: 140px; }
        .res-right-download .res-panel-body { padding: 10px; overflow: auto; display: flex; flex-direction: column; gap: 10px; }

        .res-download-wrap {
          width: 100%;
          display:flex;
          align-items:center;
          justify-content:center;
        }
        .res-download-buttons {
          width: 100%;
          display:flex;
          gap: 8px;
        }
        .res-download-buttons .btn {
          flex: 1 1 0;
          font-weight: 700;
          white-space: nowrap;
        }
        .res-copy-status {
          font-size: 11px;
          font-weight: 600;
          color: #444;
        }
        
        .res-right-mini { flex: 0 0 auto; min-height: 60px; max-height: 140px; }
        .res-right-mini .res-panel-body { padding: 8px 10px; overflow: visible; }
        .res-right-mini .shiny-input-container { margin-bottom: 6px; }
        .res-right-mini .form-group { margin-bottom: 6px; }
        .res-right-mini .selectize-input { min-height: 28px; padding: 4px 8px; font-size: 12px; }
        .res-right-mini .selectize-dropdown { font-size: 12px; }
        .res-right-mini .res-switch-row { margin: 4px 0; }
        .res-right-mini label { font-size: 11px; margin-bottom: 2px; }
        .res-right-style { flex: 1 1 auto; min-height: 0; }
        .res-style-controls { flex: 1 1 auto; min-height: 0; padding: 10px; overflow: auto; }
        
        /* --- Toggle switch ------------------------------------------------ */
        .res-switch-row{
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:10px;
          width:100%;
        }
        .res-switch-left,.res-switch-right{
          font-weight:600;
          font-size:14px;
          white-space:nowrap;
        }

        .res-switch{
          position:relative;
          width:54px;
          height:28px;
          margin:0;
          flex:0 0 auto;
          cursor:pointer;
        }
        .res-switch input{
          position:absolute;
          inset:0;
          width:100%;
          height:100%;
          opacity:0;
          margin:0;
          cursor:pointer;
        }
        .res-switch-track{
          position:absolute;
          inset:0;
          border-radius:999px;
          background:#e5e5ea;
          border:1px solid #d6d6dd;
        }
        .res-switch-knob{
          position:absolute;
          top:3px;
          left:3px;
          width:22px;
          height:22px;
          border-radius:999px;
          background:#ffffff;
          border:1px solid #cfcfd6;
          transition:transform 0.15s ease;
        }
        .res-switch input:checked ~ .res-switch-track{
          background:#E03A3E;
          border-color:#E03A3E;
        }
        .res-switch input:checked ~ .res-switch-knob{
          transform:translateX(26px);
        }

        /* Compact DPI toggle in left sidebar */
        .res-left-dpi { padding: 10px 10px 0 10px; }
        .res-left-dpi-title { font-weight: 700; font-size: 12px; margin-bottom: 4px; }
        .res-left-dpi .res-switch-left,.res-left-dpi .res-switch-right { font-size: 12px; }
        
        .res-center-stack{
          height: 100%;
          flex: 1 1 0;
          min-height: 0;
          min-width: 0;  /* add */
          display: flex;
          flex-direction: column;
          gap: 12px;
          overflow: hidden;
        }

        /* Overview 60/40 layout fills center panel */
        .res-overview-layout {
          width: 100%;
          height: 100%;
          flex: 1 1 0;
          min-height: 0;
          min-width: 0;
        }
        
        .res-plot-panel  { flex: 1 1 auto; min-height: 0; }
        .res-table-panel { flex: 0 0 clamp(220px, 30%, 360px); min-height: 0; }
        .res-only-panel  { flex: 1 1 auto; min-height: 0; }

        @media (max-width: 1200px) {
          .res-root { overflow: auto; }
          .res-layout {
            flex-direction: column;
            height: auto;
            overflow: visible;
          }
          .res-left,
          .res-right,
          .res-center {
            width: 100%;
            flex: 0 0 auto;
          }
          .res-center { min-height: 420px; }
        }

        .res-plot-controls {
          flex: 0 0 auto;
          padding: 10px;
          display: flex;
          gap: 12px;
          align-items: center;
          justify-content: space-between;
          overflow: visible;
        }
        .res-plot-controls-left  { flex: 1 1 auto; min-width: 0; max-width: 300px; }
        .res-plot-controls-right { flex: 0 0 auto; }
        .res-plot-controls .form-group { margin: 0 !important; }
        .res-plot-controls .btn { font-weight: 700; white-space: nowrap; }
        .res-plot-controls .selectize-dropdown { z-index: 1000; }
        .res-plot-controls .selectize-input { min-height: 34px; }

        .res-plot-stage {
          flex: 1 1 auto;
          min-height: 0;
          padding: 10px;
          overflow: hidden;
          display: flex;
          align-items: center;
          justify-content: center;
        }

        .res-plot-box {
          width: 100%;
          max-width: 100%;
          max-height: 100%;
          aspect-ratio: var(--res-plot-ar, 7/5);
          container-type: size;
          overflow: hidden;
          position: relative;
        }
        .res-plot-box.res-plot-box-free {
          aspect-ratio: auto;
          height: 100%;
        }

        .res-hover-card {
          position: absolute;
          z-index: 60;
          pointer-events: none;
          background: rgba(255, 255, 255, 0.96);
          border: 2px solid #000;
          border-radius: 8px;
          padding: 9px 12px;
          font-size: 16px;
          line-height: 1.5;
          box-shadow: 0 6px 18px rgba(0, 0, 0, 0.12);
          max-width: 240px;
        }
        .res-hover-title {
          font-weight: 700;
          margin-bottom: 2px;
        }
        .res-uniprot-summary {
          font-size: 13px;
          line-height: 1.5;
          white-space: pre-line;
          max-height: 400px;
          overflow-y: auto;
          color: #222;
        }
        .res-uniprot-function {
          font-size: 12px;
          line-height: 1.4;
          color: #333;
          max-width: 520px;
        }

        .res-mode-badge {
          position: absolute;
          top: 8px;
          right: 8px;
          z-index: 50;
          pointer-events: none;
          padding: 4px 8px;
          border-radius: 999px;
          font-size: 11px;
          font-weight: 700;
          letter-spacing: 0.3px;
          border: 1px solid rgba(0,0,0,0.15);
          backdrop-filter: blur(6px);
        }
        .res-mode-badge-interactive { background: rgba(232, 250, 240, 0.85); color: #176B3A; }
        .res-mode-badge-publication { background: rgba(242, 246, 255, 0.85); color: #1E3A8A; }

        /* Ensure plot fills available space and scales correctly */
        @supports (container-type: size) {
          .res-plot-box {
            height: auto;
          }
        }

        /* Fallback for browsers without container queries */
        @supports not (container-type: size) {
          .res-plot-box {
            height: 0;
            padding-bottom: calc(100% / var(--res-plot-ar, 1.4));
            position: relative;
          }
          .res-plot-box > * {
            position: absolute;
            top: 0;
            left: 0;
            width: 100%;
            height: 100%;
          }
        }

        .res-table-stage { flex: 1 1 auto; min-height: 0; padding: 10px; overflow: auto; }
        /* Ensure DT tables respect container height and show scrollbars */
        .res-table-stage .dataTables_wrapper {
          display: flex;
          flex-direction: column;
          height: 100%;
          max-height: 100%;
        }
        .res-table-stage .dataTables_scroll {
          flex: 1 1 auto;
          min-height: 0;
          overflow: auto;
        }
        .res-table-stage .dataTables_scrollBody {
          max-height: none !important;
          overflow-y: auto !important;
        }
        /* Also handle plain shiny tables */
        .res-table-stage .shiny-html-output {
          max-height: 100%;
          overflow: auto;
        }

        .res-plot-box .shiny-bound-output,
        .res-plot-box .shiny-plot-output,
        .res-plot-box .html-widget {
          width: 100% !important;
          height: 100% !important;
          min-height: 0 !important;
        }

        .res-plot-box .shiny-plot-output img {
          width: 100% !important;
          height: 100% !important;
          object-fit: contain;
          object-position: center;
        }

        .res-plot-interact {
          width: 100%;
          height: 100%;
          outline: none;
        }
        .res-plot-interact:focus {
          outline: none;
        }
        /* Plotly interactive wrapper */
        .res-plotly-wrap {
          min-height: 500px;
        }
        .res-plotly-wrap .plotly {
          width: 100% !important;
          height: 100% !important;
        }

        .res-root .control-label,
        .res-root label,
        .res-root h4 { font-weight: 800; }
        .res-root .btn { font-weight: 700; }
        .res-root .form-control { font-weight: 600; }

        /* === GO enrichment horizontal tabs (like Format files page) === */
        .res-panel-body > .nav-tabs,
        .res-panel-body > .tabbable > .nav-tabs {
          display: inline-flex !important;
          flex-direction: row !important;
          flex-wrap: nowrap !important;
          gap: 4px;
          padding: 4px;
          background: #f0ebe5;
          border-radius: 10px;
          border: 1px solid var(--md-border);
          border-bottom: 1px solid var(--md-border);
          width: auto;
          margin: 0 auto 14px auto;
        }

        .res-panel-body > .nav-tabs > .nav-item,
        .res-panel-body > .tabbable > .nav-tabs > .nav-item {
          flex: 0 0 auto;
          display: inline-block;
        }

        .res-panel-body > .nav-tabs > .nav-item > .nav-link,
        .res-panel-body > .tabbable > .nav-tabs > .nav-item > .nav-link {
          padding: 10px 28px;
          font-size: 14px;
          font-weight: 700;
          color: #4a4a4a;
          background: transparent;
          border: none;
          border-radius: 8px;
          cursor: pointer;
          transition: all 0.15s ease;
          white-space: nowrap;
        }

        .res-panel-body > .nav-tabs > .nav-item > .nav-link:hover,
        .res-panel-body > .tabbable > .nav-tabs > .nav-item > .nav-link:hover {
          background: #ffffff;
          color: #d50032;
        }

        .res-panel-body > .nav-tabs > .nav-item > .nav-link.active,
        .res-panel-body > .tabbable > .nav-tabs > .nav-item > .nav-link.active {
          background: #ffffff;
          color: #d50032;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        }

        /* Center the tab bar within its container */
        .res-panel-body > .tabbable {
          display: flex;
          flex-direction: column;
          align-items: center;
          height: 100%;
        }

        .res-panel-body > .tabbable > .tab-content {
          flex: 1 1 auto;
          width: 100%;
          min-height: 0;
          overflow: auto;
        }
      "))
    ),
    tags$script(HTML("
      (function() {
        function sync() {
          var hasResults = document.querySelector('.res-root') !== null;
          document.querySelectorAll('.msterp-content').forEach(function(el) {
            if (hasResults) el.classList.add('msterp-content-results');
            else el.classList.remove('msterp-content-results');
          });
        }

        sync();

        var target = document.getElementById('page_ui');
        if (target && !target.__resObserver) {
          var obs = new MutationObserver(function() { sync(); });
          obs.observe(target, { childList: true, subtree: true });
          target.__resObserver = obs;
        }
                // Make numeric/text areas push updates immediately (numericInput often updates on blur)
        if (window.Shiny && !window.__msterp_res_immediate_inputs) {
          window.__msterp_res_immediate_inputs = true;

          document.addEventListener('input', function(e) {
            var el = e.target;
            if (!el || !el.id) return;
            if (!el.closest || !el.closest('.res-root')) return;

            var tag = (el.tagName || '').toLowerCase();
            var type = (el.getAttribute('type') || '').toLowerCase();

            if (type === 'number') {
              var v = el.value;
              if (v === '') {
                Shiny.setInputValue(el.id, null, {priority:'event'});
              } else {
                var num = parseFloat(v);
                Shiny.setInputValue(el.id, (isFinite(num) ? num : null), {priority:'event'});
              }
              return;
            }

            if (tag === 'textarea') {
              Shiny.setInputValue(el.id, el.value, {priority:'event'});
              return;
            }
          }, true);
        }

        // Label editor keyboard shortcuts for volcano/2dgofcs plots
        if (window.Shiny && !window.__msterp_label_keys) {
          window.__msterp_label_keys = true;

          document.addEventListener('keydown', function(e) {
            var wrap = e.target && e.target.closest ? e.target.closest('.res-plot-interact') : null;
            if (!wrap) return;

            if (e.key === 'Delete' || e.key === 'Backspace' || e.key === 'Escape') {
              e.preventDefault();
              Shiny.setInputValue('res_label_key', { key: e.key, ts: Date.now() }, { priority: 'event' });
            }
          }, true);

          document.addEventListener('click', function(e) {
            var wrap = e.target && e.target.closest ? e.target.closest('.res-plot-interact') : null;
            if (wrap) {
              try { wrap.focus(); } catch (err) {}
            }
          }, true);
        }

        // Keyboard navigation: Arrow keys to navigate between steps
        if (window.Shiny && !window.__msterp_keyboard_nav) {
          window.__msterp_keyboard_nav = true;
          document.addEventListener('keydown', function(e) {
            // Only activate if focus is NOT in an input/textarea
            if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA' || e.target.tagName === 'SELECT') {
              return;
            }

            var activeBtn = document.querySelector('.res-node-btn.res-node-active');
            if (!activeBtn) return;

            var allBtns = Array.from(document.querySelectorAll('.res-node-btn'));
            var currentIdx = allBtns.indexOf(activeBtn);
            if (currentIdx === -1) return;

            var nextIdx = -1;
            if (e.key === 'ArrowDown' || e.key === 'j') {
              nextIdx = currentIdx + 1;
              e.preventDefault();
            } else if (e.key === 'ArrowUp' || e.key === 'k') {
              nextIdx = currentIdx - 1;
              e.preventDefault();
            }

            if (nextIdx >= 0 && nextIdx < allBtns.length) {
              // Skip disabled (locked) buttons
              var targetBtn = allBtns[nextIdx];
              if (targetBtn.style.cursor === 'not-allowed') {
                // Try next in direction
                if (e.key === 'ArrowDown' || e.key === 'j') {
                  nextIdx++;
                } else {
                  nextIdx--;
                }
                if (nextIdx >= 0 && nextIdx < allBtns.length) {
                  targetBtn = allBtns[nextIdx];
                } else {
                  return;
                }
              }

              if (targetBtn && targetBtn.style.cursor !== 'not-allowed') {
                targetBtn.click();
                targetBtn.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
              }
            }
          });
        }

        // Copy to clipboard functionality
        if (window.Shiny && !window.__msterp_clipboard_handler) {
          window.__msterp_clipboard_handler = true;

          // Handle copy button clicks for all three button types
          document.addEventListener('click', function(e) {
            var btn = e.target.closest('#copy_genes_btn, #copy_genes_btn_tab');
            if (!btn) return;

            // Determine which textarea to copy from
            var textareaId = 'gene_list_text';
            var feedbackId = 'copy_feedback';

            if (btn.id === 'copy_genes_btn_tab') {
              textareaId = 'gene_list_text_tab';
              feedbackId = 'copy_feedback_tab';
            }

            var textarea = document.getElementById(textareaId);
            var feedback = document.getElementById(feedbackId);

            if (!textarea) return;

            // Select and copy text
            textarea.style.position = 'static';
            textarea.style.left = '0';
            textarea.select();

            try {
              document.execCommand('copy');

              // Show feedback
              if (feedback) {
                feedback.style.display = 'inline';
                setTimeout(function() {
                  feedback.style.display = 'none';
                }, 2000);
              }
            } catch (err) {
              console.error('Copy failed:', err);
            } finally {
              // Hide textarea again
              textarea.style.position = 'absolute';
              textarea.style.left = '-9999px';
              window.getSelection().removeAllRanges();
            }
          });
        }


        // Preserve scroll position when sidebar nodes re-render
        if (window.Shiny && !window.__msterp_scroll_preserve) {
          window.__msterp_scroll_preserve = true;
          var savedScrollTop = 0;
          
          // Save scroll position before Shiny updates the sidebar
          $(document).on('shiny:outputinvalidated', function(e) {
            if (e.name === 'res_sidebar_nodes') {
              var navList = document.querySelector('.res-nav-list');
              if (navList) {
                savedScrollTop = navList.scrollTop;
              }
            }
          });
          
          // Restore scroll position after Shiny updates the sidebar
          $(document).on('shiny:value', function(e) {
            if (e.name === 'res_sidebar_nodes') {
              setTimeout(function() {
                var navList = document.querySelector('.res-nav-list');
                if (navList && savedScrollTop > 0) {
                  navList.scrollTop = savedScrollTop;
                }
              }, 0);
            }
          });
        }

        // Handle per-term visibility toggle checkbox clicks in GO enrichment tables
        // checked = visible on plot, unchecked = hidden from plot (but still in table)
        if (window.Shiny && !window.__msterp_go_visibility_cb) {
          window.__msterp_go_visibility_cb = true;
          document.addEventListener('change', function(e) {
            var cb = e.target;
            if (!cb || !cb.classList.contains('go-term-visibility-cb')) return;
            var term = cb.getAttribute('data-term');
            if (!term) return;
            // Send term and visibility state (checked = visible)
            Shiny.setInputValue('go_term_visibility_toggle', {term: term, visible: cb.checked, ts: Date.now()}, {priority: 'event'});
          });
        }

        // Handle subloc bin show/hide checkbox toggles
        if (window.Shiny && !window.__msterp_subloc_bin_toggle) {
          window.__msterp_subloc_bin_toggle = true;
          document.addEventListener('change', function(e) {
            var cb = e.target;
            if (!cb || !cb.classList.contains('subloc-bin-toggle-cb')) return;
            var bin = cb.getAttribute('data-bin');
            if (!bin) return;
            // Send both bin name and current checked state (checked = visible)
            Shiny.setInputValue('subloc_bin_toggle', {bin: bin, visible: cb.checked, ts: Date.now()}, {priority: 'event'});
          });
        }

        // Handle copy gene list button clicks in GO enrichment tables
        // Prevents copy action when DT cell editing is active (e.g., editing term name)
        // FIX: Also tracks recent cell edits to prevent accidental copy triggers
        if (window.Shiny && !window.__msterp_go_copy_genes) {
          window.__msterp_go_copy_genes = true;
          window.__msterp_last_cell_edit_ts = 0;  // Track last cell edit timestamp

          // Listen for DT cell edit starts to set debounce
          document.addEventListener('dblclick', function(e) {
            if (e.target.closest('.dataTables_wrapper td')) {
              window.__msterp_last_cell_edit_ts = Date.now();
            }
          });

          document.addEventListener('click', function(e) {
            // FIX: Only trigger on direct button clicks, not on row clicks
            var btn = e.target.closest('.go-copy-genes-btn');
            if (!btn) return;

            // Verify click was on button or its icon child, not a parent element
            var clickTarget = e.target;
            var isDirectClick = (clickTarget === btn || clickTarget.parentNode === btn ||
                                 (clickTarget.tagName === 'I' && clickTarget.parentNode === btn));
            if (!isDirectClick) return;

            // Prevent copy if click originates from within a DataTable cell edit input
            // or if a DT cell editor is currently active in the document
            // FIX: Also check for recent cell edit activity (300ms debounce)
            if (e.target.closest('input.form-control') ||
                e.target.tagName === 'INPUT' ||
                document.querySelector('.dataTables_wrapper input.form-control:focus') ||
                document.querySelector('.dataTables_wrapper input.form-control') ||
                document.querySelector('.DTE_Inline') ||
                (Date.now() - (window.__msterp_last_cell_edit_ts || 0)) < 300) {
              e.stopPropagation();
              e.preventDefault();
              return;
            }

            var genes = btn.getAttribute('data-genes');
            if (!genes) return;
            var txt = document.createElement('textarea');
            txt.innerHTML = genes;
            var decodedGenes = txt.value;
            function onCopied() {
              var origTitle = btn.getAttribute('title');
              btn.setAttribute('title', 'Copied!');
              btn.classList.add('btn-success');
              btn.classList.remove('btn-default');
              setTimeout(function() {
                btn.setAttribute('title', origTitle);
                btn.classList.remove('btn-success');
                btn.classList.add('btn-default');
              }, 2000);
            }

            function fallbackCopy(text) {
              var ta = document.createElement('textarea');
              ta.value = text;
              ta.style.position = 'fixed';
              ta.style.left = '-9999px';
              document.body.appendChild(ta);
              ta.select();
              try {
                document.execCommand('copy');
                onCopied();
              } catch (err) {
                console.error('Copy failed:', err);
              } finally {
                document.body.removeChild(ta);
                if (window.getSelection) window.getSelection().removeAllRanges();
              }
            }

            if (navigator.clipboard && navigator.clipboard.writeText) {
              navigator.clipboard.writeText(decodedGenes).then(onCopied).catch(function() {
                fallbackCopy(decodedGenes);
              });
            } else {
              fallbackCopy(decodedGenes);
            }
          });
        }

        // Handle search gene button clicks in GO enrichment tables
        // Sends term and genes to Shiny for UniProt lookup
        // FIX: Only trigger when clicking directly on the button, not surrounding cell
        if (window.Shiny && !window.__msterp_go_search_genes) {
          window.__msterp_go_search_genes = true;
          document.addEventListener('click', function(e) {
            // Only trigger if click is directly on button or its icon child
            var btn = e.target.closest('.go-search-genes-btn');
            if (!btn) return;

            // FIX: Verify the click target is actually the button or its direct children (icon)
            // This prevents triggering when clicking on the cell padding around the button
            if (e.target !== btn && !btn.contains(e.target)) return;

            // Prevent search if click originates from within a DataTable cell edit input
            // FIX: Also check for recent cell edit activity (300ms debounce)
            if (e.target.closest('input.form-control') ||
                e.target.tagName === 'INPUT' ||
                document.querySelector('.dataTables_wrapper input.form-control:focus') ||
                document.querySelector('.dataTables_wrapper input.form-control') ||
                document.querySelector('.DTE_Inline') ||
                (Date.now() - (window.__msterp_last_cell_edit_ts || 0)) < 300) {
              e.stopPropagation();
              e.preventDefault();
              return;
            }

            // Stop propagation to prevent other click handlers from firing
            e.stopPropagation();

            var term = btn.getAttribute('data-term');
            var genes = btn.getAttribute('data-genes');
            if (!term || !genes) return;

            // Decode HTML entities
            var txtTerm = document.createElement('textarea');
            txtTerm.innerHTML = term;
            var decodedTerm = txtTerm.value;

            var txtGenes = document.createElement('textarea');
            txtGenes.innerHTML = genes;
            var decodedGenes = txtGenes.value;

            // Send to Shiny for UniProt lookup
            Shiny.setInputValue('go_search_genes_click', {
              term: decodedTerm,
              genes: decodedGenes,
              ts: Date.now()
            }, {priority: 'event'});
          });
        }

        // UniProt summary click handler (opens full summary modal)
        if (window.Shiny && !window.__msterp_go_uniprot_summary) {
          window.__msterp_go_uniprot_summary = true;
          document.addEventListener('click', function(e) {
            var link = e.target.closest('.go-uniprot-summary-link');
            if (!link) return;

            // Avoid firing while a DT cell editor is active
            if (e.target.closest('input.form-control') ||
                e.target.tagName === 'INPUT' ||
                document.querySelector('.dataTables_wrapper input.form-control:focus') ||
                document.querySelector('.DTE_Inline') ||
                (Date.now() - (window.__msterp_last_cell_edit_ts || 0)) < 300) {
              e.stopPropagation();
              e.preventDefault();
              return;
            }

            var term = link.getAttribute('data-term') || '';
            var gene = link.getAttribute('data-gene') || '';
            var acc  = link.getAttribute('data-acc') || '';

            // Decode HTML entities
            var txtTerm = document.createElement('textarea'); txtTerm.innerHTML = term;
            var txtGene = document.createElement('textarea'); txtGene.innerHTML = gene;
            var txtAcc  = document.createElement('textarea'); txtAcc.innerHTML = acc;

            Shiny.setInputValue('go_uniprot_summary_click', {
              term: txtTerm.value,
              gene: txtGene.value,
              acc:  txtAcc.value,
              ts: Date.now()
            }, {priority: 'event'});
          });
        }

      })();
    ")),
    div(class = "res-root", uiOutput("res_root"))
  )
}



# ---- Server ------------------------------------------------------------------

page_results_server <- function(input, output, session) {
  rv <- reactiveValues(
    exdir = NULL,
    run_root = NULL,
    manifest = NULL,
    nodes_df = NULL,
    active_node_id = NULL,
    nav_obs = list(),
    last_sparse_by_node = list(),
    loaded = FALSE,
    load_warnings = character(),
    cache_style_by_node = list(),
    cache_plotly_by_node = list(),
    cache_vis_by_node = list(),
    has_unsaved_changes = FALSE,
    save_status = "saved",  # saved, dirty, saving, failed
    save_error = NULL,  # Store error message on failure
    terpbook_filename = NULL,  # Store original uploaded filename (e.g., "example.terpbook")
    current_plot_names = character(),  # Available plot names for multi-graph selector
    current_table_names = character(),  # Available table names for multi-table selector
    enrichment_tab_selected = NULL,  # Track selected GO enrichment tab to preserve after re-render
    preview_dpi = 150L,  # Preview-only DPI (publication export uses separate download/export settings)
    label_selected = list(id = NULL, plot_key = NULL, eng = NULL),
    hover_info_by_plot = list(),
    switching_node = FALSE,  # FIX: Flag to prevent false dirty detection during node switch
    accordion_state = list()  # Track open/closed state of style accordion sections
  )
  style_rev <- reactiveVal(0L)

  # ---- Accordion state persistence -------------------------------------------
  observeEvent(input$res_accordion_state, {
    state <- input$res_accordion_state
    if (!is.null(state$id) && !is.null(state$open)) {
      rv$accordion_state[[state$id]] <- state$open
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ---- DPI observer: sync input$res_preview_dpi -> rv$preview_dpi ------------
  observeEvent(input$res_preview_dpi, {
    x <- input$res_preview_dpi

    # New UI: checkbox-style toggle -> logical
    if (isTRUE(x)) {
      rv$preview_dpi <- 300L
      return()
    }
    if (identical(x, FALSE)) {
      rv$preview_dpi <- 150L
      return()
    }

    # Backward compatibility: older UI values may still be numeric/character
    dpi <- suppressWarnings(as.integer(x))
    if (is.finite(dpi) && dpi > 0) rv$preview_dpi <- dpi
  }, ignoreNULL = TRUE, ignoreInit = FALSE)

  # ---- Multi-graph selector linking (plot <-> table) ------------------------
  .selector_sync_guard <- reactiveVal(FALSE)

  observeEvent(input$res_plot_pick, {
    if (isTRUE(.selector_sync_guard())) return()
    plot_pick <- as.character(input$res_plot_pick %||% "")
    if (!nzchar(plot_pick)) return()

    tbl_names <- rv$current_table_names %||% character()
    if (!(plot_pick %in% tbl_names)) return()

    .selector_sync_guard(TRUE)
    on.exit(.selector_sync_guard(FALSE), add = TRUE)

    updateSelectInput(session, "res_table_pick", selected = plot_pick)
  }, ignoreInit = TRUE)

  observeEvent(input$res_table_pick, {
    if (isTRUE(.selector_sync_guard())) return()
    tbl_pick <- as.character(input$res_table_pick %||% "")
    if (!nzchar(tbl_pick)) return()

    plot_names <- rv$current_plot_names %||% character()
    if (!(tbl_pick %in% plot_names)) return()

    .selector_sync_guard(TRUE)
    on.exit(.selector_sync_guard(FALSE), add = TRUE)

    updateSelectInput(session, "res_plot_pick", selected = tbl_pick)
  }, ignoreInit = TRUE)

  # ---- UniProt summary cache (per-session) -----------------------------------
  uniprot_cache <- new.env(parent = emptyenv())
  uniprot_cache$gene_to_acc <- new.env(parent = emptyenv())
  uniprot_cache$acc_to_info <- new.env(parent = emptyenv())
  uniprot_cache$last_http_time <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")

  .uniprot_throttle <- function(min_interval_s = 0.35) {
    last <- uniprot_cache$last_http_time %||% as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
    now <- Sys.time()
    dt <- as.numeric(difftime(now, last, units = "secs"))
    if (is.finite(dt) && dt >= 0 && dt < min_interval_s) Sys.sleep(min_interval_s - dt)
    uniprot_cache$last_http_time <- Sys.time()
    invisible(TRUE)
  }

  .uniprot_safe <- function(expr, default = NULL) {
    tryCatch(expr, error = function(e) default)
  }

  .uniprot_acc_cached <- function(gene_id, species) {
    gene_id <- as.character(gene_id %||% "")
    species <- as.character(species %||% "")
    if (!nzchar(gene_id)) return(NA_character_)

    key <- paste0(tolower(species), "|", gene_id)
    if (exists(key, envir = uniprot_cache$gene_to_acc, inherits = FALSE)) {
      return(get(key, envir = uniprot_cache$gene_to_acc, inherits = FALSE))
    }

    if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
      assign(key, NA_character_, envir = uniprot_cache$gene_to_acc)
      return(NA_character_)
    }

    .uniprot_throttle()
    acc <- .uniprot_safe(lookup_uniprot_acc_fallback(gene_id, species), default = NA_character_)
    assign(key, acc, envir = uniprot_cache$gene_to_acc)
    acc
  }

  .uniprot_info_cached <- function(acc) {
    acc <- toupper(trimws(as.character(acc %||% "")))
    if (!nzchar(acc) || is.na(acc)) return(list(gene = NA_character_, function_text = NA_character_))

    if (exists(acc, envir = uniprot_cache$acc_to_info, inherits = FALSE)) {
      return(get(acc, envir = uniprot_cache$acc_to_info, inherits = FALSE))
    }

    if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
      info <- list(gene = NA_character_, function_text = NA_character_)
      assign(acc, info, envir = uniprot_cache$acc_to_info)
      return(info)
    }

    .uniprot_throttle()
    info <- .uniprot_safe(fetch_uniprot_info(acc), default = list(gene = NA_character_, function_text = NA_character_))
    if (is.null(info) || !is.list(info)) info <- list(gene = NA_character_, function_text = NA_character_)
    assign(acc, info, envir = uniprot_cache$acc_to_info)
    info
  }

  .res_html_escape <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
  }

  .res_truncate <- function(x, n = 100) {
    x <- as.character(x %||% "")
    if (!nzchar(x)) return("")
    if (nchar(x) <= n) return(x)
    paste0(substr(x, 1, n), "…")
  }

  .res_first_gene_from_row <- function(row) {
    for (col in c("genes", "protein_ids", "geneID", "core_enrichment", "geneNames", "gene_symbol", "gene_id", "Genes")) {
      if (!col %in% names(row)) next
      gene_str <- as.character(row[[col]] %||% "")
      if (!nzchar(gene_str)) next
      genes <- trimws(unlist(strsplit(gene_str, "[,;|/]", perl = TRUE)))
      genes <- genes[nzchar(genes)]
      if (length(genes)) return(genes[[1]])
    }
    NA_character_
  }

  .res_uniprot_summary_cell <- function(term, gene_id, species) {
    gene_id <- as.character(gene_id %||% "")
    if (!nzchar(gene_id) || is.na(gene_id)) {
      return('<span class=\"text-muted\">No summary available</span>')
    }

    acc <- .uniprot_acc_cached(gene_id, species)
    info <- .uniprot_info_cached(acc)
    summary <- as.character(info$function_text %||% "")

    if (!nzchar(summary) || is.na(summary)) {
      return('<span class=\"text-muted\">No summary available</span>')
    }

    term_safe <- .res_html_escape(term)
    gene_safe <- .res_html_escape(gene_id)
    acc_safe  <- .res_html_escape(acc %||% "")
    txt_safe  <- .res_html_escape(.res_truncate(summary, 100))

    sprintf(
      '<a href=\"#\" class=\"go-uniprot-summary-link\" data-term=\"%s\" data-gene=\"%s\" data-acc=\"%s\" title=\"Click to expand\">%s</a>',
      term_safe, gene_safe, acc_safe, txt_safe
    )
  }

  # Create debounced reactives for label inputs to avoid re-render on each keystroke
  # Debounce the volcano label genes input (500ms delay)
  volcano_label_genes_debounced <- debounce(
    reactive({ input$res_volcano_label_genes }),
    millis = 500
  )

  # Track all input IDs for the active node
  active_input_ids <- reactiveVal(character())
  output$res_root <- renderUI({
    if (!isTRUE(rv$loaded) || is.null(rv$nodes_df) || nrow(rv$nodes_df) == 0) {
      return(
        div(
          class = "msterp-gate res-gate",
          div(
            class = "res-panel res-gate-card msterp-gate-card",
            div(class = "res-panel-head"),
            div(
              class = "res-panel-body res-gate-inner msterp-gate-body",
              div(class = "msterp-gate-title", "Load Results"),
              div(class = "msterp-gate-help", "Upload a .terpbook or .zip to view results."),
              div(
                class = "msterp-gate-actions",
                fileInput("res_terpbook_upload", label = NULL, accept = c(".terpbook", ".zip")),
                actionButton("res_load_upload", "Load")
              )
            )
          )
        )
      )
    }
    
    eng <- active_engine_id()
    # Hide style panel for Overview (has its own layout) and DataProcessor (table-only)
    is_overview <- identical(rv$active_node_id, "overview")
    show_style <- !is_overview && !identical(tolower(eng %||% ""), "dataprocessor")
    # Only PCA needs the mode panel (scores/scree toggle) - volcano/2dgofcs controls moved to style panel
    show_plot_mode_panel <- identical(tolower(eng %||% ""), "pca")
    
    div(
      class = "res-layout",
      
      # LEFT: Steps panel (no minimize/collapse)
      div(
        class = "res-panel res-left",
        div(class = "res-panel-head"),
        div(
          class = "res-panel-body",
          div(
            class = "res-left-top",
            actionButton("res_return_gate", "Return"),
            div(
              class = "res-left-title",
              uiOutput("res_status_indicator")
            )
          ),
          div(
            class = "res-left-dpi",
            div(class = "res-left-dpi-title", "Preview DPI"),
            res_switch_input(
              "res_preview_dpi",
              left_label = "150",
              right_label = "300",
              value = isTRUE(as.integer(rv$preview_dpi %||% 150L) >= 300L),
              help_title = "Preview-only DPI (150 vs 300)."
            )
          ),
          # Axis style is now per-engine in style panel (see registry.R mk_style hidden=FALSE)
          div(class = "res-nav-list", uiOutput("res_sidebar_nodes"))
        )
      ),
      
      # CENTER
      div(
        class = "res-center",
        uiOutput("res_main")
      ),
      
      # RIGHT: stacked panels (mode panel + style panel)
      if (isTRUE(show_style)) {
        div(
          class = "res-right",
          
          # Panel 1: Plot mode switch (PCA only - volcano/2dgofcs controls in style panel)
          if (isTRUE(show_plot_mode_panel)) {
            div(
              class = "res-panel res-right-mini",
              div(class = "res-panel-head"),
              div(class = "res-panel-body", uiOutput("res_right_mode_ui"))
            )
          },
          
          # Panel 2: Style controls (fills remaining height)
          div(
            class = "res-panel res-right-style",
            div(class = "res-panel-head"),
            div(
              class = "res-panel-body",
              div(class = "res-style-controls", uiOutput("res_controls_ui"))
            )
          ),
          
          # Panel 3: Download (always at bottom, fixed height)
          div(
            class = "res-panel res-right-download",
            div(class = "res-panel-head"),
            div(class = "res-panel-body", uiOutput("res_download_ui"))
          )
        )
      }
    )
  })
  
    observeEvent(input$res_load_upload, {
    f <- input$res_terpbook_upload
    if (is.null(f) || is.null(f$datapath) || !file.exists(f$datapath)) {
      showNotification("Upload missing or file does not exist.", type = "error")
      return()
    }

    # Clean up any previous extraction to avoid temp bloat between loads
    if (!is.null(rv$exdir) && dir.exists(rv$exdir)) {
      try(unlink(rv$exdir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
    rv$exdir <- NULL

    # Clear I/O caches before loading new terpbook
    if (exists("tb_cache_clear", mode = "function")) {
      tb_cache_clear()
    }

    msterp_set_busy(session, TRUE, "Loading terpbook...", percent = 0)
    on.exit(msterp_set_busy(session, FALSE), add = TRUE)

    tryCatch({
      msterp_set_busy(session, TRUE, "Reading archive index...", percent = 10)
      zlist <- utils::unzip(f$datapath, list = TRUE)
      znames <- as.character(zlist$Name %||% zlist[, 1, drop = TRUE])
      if (length(znames) == 0) stop("Zip index empty (archive invalid).")

      has_manifest <- any(grepl("(^|/)manifest\\.json$", znames))
      if (!has_manifest) {
        stop("Archive does not contain manifest.json (expected run/manifest.json or similar).")
      }

      msterp_set_busy(session, TRUE, "Extracting files...", percent = 35)
      exdir <- tempfile("terpbook_")
      dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
      utils::unzip(f$datapath, exdir = exdir, overwrite = TRUE)

      msterp_set_busy(session, TRUE, "Locating run root...", percent = 55)
      run_root <- tb_find_run_root(exdir)

      msterp_set_busy(session, TRUE, "Loading manifest...", percent = 70)
      man <- tb_load_manifest(run_root)

      msterp_set_busy(session, TRUE, "Building tree...", percent = 85)
      nodes <- tb_nodes_df(man$run_root, man$manifest)
      if (is.null(nodes) || !is.data.frame(nodes)) stop("tb_nodes_df did not return a data.frame.")
      if (nrow(nodes) == 0) stop("No nodes found (manifest had no steps or step folders missing).")

      msterp_set_busy(session, TRUE, "Finalizing...", percent = 95)

      rv$exdir <- tryCatch(
        normalizePath(exdir, winslash = "/", mustWork = FALSE),
        error = function(e) exdir
      )
      rv$run_root <- man$run_root
      rv$manifest <- man$manifest
      rv$nodes_df <- nodes
      rv$load_warnings <- man$warnings %||% character()

      # Store the original terpbook filename (as saved in Windows)
      rv$terpbook_filename <- f$name %||% basename(f$datapath) %||% "Unknown"

      # Start with Overview as the active node
      rv$active_node_id <- "overview"

      rv$loaded <- TRUE

      if (length(rv$load_warnings) > 0) {
        showNotification(paste(rv$load_warnings, collapse = "\n"), type = "warning", duration = NULL)
      }

      msterp_set_busy(session, TRUE, "Ready.", percent = 100)

    }, error = function(e) {
      showNotification(paste("Load failed:", conditionMessage(e)), type = "error")
    })
  }, ignoreInit = TRUE)

  
  active_node_row <- reactive({
    req(rv$nodes_df, rv$active_node_id)

    # Overview is a virtual node, not in nodes_df
    if (identical(rv$active_node_id, "overview")) {
      return(data.frame(
        node_id = "overview",
        parent_id = NA_character_,
        depth = 0L,
        kind = "overview",
        step_index = 0L,
        engine_id = "overview",
        label = "Overview",
        node_dir = rv$run_root,
        stringsAsFactors = FALSE
      ))
    }

    i <- match(rv$active_node_id, rv$nodes_df$node_id)
    if (!is.finite(i)) return(NULL)
    rv$nodes_df[i, , drop = FALSE]
  })
  
  active_node_dir <- reactive({
    r <- active_node_row()
    if (is.null(r)) return(NULL)
    r$node_dir[[1]]
  })
  
  observeEvent(list(rv$loaded, rv$active_node_id), {
    req(rv$loaded, rv$active_node_id)
    nd <- active_node_dir()
    req(!is.null(nd))

    key <- as.character(rv$active_node_id)

    # seed caches from disk sparse override (NOT full effective)
     rs_o <- tb_load_render_state_override(nd)
     rv$cache_style_by_node[[key]] <- rs_o$style %||% list()
     rv$cache_plotly_by_node[[key]] <- rs_o$plotly %||% NULL
     rv$cache_vis_by_node[[key]] <- rs_o$visibility %||% list()
     # term_labels are now persisted in visibility section of render_state.json
     rv$last_sparse_by_node[[key]] <- rv$cache_style_by_node[[key]]

     # FIX: Clear switching flag after a short delay to allow UI to settle
     if (requireNamespace("later", quietly = TRUE)) {
       later::later(function() {
         rv$switching_node <- FALSE
       }, delay = 0.5)  # 500ms delay to let inputs update
     } else {
       rv$switching_node <- FALSE
     }

   }, ignoreInit = TRUE)
  
  active_engine_id <- reactive({
    r <- active_node_row()
    if (is.null(r)) return("")
    eng <- tolower(as.character(r$engine_id[[1]] %||% ""))
    if (exists("migrate_legacy_engine_name", mode = "function")) {
      eng <- tolower(migrate_legacy_engine_name(eng))
    }
    eng
  })
  
  active_results <- reactive({
    nd <- active_node_dir()
    if (is.null(nd)) return(NULL)
    tb_load_results(nd)
  })
  
  active_meta <- reactive({
    nd <- active_node_dir()
    if (is.null(nd)) return(list())
    tb_node_meta(nd)
  })
  
  # ---- Effective state (MEMORY-FIRST; disk only as fallback) -----------------
  res_effective_render_state_mem <- function(nodes_df, node_id) {
    chain <- tb_ancestor_chain(nodes_df, node_id)
    if (length(chain) == 0) return(list(style = list(), plotly = list(), visibility = list()))

    state <- list(style = list(), plotly = list(), visibility = list())

    for (nid in chain) {
      j <- match(nid, nodes_df$node_id)
      if (!is.finite(j)) next
      nd <- nodes_df$node_dir[[j]]

      defaults <- tb_node_defaults(nd)

      # Read disk override ONCE (used only if we don't have a cache entry)
      rs_disk <- tb_load_render_state_override(nd)

      # Cache presence matters: empty list means "cleared override" and must win over disk.
      # Wrap in isolate() to avoid reactive context issues
      has_style_cache <- isolate(!is.null(names(rv$cache_style_by_node)) && (nid %in% names(rv$cache_style_by_node)))
      has_plotly_cache <- isolate(!is.null(names(rv$cache_plotly_by_node)) && (nid %in% names(rv$cache_plotly_by_node)))
      has_vis_cache <- isolate(!is.null(names(rv$cache_vis_by_node)) && (nid %in% names(rv$cache_vis_by_node)))

      ov_style <- if (has_style_cache) {
        isolate(rv$cache_style_by_node[[nid]] %||% list())
      } else {
        rs_disk$style %||% list()
      }

      ov_plotly <- if (has_plotly_cache) {
        isolate(rv$cache_plotly_by_node[[nid]])
      } else {
        rs_disk$plotly %||% NULL
      }

       ov_vis <- if (has_vis_cache) {
         isolate(rv$cache_vis_by_node[[nid]])
       } else {
         rs_disk$visibility %||% NULL
       }

       # term_labels are now persisted in visibility section of render_state.json

       node_state <- tb_merge_states(defaults, list(style = ov_style, plotly = ov_plotly, visibility = ov_vis))
       state <- tb_merge_states(state, node_state)
     }

     state
  }
  
  active_effective_state <- reactive({
    req(rv$nodes_df, rv$active_node_id)
    style_rev()  # Trigger re-computation when style changes

    # Overview has no render state
    if (identical(rv$active_node_id, "overview")) {
      return(list(style = list(), plotly = list(), visibility = list()))
    }

    st <- res_effective_render_state_mem(rv$nodes_df, rv$active_node_id)

    # Merge visibility from cache (includes persisted term_labels)
    key <- as.character(rv$active_node_id)
    cached_vis <- isolate(rv$cache_vis_by_node[[key]] %||% list())
    st$visibility <- st$visibility %||% list()
    # Override with cached term_labels if present
    if (!is.null(cached_vis$term_labels)) {
      st$visibility$term_labels <- cached_vis$term_labels
    }

    st
  })
  
  active_rendered <- reactive({
    style_rev()
    res <- active_results()
    if (is.null(res)) return(NULL)

    eng <- active_engine_id()
    st  <- active_effective_state()
    node_meta <- active_meta()

    tryCatch(
      terpbook_render_node(eng, res, st, registry = res_registry(), node_meta = node_meta),
      error = function(e) {
        node_id <- rv$active_node_id %||% NA_character_
        msg <- paste0(
          "[Result Viewer] Render error for engine '",
          eng,
          "' node '",
          node_id,
          "': ",
          conditionMessage(e)
        )
        message(msg)

        trace_lines <- NULL
        if (requireNamespace("rlang", quietly = TRUE)) {
          trace_lines <- tryCatch(capture.output(rlang::trace_back()), error = function(e) NULL)
        }
        if (is.null(trace_lines) || length(trace_lines) == 0) {
          trace_lines <- vapply(sys.calls(), function(x) paste(deparse(x), collapse = ""), character(1))
        }
        if (length(trace_lines) > 0) {
          message(paste(trace_lines, collapse = "
"))
        }

        style_dump <- tryCatch(capture.output(utils::str(st$style, max.level = 2)), error = function(e) NULL)
        if (!is.null(style_dump) && length(style_dump) > 0) {
          message("[Result Viewer] Effective style:")
          message(paste(style_dump, collapse = "
"))
        }

        attr(e, "msterp_where") <- "terpbook_render_node"
        e
      }
    )
  })
  
  res_label_empty_state <- function() {
    list(id = NULL, plot_key = NULL, eng = NULL)
  }

  res_set_label_selected <- function(id, plot_key, eng) {
    rv$label_selected <- list(id = id, plot_key = plot_key, eng = eng)
  }

  res_clear_label_selected <- function() {
    rv$label_selected <- res_label_empty_state()
  }

  res_nearest_id <- function(click, df, id_col, x_col, y_col, tol_x, tol_y) {
    if (is.null(click) || is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
    if (!is.finite(tol_x) || !is.finite(tol_y) || tol_x <= 0 || tol_y <= 0) return(NULL)

    x <- suppressWarnings(as.numeric(click$x))
    y <- suppressWarnings(as.numeric(click$y))
    if (!is.finite(x) || !is.finite(y)) return(NULL)

    xs <- suppressWarnings(as.numeric(df[[x_col]]))
    ys <- suppressWarnings(as.numeric(df[[y_col]]))
    ok <- is.finite(xs) & is.finite(ys)
    if (!any(ok)) return(NULL)

    xs <- xs[ok]
    ys <- ys[ok]
    ids <- as.character(df[[id_col]][ok])

    dx <- (xs - x) / tol_x
    dy <- (ys - y) / tol_y
    d <- sqrt(dx * dx + dy * dy)
    idx <- which.min(d)
    if (length(idx) == 0 || !is.finite(d[[idx]]) || d[[idx]] >= 1) return(NULL)
    ids[[idx]]
  }

  res_hover_empty_state <- function() {
    list(show = FALSE, title = "", lines = character(), x = NA_real_, y = NA_real_)
  }

  res_event_tolerance <- function(evt, fallback_x, fallback_y, px = 10) {
    tol_x <- fallback_x
    tol_y <- fallback_y
    if (is.null(evt)) return(list(tol_x = tol_x, tol_y = tol_y))

    dom <- evt$domain %||% list()
    rng <- evt$range %||% list()

    dx <- suppressWarnings(as.numeric(dom$right %||% NA_real_)) -
      suppressWarnings(as.numeric(dom$left %||% NA_real_))
    dy <- suppressWarnings(as.numeric(dom$top %||% NA_real_)) -
      suppressWarnings(as.numeric(dom$bottom %||% NA_real_))

    rx <- suppressWarnings(as.numeric(rng$right %||% NA_real_)) -
      suppressWarnings(as.numeric(rng$left %||% NA_real_))
    ry <- suppressWarnings(as.numeric(rng$bottom %||% NA_real_)) -
      suppressWarnings(as.numeric(rng$top %||% NA_real_))

    if (is.finite(dx) && is.finite(rx) && rx != 0) {
      tol_x <- abs(dx / rx) * px
    }
    if (is.finite(dy) && is.finite(ry) && ry != 0) {
      tol_y <- abs(dy / ry) * px
    }

    list(tol_x = tol_x, tol_y = tol_y)
  }

  res_hover_coords <- function(hover) {
    if (is.null(hover)) return(list(x = NA_real_, y = NA_real_))
    coords <- hover$coords_css %||% hover$coords_img %||% list()
    x <- suppressWarnings(as.numeric(coords$x %||% NA_real_))
    y <- suppressWarnings(as.numeric(coords$y %||% NA_real_))
    if (!is.finite(x)) x <- NA_real_
    if (!is.finite(y)) y <- NA_real_
    list(x = x, y = y)
  }

  res_format_num <- function(x, digits = 2) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) return("NA")
    sprintf(paste0("%.", digits, "f"), v)
  }

  res_format_pval <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) return("NA")
    if (v < 0.001) return(sprintf("%.2e", v))
    sprintf("%.3f", signif(v, 2))
  }

  res_hover_card_ui <- function(info) {
    if (is.null(info) || !isTRUE(info$show)) return(NULL)
    x <- suppressWarnings(as.numeric(info$x %||% NA_real_))
    y <- suppressWarnings(as.numeric(info$y %||% NA_real_))
    if (!is.finite(x) || !is.finite(y)) return(NULL)

    style <- sprintf("left:%spx; top:%spx; transform: translate(12px, 12px);", round(x), round(y))
    div(
      class = "res-hover-card",
      style = style,
      div(class = "res-hover-title", as.character(info$title %||% "")),
      lapply(as.character(info$lines %||% character()), function(line) {
        div(line)
      })
    )
  }

  res_set_hover_info <- function(plot_key, info) {
    plot_key <- as.character(plot_key %||% "")
    if (!nzchar(plot_key)) return(invisible(NULL))
    cur <- rv$hover_info_by_plot %||% list()
    cur[[plot_key]] <- info
    rv$hover_info_by_plot <- cur
    invisible(NULL)
  }

  res_volcano_label_list <- function(style, plot_key) {
    label_map_json <- style$label_genes_map %||% "{}"
    label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())
    labs <- label_map[[plot_key]] %||% ""
    labs <- trimws(unlist(strsplit(as.character(labs), "\n", fixed = TRUE)))
    labs[nzchar(labs)]
  }

  res_update_volcano_label_map <- function(plot_key, labels, update_input = TRUE) {
    plot_key <- as.character(plot_key %||% "")
    if (!nzchar(plot_key)) return(invisible(NULL))

    labels <- unique(as.character(labels))
    labels <- labels[nzchar(labels)]

    eff <- isolate(active_effective_state())
    label_map_json <- eff$style$label_genes_map %||% "{}"
    label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())

    label_map[[plot_key]] <- if (length(labels) > 0) paste(labels, collapse = "\n") else ""

    new_json <- jsonlite::toJSON(label_map, auto_unbox = TRUE)
    node_id <- rv$active_node_id
    key <- as.character(node_id %||% "")
    if (!nzchar(key)) return(invisible(NULL))

    cur_style <- rv$cache_style_by_node[[key]] %||% list()
    cur_style$label_genes_map <- as.character(new_json)
    rv$cache_style_by_node[[key]] <- cur_style

    nd <- active_node_dir()
    if (!is.null(nd)) {
      .commit_style_debounced(node_dir = nd, payload = list(style = cur_style))
    }

    if (isTRUE(update_input)) {
      plot_names <- rv$current_plot_names %||% character()
      current_plot <- input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
      if (identical(current_plot, plot_key)) {
        updateTextAreaInput(session, "res_volcano_label_genes", value = label_map[[plot_key]] %||% "")
      }
    }

    rv$has_unsaved_changes <- TRUE
    rv$save_status <- "dirty"
    style_rev(isolate(style_rev()) + 1L)

    invisible(NULL)
  }

  res_volcano_plot_state <- function(res, style, visibility, plot_key, plotly_state) {
    comparisons <- res$data$comparisons %||% NULL
    comp <- if (!is.null(comparisons) && plot_key %in% names(comparisons)) comparisons[[plot_key]] else NULL

    df <- comp$points %||% res$data$points
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

    if (is.null(df$gene)) {
      df$gene <- df$gene_symbol %||% df$gene_id %||% df$id %||% df$Gene %||% ""
    }
    df$gene <- as.character(df$gene)

    hide_ids <- unique(as.character(visibility$hide_ids %||% character()))
    hide_ids <- hide_ids[nzchar(hide_ids)]
    if (length(hide_ids) > 0) df <- df[!(df$gene %in% hide_ids), , drop = FALSE]

    apply_fdr <- comp$comparison$apply_fdr %||% res$params$apply_fdr %||% TRUE
    if (isTRUE(apply_fdr) && !is.null(df$padj)) {
      pvec <- df$padj
    } else {
      pvec <- df$pval %||% df$p %||% df$padj
    }

    pvec_raw <- suppressWarnings(as.numeric(pvec))
    pvec_raw[!is.finite(pvec_raw)] <- NA_real_
    pvec <- pmax(pvec_raw, 1e-300)
    pvec[!is.finite(pvec)] <- 1
    df$neglog10p <- -log10(pvec)
    df$pval_show <- pvec_raw

    df$log2fc <- suppressWarnings(as.numeric(df$log2fc %||% df$log2FC))

    x_range_mode <- style$x_range_mode %||% "auto"
    y_range_mode <- style$y_range_mode %||% "auto"

    if (x_range_mode == "manual") {
      xlim <- suppressWarnings(as.numeric(c(style$x_min %||% -7, style$x_max %||% 7)))
    } else {
      x_data_range <- range(df$log2fc, na.rm = TRUE, finite = TRUE)
      x_pad <- max(abs(x_data_range)) * 1.1
      xlim <- c(-x_pad, x_pad)
    }
    if (length(xlim) != 2 || any(!is.finite(xlim))) xlim <- c(-7, 7)
    xlim <- sort(xlim)

    if (y_range_mode == "manual") {
      y_min_val <- style$y_min %||% 0
      ylim <- suppressWarnings(as.numeric(c(y_min_val, style$y_max %||% max(df$neglog10p, na.rm = TRUE))))
    } else {
      ylim <- c(0, max(df$neglog10p, na.rm = TRUE) * 1.05)
    }
    if (length(ylim) != 2 || any(!is.finite(ylim))) ylim <- c(0, max(df$neglog10p, na.rm = TRUE))
    ylim <- sort(ylim)

    labs <- res_volcano_label_list(style, plot_key)
    labs <- intersect(labs, df$gene)

    saved <- plotly_state$labels_by_plot[[plot_key]] %||%
      plotly_state$labels_by_plot$default %||%
      plotly_state$labels %||% list()

    list(
      df = df,
      xlim = xlim,
      ylim = ylim,
      labs = labs,
      saved = saved,
      apply_fdr = isTRUE(apply_fdr)
    )
  }

  res_volcano_label_positions <- function(state) {
    if (is.null(state)) return(data.frame())
    labs <- state$labs %||% character()
    if (length(labs) == 0) return(data.frame())

    df <- state$df
    df_lab <- df[df$gene %in% labs, , drop = FALSE]
    if (nrow(df_lab) == 0) return(data.frame())

    xlim <- state$xlim
    ylim <- state$ylim
    saved <- state$saved %||% list()

    df_lab$label_x <- NA_real_
    df_lab$label_y <- NA_real_

    for (i in seq_len(nrow(df_lab))) {
      id <- as.character(df_lab$gene[[i]])
      s <- saved[[id]] %||% list()

      pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
      lx <- pos$x
      ly <- pos$y

      if (!is.finite(lx)) lx <- df_lab$log2fc[[i]]
      if (!is.finite(ly)) ly <- df_lab$neglog10p[[i]]

      if (is.finite(lx) && is.finite(ly) && is.null(s$x_range) && is.null(s$y_range)) {
        if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
            !is.finite(suppressWarnings(as.numeric(s$y)))) {
          lx <- df_lab$log2fc[[i]] + ifelse(df_lab$log2fc[[i]] < 0, -0.5, 0.5)
          ly <- df_lab$neglog10p[[i]] + 0.5
        }
      }

      lx <- max(xlim[[1]], min(xlim[[2]], lx))
      ly <- max(ylim[[1]], min(ylim[[2]], ly))

      df_lab$label_x[[i]] <- lx
      df_lab$label_y[[i]] <- ly
    }

    data.frame(
      id = df_lab$gene,
      data_x = df_lab$log2fc,
      data_y = df_lab$neglog10p,
      label_x = df_lab$label_x,
      label_y = df_lab$label_y,
      stringsAsFactors = FALSE
    )
  }

  res_2dgofcs_plot_state <- function(res, style, visibility, plot_key, plotly_state) {
    data_obj <- res$data %||% list()
    df <- NULL

    analyses <- data_obj$analyses %||% NULL
    if (!is.null(analyses) && is.list(analyses) && plot_key %in% names(analyses)) {
      df <- analyses[[plot_key]]$terms
    }

    if (is.null(df) || !is.data.frame(df)) {
      tab_key <- toupper(sub("_plot$", "", plot_key))
      if (tab_key %in% c("BP", "MF", "CC")) {
        tab_data <- data_obj[[tab_key]]
        df <- tab_data$terms %||% tab_data$data %||% tab_data
      }
    }

    if (is.null(df) || !is.data.frame(df)) {
      df <- data_obj$terms %||% data_obj
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

    if (!"term_id" %in% names(df)) {
      for (col in c("TermID", "termID", "go_id", "GO", "ID")) {
        if (col %in% names(df)) { df$term_id <- df[[col]]; break }
      }
    }
    if (!"term" %in% names(df)) {
      for (col in c("term_name", "Term", "pathway", "Pathway", "term_id")) {
        if (col %in% names(df)) { df$term <- df[[col]]; break }
      }
    }
    if ("term_id" %in% names(df)) df$term_id <- as.character(df$term_id)
    if ("term" %in% names(df)) df$term <- as.character(df$term)
    if (!"fdr" %in% names(df)) {
      for (col in c("FDR", "p.adjust", "padj", "pval")) {
        if (col %in% names(df)) { df$fdr <- df[[col]]; break }
      }
      if (!"fdr" %in% names(df)) df$fdr <- 0.05
    }
    if (!"n" %in% names(df)) {
      for (col in c("n_genes", "count", "Count", "GeneCount", "size")) {
        if (col %in% names(df)) { df$n <- df[[col]]; break }
      }
      if (!"n" %in% names(df)) df$n <- 5
    }

    if (!"score_x" %in% names(df)) df$score_x <- 0
    if (!"score_y" %in% names(df)) df$score_y <- 0
    if (!"ontology" %in% names(df)) df$ontology <- NA_character_

    ontology_filter <- style$ontology_filter %||% "all"
    if (!identical(ontology_filter, "all") && "ontology" %in% names(df)) {
      df <- df[toupper(as.character(df$ontology)) == toupper(ontology_filter), , drop = FALSE]
    }

    df_all <- df
    hidden_terms <- visibility$hidden_terms %||% character(0)
    term_labels <- visibility$term_labels %||% list()
    df_plot <- df_all[!(df_all$term %in% hidden_terms), , drop = FALSE]

    if (length(term_labels) > 0 && nrow(df_plot) > 0) {
      df_plot$term_original <- df_plot$term
      for (i in seq_len(nrow(df_plot))) {
        orig <- df_plot$term[i]
        if (!is.null(term_labels[[orig]]) && nzchar(term_labels[[orig]])) {
          df_plot$term[i] <- term_labels[[orig]]
        }
      }
    } else if (nrow(df_plot) > 0) {
      df_plot$term_original <- df_plot$term
    }

    xlim <- suppressWarnings(as.numeric(c(style$x_min %||% -1, style$x_max %||% 1)))
    ylim <- suppressWarnings(as.numeric(c(style$y_min %||% -1, style$y_max %||% 1)))
    if (length(xlim) != 2 || any(!is.finite(xlim))) xlim <- c(-1, 1)
    if (length(ylim) != 2 || any(!is.finite(ylim))) ylim <- c(-1, 1)
    xlim <- sort(xlim)
    ylim <- sort(ylim)

    saved <- plotly_state$labels_by_plot[[plot_key]] %||%
      plotly_state$labels_by_plot$default %||%
      plotly_state$labels %||% list()

    # For 2dgofcs: ALL visible points get labels (visibility controlled via table checkbox)
    # Labels are tied to point visibility - if a point is visible, it has a label
    labs <- if (nrow(df_plot) > 0) {
      df_plot$term
    } else {
      character(0)
    }

    list(df_plot = df_plot, xlim = xlim, ylim = ylim, labs = labs, saved = saved)
  }

  res_2dgofcs_label_positions <- function(state) {
    if (is.null(state)) return(data.frame())
    labs <- state$labs %||% character()
    df_plot <- state$df_plot
    if (length(labs) == 0 || is.null(df_plot) || nrow(df_plot) == 0) return(data.frame())

    df_lab <- df_plot[df_plot$term %in% labs, , drop = FALSE]
    if (nrow(df_lab) == 0) return(data.frame())

    xlim <- state$xlim
    ylim <- state$ylim
    saved <- state$saved %||% list()

    x_span <- diff(xlim)
    y_span <- diff(ylim)
    if (!is.finite(x_span) || x_span <= 0) x_span <- 2
    if (!is.finite(y_span) || y_span <= 0) y_span <- 2

    df_lab$label_x <- NA_real_
    df_lab$label_y <- NA_real_
    label_ids <- character(nrow(df_lab))

    for (i in seq_len(nrow(df_lab))) {
      id <- as.character(df_lab$term_id[[i]] %||% df_lab$term_original[[i]] %||% df_lab$term[[i]])
      label_ids[[i]] <- id
      s <- saved[[id]] %||% list()

      px <- df_lab$score_x[[i]]
      py <- df_lab$score_y[[i]]

      x_mid <- mean(xlim)
      x_right_edge <- xlim[[1]] + 0.85 * x_span
      x_left_edge <- xlim[[1]] + 0.15 * x_span
      y_top_edge <- ylim[[1]] + 0.85 * y_span

      default_x_offset <- if (px > x_right_edge) {
        -0.06 * x_span
      } else if (px < x_left_edge) {
        0.06 * x_span
      } else if (px < x_mid) {
        -0.06 * x_span
      } else {
        0.06 * x_span
      }
      default_y_offset <- if (py > y_top_edge) -0.04 * y_span else 0.04 * y_span

      pos <- .tb_label_xy_from_state(s, x_range = xlim, y_range = ylim)
      lx <- pos$x
      ly <- pos$y

      if (!is.finite(lx)) lx <- px
      if (!is.finite(ly)) ly <- py

      if (is.finite(lx) && is.finite(ly) && is.null(s$x_range) && is.null(s$y_range)) {
        if (is.null(s$x) || is.null(s$y) || !is.finite(suppressWarnings(as.numeric(s$x))) ||
            !is.finite(suppressWarnings(as.numeric(s$y)))) {
          lx <- px + default_x_offset
          ly <- py + default_y_offset
        }
      }

      pad_x <- 0.05 * x_span
      pad_y <- 0.05 * y_span
      lx <- max(xlim[[1]] + pad_x, min(xlim[[2]] - pad_x, lx))
      ly <- max(ylim[[1]] + pad_y, min(ylim[[2]] - pad_y, ly))

      df_lab$label_x[[i]] <- lx
      df_lab$label_y[[i]] <- ly
    }

    data.frame(
      id = label_ids,
      data_x = df_lab$score_x,
      data_y = df_lab$score_y,
      label_x = df_lab$label_x,
      label_y = df_lab$label_y,
      stringsAsFactors = FALSE
    )
  }

  res_update_plotly_labels <- function(plot_key, update_fn) {
    plot_key <- as.character(plot_key %||% "")
    if (!nzchar(plot_key) || !is.function(update_fn)) return(invisible(NULL))

    node_id <- rv$active_node_id
    key <- as.character(node_id %||% "")
    if (!nzchar(key)) return(invisible(NULL))

    cur_plotly <- rv$cache_plotly_by_node[[key]] %||% list()
    cur_plotly$labels_by_plot <- cur_plotly$labels_by_plot %||% list()
    cur_labels <- cur_plotly$labels_by_plot[[plot_key]] %||% list()
    new_labels <- update_fn(cur_labels)
    if (is.null(new_labels)) new_labels <- list()

    cur_plotly$labels_by_plot[[plot_key]] <- new_labels
    cur_plotly$labels <- new_labels
    rv$cache_plotly_by_node[[key]] <- cur_plotly

    nd <- active_node_dir()
    if (!is.null(nd)) {
      .commit_style_debounced(node_dir = nd, payload = list(plotly = cur_plotly))
    }

    rv$has_unsaved_changes <- TRUE
    rv$save_status <- "dirty"
    style_rev(isolate(style_rev()) + 1L)

    invisible(new_labels)
  }

  res_set_label_position <- function(plot_key, label_id, xlim, ylim, click) {
    plot_key <- as.character(plot_key %||% "")
    label_id <- as.character(label_id %||% "")
    if (!nzchar(plot_key) || !nzchar(label_id)) return(invisible(NULL))

    xlim <- suppressWarnings(as.numeric(xlim))
    ylim <- suppressWarnings(as.numeric(ylim))
    if (length(xlim) != 2 || length(ylim) != 2 || any(!is.finite(c(xlim, ylim)))) {
      return(invisible(NULL))
    }

    norm <- tb_normalize_coords(click$x, click$y, xlim, ylim)
    x_norm <- suppressWarnings(as.numeric(norm$x[[1]]))
    y_norm <- suppressWarnings(as.numeric(norm$y[[1]]))
    if (!is.finite(x_norm) || !is.finite(y_norm)) return(invisible(NULL))

    res_update_plotly_labels(plot_key, function(cur_labels) {
      lab <- cur_labels[[label_id]] %||% list()
      lab$x <- x_norm
      lab$y <- y_norm
      lab$x_range <- xlim
      lab$y_range <- ylim
      cur_labels[[label_id]] <- lab
      cur_labels
    })
  }

  res_add_volcano_label <- function(plot_key, gene_id) {
    plot_key <- as.character(plot_key %||% "")
    gene_id <- as.character(gene_id %||% "")
    if (!nzchar(plot_key) || !nzchar(gene_id)) return(invisible(NULL))

    style <- isolate(active_effective_state()$style %||% list())
    labs <- res_volcano_label_list(style, plot_key)
    if (gene_id %in% labs) return(invisible(NULL))
    res_update_volcano_label_map(plot_key, c(labs, gene_id), update_input = TRUE)
  }

  res_add_2dgofcs_label <- function(plot_key, term_id) {
    plot_key <- as.character(plot_key %||% "")
    term_id <- as.character(term_id %||% "")
    if (!nzchar(plot_key) || !nzchar(term_id)) return(invisible(NULL))

    res_update_plotly_labels(plot_key, function(cur_labels) {
      if (!term_id %in% names(cur_labels)) {
        cur_labels[[term_id]] <- list()
      }
      cur_labels
    })
  }

  # Toggle 2dgofcs label (add if not present, remove if present)
  res_toggle_2dgofcs_label <- function(plot_key, term_id) {
    plot_key <- as.character(plot_key %||% "")
    term_id <- as.character(term_id %||% "")
    if (!nzchar(plot_key) || !nzchar(term_id)) return(invisible(NULL))

    res_update_plotly_labels(plot_key, function(cur_labels) {
      if (term_id %in% names(cur_labels)) {
        # Remove label
        cur_labels[[term_id]] <- NULL
      } else {
        # Add label
        cur_labels[[term_id]] <- list()
      }
      cur_labels
    })
  }

  res_remove_label <- function(eng, plot_key, label_id) {
    eng <- tolower(as.character(eng %||% ""))
    plot_key <- as.character(plot_key %||% "")
    label_id <- as.character(label_id %||% "")
    if (!nzchar(plot_key) || !nzchar(label_id)) return(invisible(NULL))

    if (identical(eng, "volcano")) {
      style <- isolate(active_effective_state()$style %||% list())
      labs <- res_volcano_label_list(style, plot_key)
      if (label_id %in% labs) {
        res_update_volcano_label_map(plot_key, setdiff(labs, label_id), update_input = TRUE)
      }
    }

    res_update_plotly_labels(plot_key, function(cur_labels) {
      cur_labels[[label_id]] <- NULL
      cur_labels
    })

    res_clear_label_selected()
  }

  res_is_interactive_view <- function(style) {
    mode <- tolower(as.character(style$view_mode %||% "export_preview"))
    identical(mode, "interactive")
  }

  res_volcano_hover_info <- function(state, hover) {
    if (is.null(state)) return(res_hover_empty_state())
    df <- state$df
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(res_hover_empty_state())

    x_span <- diff(state$xlim)
    y_span <- diff(state$ylim)
    if (!is.finite(x_span) || x_span <= 0) x_span <- 1
    if (!is.finite(y_span) || y_span <= 0) y_span <- 1

    tol_defaults <- list(
      tol_x = max(0.03 * x_span, 0.2),
      tol_y = max(0.05 * y_span, 0.3)
    )
    tol <- res_event_tolerance(hover, tol_defaults$tol_x, tol_defaults$tol_y, px = 10)
    tol_x <- tol$tol_x
    tol_y <- tol$tol_y

    hovered_id <- res_nearest_id(hover, df, "gene", "log2fc", "neglog10p", tol_x, tol_y)
    if (is.null(hovered_id)) return(res_hover_empty_state())

    row <- df[df$gene == hovered_id, , drop = FALSE]
    if (nrow(row) == 0) return(res_hover_empty_state())
    row <- row[1, , drop = FALSE]

    coords <- res_hover_coords(hover)
    if (!is.finite(coords$x) || !is.finite(coords$y)) return(res_hover_empty_state())

    p_label <- if (isTRUE(state$apply_fdr)) "padj" else "pval"
    p_val <- suppressWarnings(as.numeric(row$pval_show %||% NA_real_))
    if (!is.finite(p_val)) p_val <- suppressWarnings(as.numeric(row$padj %||% row$pval %||% row$p %||% NA_real_))

    list(
      show = TRUE,
      title = as.character(hovered_id),
      lines = c(
        paste0("log2FC: ", res_format_num(row$log2fc, 2)),
        paste0(p_label, ": ", res_format_pval(p_val))
      ),
      x = coords$x,
      y = coords$y
    )
  }

  res_2dgofcs_hover_info <- function(state, hover) {
    if (is.null(state)) return(res_hover_empty_state())
    df_points <- state$df_plot
    if (is.null(df_points) || !is.data.frame(df_points) || nrow(df_points) == 0) return(res_hover_empty_state())

    df_points$id <- as.character(df_points$term_id %||% df_points$term_original %||% df_points$term)

    x_span <- diff(state$xlim)
    y_span <- diff(state$ylim)
    if (!is.finite(x_span) || x_span <= 0) x_span <- 2
    if (!is.finite(y_span) || y_span <= 0) y_span <- 2

    tol_defaults <- list(
      tol_x = max(0.03 * x_span, 0.05),
      tol_y = max(0.03 * y_span, 0.05)
    )
    tol <- res_event_tolerance(hover, tol_defaults$tol_x, tol_defaults$tol_y, px = 10)
    tol_x <- tol$tol_x
    tol_y <- tol$tol_y

    hovered_id <- res_nearest_id(hover, df_points, "id", "score_x", "score_y", tol_x, tol_y)
    if (is.null(hovered_id)) return(res_hover_empty_state())

    row <- df_points[df_points$id == hovered_id, , drop = FALSE]
    if (nrow(row) == 0) return(res_hover_empty_state())
    row <- row[1, , drop = FALSE]

    coords <- res_hover_coords(hover)
    if (!is.finite(coords$x) || !is.finite(coords$y)) return(res_hover_empty_state())

    n_val <- suppressWarnings(as.numeric(row$n %||% row$n_genes %||% NA_real_))
    n_val <- if (is.finite(n_val)) as.integer(round(n_val)) else NA_integer_

    list(
      show = TRUE,
      title = as.character(row$term %||% hovered_id),
      lines = c(
        paste0("FDR: ", res_format_pval(row$fdr)),
        paste0("Overlap genes: ", if (is.finite(n_val)) n_val else "NA")
      ),
      x = coords$x,
      y = coords$y
    )
  }

  res_handle_plot_hover <- function(hover, eng, plot_key) {
    eng <- tolower(as.character(eng %||% ""))
    plot_key <- as.character(plot_key %||% "")
    if (!nzchar(plot_key)) return(invisible(NULL))

    if (is.null(hover) || isFALSE(hover$inside)) {
      res_set_hover_info(plot_key, res_hover_empty_state())
      return(invisible(NULL))
    }

    res <- active_results()
    if (is.null(res)) return(invisible(NULL))

    st <- active_effective_state()
    style <- st$style %||% list()
    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()
    is_interactive <- isTRUE(res_is_interactive_view(style))

    if (!isTRUE(is_interactive)) {
      res_set_hover_info(plot_key, res_hover_empty_state())
      return(invisible(NULL))
    }

    info <- res_hover_empty_state()
    if (identical(eng, "volcano")) {
      state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
      info <- res_volcano_hover_info(state, hover)
    } else if (identical(eng, "2dgofcs")) {
      state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
      info <- res_2dgofcs_hover_info(state, hover)
    }

    res_set_hover_info(plot_key, info)
    invisible(NULL)
  }

  res_handle_label_click <- function(click, eng, plot_key) {
    eng <- tolower(as.character(eng %||% ""))
    plot_key <- as.character(plot_key %||% "")
    if (is.null(click) || !nzchar(plot_key) || !nzchar(eng)) return(invisible(NULL))

    res <- active_results()
    if (is.null(res)) return(invisible(NULL))

    st <- active_effective_state()
    style <- st$style %||% list()
    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()
    is_interactive <- isTRUE(res_is_interactive_view(style))

    selected <- rv$label_selected %||% res_label_empty_state()
    sel_id <- as.character(selected$id %||% "")
    sel_plot <- as.character(selected$plot_key %||% "")
    sel_eng <- as.character(selected$eng %||% "")

    if (identical(eng, "volcano")) {
      state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(invisible(NULL))

      labels_df <- res_volcano_label_positions(state)
      x_span <- diff(state$xlim)
      y_span <- diff(state$ylim)
      if (!is.finite(x_span) || x_span <= 0) x_span <- 1
      if (!is.finite(y_span) || y_span <= 0) y_span <- 1

      tol_x <- max(0.03 * x_span, 0.2)
      tol_y <- max(0.05 * y_span, 0.3)
      tol_defaults <- list(tol_x = tol_x, tol_y = tol_y)
      tol <- res_event_tolerance(click, tol_defaults$tol_x, tol_defaults$tol_y, px = 10)
      tol_x <- tol$tol_x
      tol_y <- tol$tol_y

      clicked_label <- res_nearest_id(click, labels_df, "id", "label_x", "label_y", tol_x, tol_y)
      if (!is.null(clicked_label)) {
        if (nzchar(sel_id) && identical(sel_id, clicked_label) &&
            identical(sel_plot, plot_key) && identical(sel_eng, eng)) {
          res_clear_label_selected()
        } else {
          res_set_label_selected(clicked_label, plot_key, eng)
        }
        return(invisible(NULL))
      }

      if (nzchar(sel_id) && identical(sel_plot, plot_key) && identical(sel_eng, eng)) {
        res_set_label_position(plot_key, sel_id, state$xlim, state$ylim, click)
        res_clear_label_selected()
        return(invisible(NULL))
      }

      clicked_point <- res_nearest_id(click, state$df, "gene", "log2fc", "neglog10p", tol_x, tol_y)
      if (!is.null(clicked_point)) {
        if (isTRUE(is_interactive)) {
          res_show_uniprot_summary(clicked_point)
        } else {
          res_add_volcano_label(plot_key, clicked_point)
        }
      }
      return(invisible(NULL))
    }

    if (identical(eng, "2dgofcs")) {
      state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(invisible(NULL))

      labels_df <- res_2dgofcs_label_positions(state)
      x_span <- diff(state$xlim)
      y_span <- diff(state$ylim)
      if (!is.finite(x_span) || x_span <= 0) x_span <- 2
      if (!is.finite(y_span) || y_span <= 0) y_span <- 2

      tol_x <- max(0.03 * x_span, 0.05)
      tol_y <- max(0.03 * y_span, 0.05)
      tol_defaults <- list(tol_x = tol_x, tol_y = tol_y)
      tol <- res_event_tolerance(click, tol_defaults$tol_x, tol_defaults$tol_y, px = 10)
      tol_x <- tol$tol_x
      tol_y <- tol$tol_y

      clicked_label <- res_nearest_id(click, labels_df, "id", "label_x", "label_y", tol_x, tol_y)
      if (!is.null(clicked_label)) {
        if (nzchar(sel_id) && identical(sel_id, clicked_label) &&
            identical(sel_plot, plot_key) && identical(sel_eng, eng)) {
          res_clear_label_selected()
        } else {
          res_set_label_selected(clicked_label, plot_key, eng)
        }
        return(invisible(NULL))
      }

      if (nzchar(sel_id) && identical(sel_plot, plot_key) && identical(sel_eng, eng)) {
        res_set_label_position(plot_key, sel_id, state$xlim, state$ylim, click)
        res_clear_label_selected()
        return(invisible(NULL))
      }

      df_points <- state$df_plot
      if (!is.null(df_points) && nrow(df_points) > 0) {
        df_points$id <- as.character(df_points$term_id %||% df_points$term_original %||% df_points$term)
        clicked_point <- res_nearest_id(click, df_points, "id", "score_x", "score_y", tol_x, tol_y)
        if (!is.null(clicked_point)) {
      if (!isTRUE(is_interactive)) {
        res_add_2dgofcs_label(plot_key, clicked_point)
      }
        }
      }
      return(invisible(NULL))
    }

    invisible(NULL)
  }

  res_handle_label_dblclick <- function(click, eng, plot_key) {
    eng <- tolower(as.character(eng %||% ""))
    plot_key <- as.character(plot_key %||% "")
    if (is.null(click) || !nzchar(plot_key) || !nzchar(eng)) return(invisible(NULL))

    res <- active_results()
    if (is.null(res)) return(invisible(NULL))

    st <- active_effective_state()
    style <- st$style %||% list()
    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()

    if (identical(eng, "volcano")) {
      state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(invisible(NULL))

      labels_df <- res_volcano_label_positions(state)
      x_span <- diff(state$xlim)
      y_span <- diff(state$ylim)
      if (!is.finite(x_span) || x_span <= 0) x_span <- 1
      if (!is.finite(y_span) || y_span <= 0) y_span <- 1

      tol_x <- max(0.03 * x_span, 0.2)
      tol_y <- max(0.05 * y_span, 0.3)

      clicked_label <- res_nearest_id(click, labels_df, "id", "label_x", "label_y", tol_x, tol_y)
      if (!is.null(clicked_label)) {
        res_remove_label(eng, plot_key, clicked_label)
      }
      return(invisible(NULL))
    }

    if (identical(eng, "2dgofcs")) {
      state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(invisible(NULL))

      labels_df <- res_2dgofcs_label_positions(state)
      x_span <- diff(state$xlim)
      y_span <- diff(state$ylim)
      if (!is.finite(x_span) || x_span <= 0) x_span <- 2
      if (!is.finite(y_span) || y_span <= 0) y_span <- 2

      tol_x <- max(0.03 * x_span, 0.05)
      tol_y <- max(0.03 * y_span, 0.05)

      clicked_label <- res_nearest_id(click, labels_df, "id", "label_x", "label_y", tol_x, tol_y)
      if (!is.null(clicked_label)) {
        res_remove_label(eng, plot_key, clicked_label)
      }
      return(invisible(NULL))
    }

    invisible(NULL)
  }

  res_remove_selected_label <- function() {
    selected <- rv$label_selected %||% res_label_empty_state()
    sel_id <- as.character(selected$id %||% "")
    sel_plot <- as.character(selected$plot_key %||% "")
    sel_eng <- as.character(selected$eng %||% "")
    if (!nzchar(sel_id) || !nzchar(sel_plot) || !nzchar(sel_eng)) return(invisible(NULL))

    res_remove_label(sel_eng, sel_plot, sel_id)
    invisible(NULL)
  }

  # ---- Sidebar tree ----------------------------------------------------------

  output$res_sidebar_nodes <- renderUI({
    if (is.null(rv$nodes_df) || nrow(rv$nodes_df) == 0) {
      return(div("No terpbook loaded."))
    }

    df <- rv$nodes_df
    num_map <- res_compute_node_numbers(df)

    # All nodes are clickable - parent nodes with children can still render
    # their own content (e.g., PCA scores/scree, Volcano plot)
    registry <- res_registry()
    should_lock <- res_should_lock_nodes(df, registry)
    should_hide <- res_should_hide_nodes(df, registry)

    btns <- tagList()

    # Add Overview button first ([0] Overview)
    overview_active <- identical(rv$active_node_id, "overview")
    btns <- tagAppendChildren(
      btns,
      actionButton(
        "res_nav_overview",
        label = tagList(
          span(class = "res-step-num", "[0]"),
          span(class = "res-step-text", "Overview")
        ),
        class = paste(
          "res-node-btn",
          "res-node-depth-0",
          if (overview_active) "res-node-active" else ""
        ),
        style = "margin-left:0px; width: calc(100% - 0px);"
      )
    )

    for (i in seq_len(nrow(df))) {
      nid <- df$node_id[i]

      # Skip overview (already added) - now showing Data Processor
      if (nid == "overview") next

      # Skip nodes that should be hidden (e.g., system-generated substeps with results_hidden_system_generated)
      if (should_hide[i]) next

      eng_id <- tolower(df$engine_id[i] %||% "")

      depth <- as.integer(df$depth[i] %||% 0L)
      lab <- res_pretty_label(df$label[i], engine_id = df$engine_id[i])
      is_locked <- should_lock[i]

      number <- num_map[[nid]] %||% ""
      btn_label <- tagList(
        span(class = "res-step-num", number),
        span(class = "res-step-text", lab)
      )

      id <- paste0("res_nav_", gsub("[^A-Za-z0-9_]", "_", nid))

      depth_class <- paste0("res-node-depth-", pmin(depth, 4L))

      indent_px <- max(0L, pmin(depth, 4L)) * 16L
      btn_style <- sprintf("margin-left:%dpx; width: calc(100%% - %dpx);", indent_px, indent_px)

      # Add disabled style for locked parent nodes
      if (is_locked) {
        btn_style <- paste0(btn_style, " opacity: 0.5; cursor: not-allowed;")
      }

      btns <- tagAppendChildren(
        btns,
        actionButton(
          id,
          label = btn_label,
          class = paste(
            "res-node-btn",
            depth_class,
            if (is_locked) "res-node-locked" else "",
            if (identical(rv$active_node_id, nid)) "res-node-active" else ""
          ),
          style = btn_style
        )
      )
    }

    btns
  })

  # Create observers for navigation buttons OUTSIDE of renderUI
  # This prevents reactive context errors

  # Observer for Overview button (always present)
  observeEvent(input$res_nav_overview, {
    rv$switching_node <- TRUE  # FIX: Set flag before node switch
    rv$active_node_id <- "overview"
  }, ignoreInit = TRUE)

  observe({
    req(rv$nodes_df)
    df <- rv$nodes_df

    registry <- res_registry()
    should_lock <- res_should_lock_nodes(df, registry)
    should_hide <- res_should_hide_nodes(df, registry)

    for (i in seq_len(nrow(df))) {
      nid <- df$node_id[i]

      # Skip hidden nodes (e.g., system-generated substeps with results_hidden_system_generated)
      if (should_hide[i]) next

      # No steps are skipped - Data Processor is now navigable
      eng_id <- tolower(df$engine_id[i] %||% "")

      is_locked <- should_lock[i]

      # Only attach observer if NOT locked and not already created
      if (!is_locked && is.null(rv$nav_obs[[nid]])) {
        local({
          node_id <- nid
          btn_id <- paste0("res_nav_", gsub("[^A-Za-z0-9_]", "_", node_id))
          rv$nav_obs[[node_id]] <- observeEvent(input[[btn_id]], {
            rv$switching_node <- TRUE  # FIX: Set flag before node switch
            rv$active_node_id <- node_id
          }, ignoreInit = TRUE)
        })
      }
    }
  })
  
  # ---- Controls UI -----------------------------------------------------------
  output$res_controls_ui <- renderUI({
    req(rv$nodes_df, rv$active_node_id)

    # Overview has no style controls
    if (identical(rv$active_node_id, "overview")) return(NULL)

    eng <- active_engine_id()
    if (identical(eng, "dataprocessor")) return(NULL)

    registry <- res_registry()
    edef <- res_engine_def(eng, registry)
    if (is.null(edef)) return(div(paste("Engine not found in registry:", eng)))

    schema <- res_viewer_schema(edef)
    if (length(schema) == 0) return(div("No style controls for this engine."))

    # Filter out hidden fields (controlled elsewhere, like axis_style via global toggle)
    schema <- Filter(function(f) !isTRUE(f$hidden %||% FALSE), schema)
    if (length(schema) == 0) return(div("No style controls for this engine."))

    # Define eng_lower early for use in multiple places
    eng_lower <- tolower(eng %||% "")

    # Hide overlap plot type selector when too many groups for Venn
    if (eng_lower %in% c("idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified")) {
      res <- isolate(active_results())
      group_count <- NA_integer_
      if (!is.null(res) && !is.null(res$data$sample_meta)) {
        smeta <- res$data$sample_meta
        grp_col <- smeta$group_name %||% smeta$group
        grp_vals <- unique(as.character(grp_col %||% character()))
        grp_vals <- grp_vals[nzchar(grp_vals)]
        group_count <- length(grp_vals)
      } else if (!is.null(res$data$group_colors)) {
        group_count <- length(res$data$group_colors)
      }

      if (is.finite(group_count) && group_count > 6) {
        schema <- Filter(function(f) !identical(as.character(f$name %||% ""), "overlap_plot_type"), schema)
      }
    }

    # Avoid rendering raw label_genes_map JSON since volcano has a custom UI control
    if (identical(eng_lower, "volcano")) {
      schema <- Filter(function(f) !identical(as.character(f$name %||% ""), "label_genes_map"), schema)
      if (length(schema) == 0) return(div("No style controls for this engine."))
    }

    node <- active_node_row()
    eff <- isolate(active_effective_state())

    # Store input IDs for reactive tracking
    schema_names <- vapply(schema, function(f) as.character(f$name), character(1))
    input_ids <- vapply(schema, function(f) res_field_input_id(rv$active_node_id, f$name), character(1))
    active_input_ids(input_ids)

    view_mode_field <- NULL
    if (eng_lower %in% c("volcano", "2dgofcs")) {
      view_mode_idx <- which(vapply(schema, function(f) identical(as.character(f$name %||% ""), "view_mode"), logical(1)))
      if (length(view_mode_idx) > 0) {
        view_mode_field <- schema[[view_mode_idx[[1]]]]
        schema <- schema[-view_mode_idx[[1]]]
      }
    }

    axis_field <- NULL
    axis_idx <- which(vapply(schema, function(f) identical(as.character(f$name %||% ""), "axis_style"), logical(1)))
    if (length(axis_idx) > 0) {
      axis_field <- schema[[axis_idx[[1]]]]
      schema <- schema[-axis_idx[[1]]]
    }

    view_mode_ui <- NULL
    if (!is.null(view_mode_field)) {
      v_eff <- eff$style[[view_mode_field$name]] %||% view_mode_field$default
      view_mode_ui <- res_field_ui(rv$active_node_id, view_mode_field, value_override = v_eff)
    }

    axis_ui <- NULL
    if (!is.null(axis_field)) {
      v_eff <- eff$style[[axis_field$name]] %||% axis_field$default
      axis_ui <- res_field_ui(rv$active_node_id, axis_field, value_override = v_eff)
    }

    # Extract ontology_filter for enrichment engines and render at top of style panel
    ontology_filter_field <- NULL
    if (eng_lower %in% c("1dgofcs", "2dgofcs", "goora")) {
      ont_idx <- which(vapply(schema, function(f) identical(as.character(f$name %||% ""), "ontology_filter"), logical(1)))
      if (length(ont_idx) > 0) {
        ontology_filter_field <- schema[[ont_idx[[1]]]]
        schema <- schema[-ont_idx[[1]]]
      }
    }

    ontology_filter_ui <- NULL
    if (!is.null(ontology_filter_field)) {
      v_eff <- eff$style[[ontology_filter_field$name]] %||% ontology_filter_field$default
      ontology_filter_ui <- res_field_ui(rv$active_node_id, ontology_filter_field, value_override = v_eff)
    }

    # Extract group names from results for selected_group dropdown (hor_dis, vert_dis)
    group_choices <- NULL
    if (eng_lower %in% c("hor_dis", "vert_dis")) {
      res <- isolate(active_results())
      if (!is.null(res) && !is.null(res$data$values)) {
        grp_col <- res$data$values$group
        if (!is.null(grp_col)) {
          group_choices <- unique(as.character(grp_col))
          group_choices <- group_choices[nzchar(group_choices)]
        }
      }
    }

    # FIX: For hor_dis/vert_dis, extract selected_group field and render it at the TOP
    # This ensures the group selector is prominently visible when within_groups mode is active
    # IMPORTANT: Only show when compare_mode == "within_groups" (checked at build time since compare_mode is a params field)
    selected_group_ui <- NULL
    if (eng_lower %in% c("hor_dis", "vert_dis")) {
      # Check if compare_mode is "within_groups" from results params
      res <- isolate(active_results())
      compare_mode <- res$params$compare_mode %||% "avg_groups"

      sg_idx <- which(vapply(schema, function(f) identical(as.character(f$name %||% ""), "selected_group"), logical(1)))
      if (length(sg_idx) > 0) {
        sg_field <- schema[[sg_idx[[1]]]]
        schema <- schema[-sg_idx[[1]]]  # Remove from main schema so it's not rendered twice

        # FIX: Only build the group selector UI if compare_mode is "within_groups"
        if (compare_mode == "within_groups") {
          v_eff <- eff$style[[sg_field$name]] %||% sg_field$default
          # Use first group as default if not set
          if ((!is.null(group_choices) && length(group_choices) > 0) && (!nzchar(v_eff %||% ""))) {
            v_eff <- group_choices[[1]]
          }
          # FIX: Render the field directly without the conditional wrapper (we already checked compare_mode)
          selected_group_ui <- res_field_ui_core(rv$active_node_id, sg_field, value_override = v_eff, dynamic_choices = group_choices)
        }
      }
    }

    # =====================================================
    # Categorize fields into sections using get_style_section()
    # Fields returning NULL go at top (selectors), others into accordions
    # =====================================================
    selector_fields <- list()
    section_fields <- list(
      labels = list(),
      graph_elements = list(),
      axis_elements = list()
    )

    for (f in schema) {
      section <- get_style_section(f$name)
      if (is.null(section)) {
        selector_fields <- c(selector_fields, list(f))
      } else {
        section_fields[[section]] <- c(section_fields[[section]], list(f))
      }
    }

    # Build UI for selector fields (always visible at top, outside accordions)
    selectors_ui <- tagList(lapply(selector_fields, function(f) {
      v_eff <- eff$style[[f$name]] %||% f$default
      dyn_choices <- if (identical(f$name, "selected_group")) group_choices else NULL
      res_field_ui(rv$active_node_id, f, value_override = v_eff, dynamic_choices = dyn_choices)
    }))

    # Build accordion panels for each section
    accordions_ui <- tagList(lapply(names(STYLE_SECTIONS), function(section_name) {
      section_def <- STYLE_SECTIONS[[section_name]]
      fields_in_section <- section_fields[[section_name]]

      # Skip empty sections
      if (length(fields_in_section) == 0) return(NULL)

      # Build field UIs for this section
      field_uis <- tagList(lapply(fields_in_section, function(f) {
        v_eff <- eff$style[[f$name]] %||% f$default
        dyn_choices <- if (identical(f$name, "selected_group")) group_choices else NULL
        res_field_ui(rv$active_node_id, f, value_override = v_eff, dynamic_choices = dyn_choices)
      }))

      # Determine open state: use persisted state if available, else default
      # Use isolate() to prevent accordion state changes from triggering re-render
      section_id <- paste0("style_section_", section_name)
      persisted_open <- isolate(rv$accordion_state[[section_id]])
      is_open <- if (!is.null(persisted_open)) persisted_open else isTRUE(section_def$open)

      # Create collapsible section with field count badge
      res_collapse_section_ui(
        id = section_id,
        title = section_def$title,
        badge_text = as.character(length(fields_in_section)),
        open = is_open,
        field_uis
      )
    }))

    # Build special controls for volcano, 2dgofcs, and spearman at top of style panel
    mode_controls <- NULL
    if (eng_lower %in% c("volcano", "2dgofcs", "spearman")) {
      plot_names <- rv$current_plot_names %||% character()
      has_multi_plots <- length(plot_names) > 1

      # For volcano: get per-plot label genes from label_genes_map
      volcano_label_input <- NULL
      if (eng_lower == "volcano") {
        current_plot <- input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
        label_map_json <- eff$style$label_genes_map %||% "{}"
        label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())
        current_labels <- label_map[[current_plot]] %||% ""

        volcano_label_input <- div(
          style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #eee;",
          textAreaInput(
            "res_volcano_label_genes",
            label = paste0("Gene labels (", current_plot, ")"),
            value = as.character(current_labels),
            rows = 4,
            placeholder = "Enter gene symbols, one per line"
          )
        )
      }

      mode_controls <- tagList(
        div(
          style = "margin-bottom: 10px; padding-bottom: 10px; border-bottom: 1px solid #eee;",
          # Plot selector dropdown (only if multiple plots available)
          if (isTRUE(has_multi_plots)) {
            div(
              style = "margin-top: 8px;",
              tags$label("Comparison:", style = "font-weight: 600; font-size: 11px; display: block; margin-bottom: 2px;"),
              selectInput(
                "res_plot_pick", NULL,
                choices  = stats::setNames(plot_names, plot_names),
                selected = input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else NULL),
                width    = "100%"
              )
            )
          },
          # Per-plot gene labels for volcano
          volcano_label_input
        )
      )
    }

    tagList(
      h4("Style"),
      view_mode_ui,
      ontology_filter_ui,  # Ontology selector at top for enrichment engines (1dgofcs, 2dgofcs, goora)
      axis_ui,
      mode_controls,
      selected_group_ui,  # Group selector at top for hor_dis/vert_dis (conditional on within_groups mode)
      selectors_ui,       # Selector fields (always visible, not in accordions)
      accordions_ui       # Accordion panels for grouped fields
      # Apply button and reset override removed - updates happen automatically
    )
  })
  
  # Auto-update graphs when any style input changes (with debounce)
  .update_token <- reactiveVal(0L)

  observe({
    req(rv$loaded, rv$nodes_df, rv$active_node_id)
    input_ids <- active_input_ids()
    if (length(input_ids) == 0) return()

    # Create a reactive trigger for any input change
    lapply(input_ids, function(id) input[[id]])

    eng <- active_engine_id()
    if (identical(eng, "dataprocessor")) return(invisible(NULL))

    registry <- res_registry()
    edef <- res_engine_def(eng, registry)
    if (is.null(edef)) return(invisible(NULL))

    schema <- res_viewer_schema(edef)
    if (identical(tolower(eng %||% ""), "volcano")) {
      schema <- Filter(function(f) !identical(as.character(f$name %||% ""), "label_genes_map"), schema)
    }
    if (length(schema) == 0) return(invisible(NULL))

    node <- active_node_row()
    if (is.null(node)) return(invisible(NULL))

    kind    <- as.character(node$kind[[1]] %||% "view")
    node_id <- rv$active_node_id
    nd      <- active_node_dir()
    if (is.null(nd)) return(invisible(NULL))

    schema_names <- vapply(schema, function(f) as.character(f$name), character(1))
    input_ids_local <- vapply(schema, function(f) res_field_input_id(node_id, f$name), character(1))

    reg_defaults <- res_style_defaults(edef)

    # baseline = effective style for this node if override is cleared
    base_node <- tb_node_defaults(nd)$style %||% list()

    parent_id <- node$parent_id[[1]]
    parent_eff <- if (!is.na(parent_id) && nzchar(parent_id)) {
      res_effective_render_state_mem(rv$nodes_df, parent_id)$style %||% list()
    } else {
      list()
    }

    baseline <- modifyList(reg_defaults, parent_eff)
    baseline <- modifyList(baseline, base_node)

    # keep prior effective value for inputs that are currently hidden (conditionalPanel)
    eff_style <- tryCatch(isolate(active_effective_state()$style %||% list()), error = function(e) list())

    vals <- vector("list", length(schema_names))
    for (i in seq_along(schema_names)) {
      nm <- schema_names[[i]]
      id <- input_ids_local[[i]]
      v  <- input[[id]]

      if (is.null(v)) {
        v <- eff_style[[nm]] %||% baseline[[nm]] %||% reg_defaults[[nm]]
      }
      if (identical(nm, "view_mode") && is.logical(v)) {
        v <- if (isTRUE(v)) "interactive" else "export_preview"
      }
      vals[[i]] <- v
    }

    cur    <- stats::setNames(vals, schema_names)
    sparse <- tb_style_sparse(cur, baseline, schema_names = schema_names)

    key <- as.character(node_id)
    # Preserve non-schema style keys (e.g., volcano `label_genes_map`)
    prev_style <- rv$cache_style_by_node[[key]] %||% list()
    rv$cache_style_by_node[[key]] <- tb_style_merge_preserve_extras(prev_style, sparse, schema_names)

    prev <- rv$last_sparse_by_node[[key]] %||% NULL
    if (!isTRUE(tb_is_equal(prev, sparse))) {
      rv$last_sparse_by_node[[key]] <- sparse

      # FIX: Only mark dirty if NOT switching nodes (avoids false positives on navigation)
      if (!isTRUE(rv$switching_node)) {
        rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      }

      # Debounce: increment token and schedule delayed update
      .update_token(.update_token() + 1L)
      my_token <- .update_token()

      if (requireNamespace("later", quietly = TRUE)) {
        later::later(function() {
          if (identical(my_token, isolate(.update_token()))) {
            # Only update if no newer changes
            style_rev(isolate(style_rev()) + 1L)
          }
        }, delay = 0.3)  # 300ms delay
      } else {
        # Fallback if 'later' not available
        style_rev(isolate(style_rev()) + 1L)
      }
    }

    invisible(TRUE)
  })


  observeEvent(input$res_return_gate, {
    # Cleanup extracted dir if present
    if (!is.null(rv$exdir) && dir.exists(rv$exdir)) {
      try(unlink(rv$exdir, recursive = TRUE, force = TRUE), silent = TRUE)
    }
    
    rv$exdir <- NULL
    rv$run_root <- NULL
    rv$manifest <- NULL
    rv$nodes_df <- NULL
    rv$active_node_id <- NULL
    rv$nav_obs <- list()
    rv$last_sparse_by_node <- list()
    rv$loaded <- FALSE
  }, ignoreInit = TRUE)

  # Sync volcano per-plot gene labels to label_genes_map
  # FIX: Use debounced input to avoid re-render on each keystroke
  observeEvent(volcano_label_genes_debounced(), {
    req(rv$loaded, rv$active_node_id)
    eng <- tolower(active_engine_id() %||% "")
    if (eng != "volcano") return()

    plot_names <- rv$current_plot_names %||% character()
    current_plot <- input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
    new_labels <- volcano_label_genes_debounced() %||% ""

    # Get current label map
    eff <- isolate(active_effective_state())
    label_map_json <- eff$style$label_genes_map %||% "{}"
    label_map <- tryCatch(jsonlite::fromJSON(label_map_json, simplifyVector = FALSE), error = function(e) list())

    # Update the map with new labels for this plot
    label_map[[current_plot]] <- new_labels

    # Encode back to JSON and store in cache
    new_json <- jsonlite::toJSON(label_map, auto_unbox = TRUE)

    node_id <- rv$active_node_id
    key <- as.character(node_id)
    cur_style <- rv$cache_style_by_node[[key]] %||% list()
    cur_style$label_genes_map <- as.character(new_json)
    rv$cache_style_by_node[[key]] <- cur_style

    nd <- active_node_dir()
    if (!is.null(nd)) {
      .commit_style_debounced(node_dir = nd, payload = list(style = cur_style))
    }

    rv$has_unsaved_changes <- TRUE
    rv$save_status <- "dirty"

    # Trigger re-render
    style_rev(isolate(style_rev()) + 1L)
  }, ignoreInit = TRUE)

  observeEvent(list(rv$active_node_id, input$res_plot_pick, input$res_enrichment_tabs), {
    res_clear_label_selected()
  }, ignoreInit = TRUE)

  observeEvent(list(rv$active_node_id, input$res_plot_pick, input$res_enrichment_tabs, style_rev()), {
    rv$hover_info_by_plot <- list()
  }, ignoreInit = TRUE)

  observeEvent(input$res_label_plot_click, {
    eng <- tolower(active_engine_id() %||% "")
    if (!(eng %in% c("volcano", "2dgofcs"))) return()

    plot_names <- rv$current_plot_names %||% character()
    plot_key <- if (identical(eng, "volcano")) {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
    } else {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "2dgofcs_plot")
    }

    res_handle_label_click(input$res_label_plot_click, eng, plot_key)
  }, ignoreInit = TRUE)

  observeEvent(input$res_label_plot_dblclick, {
    eng <- tolower(active_engine_id() %||% "")
    if (!(eng %in% c("volcano", "2dgofcs"))) return()

    plot_names <- rv$current_plot_names %||% character()
    plot_key <- if (identical(eng, "volcano")) {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
    } else {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "2dgofcs_plot")
    }

    res_handle_label_dblclick(input$res_label_plot_dblclick, eng, plot_key)
  }, ignoreInit = TRUE)

  observeEvent(input$res_plot_hover, {
    eng <- tolower(active_engine_id() %||% "")
    if (!(eng %in% c("volcano", "2dgofcs"))) return()

    plot_names <- rv$current_plot_names %||% character()
    plot_key <- if (identical(eng, "volcano")) {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "volcano_plot")
    } else {
      input$res_plot_pick %||% (if (length(plot_names) > 0) plot_names[[1]] else "2dgofcs_plot")
    }

    res_handle_plot_hover(input$res_plot_hover, eng, plot_key)
  }, ignoreInit = TRUE)

  observeEvent(input$res_label_key, {
    key <- as.character(input$res_label_key$key %||% "")
    if (key %in% c("Delete", "Backspace")) {
      res_remove_selected_label()
    } else if (identical(key, "Escape")) {
      res_clear_label_selected()
    }
  }, ignoreInit = TRUE)

  observe({
    eng <- tolower(active_engine_id() %||% "")
    if (!identical(eng, "2dgofcs")) return()

    rend <- active_rendered()
    if (is.null(rend) || inherits(rend, "error")) return()

    has_tabs <- !is.null(rend$tabs) && length(rend$tabs) > 0
    if (!has_tabs) return()

    if (is.null(session$userData$res_label_click_bound)) {
      session$userData$res_label_click_bound <- new.env(parent = emptyenv())
    }
    bound <- session$userData$res_label_click_bound

    for (tab_name in rend$tabs) {
      t_normalized <- gsub(" ", "_", tolower(tab_name))
      plot_key <- paste0(t_normalized, "_plot")

      click_key <- paste0("click|", t_normalized)
      if (!exists(click_key, envir = bound, inherits = FALSE)) {
        assign(click_key, TRUE, envir = bound)
        observeEvent(input[[paste0("res_label_plot_click_", t_normalized)]], {
          res_handle_label_click(input[[paste0("res_label_plot_click_", t_normalized)]], "2dgofcs", plot_key)
        }, ignoreInit = TRUE)
      }

      dbl_key <- paste0("dbl|", t_normalized)
      if (!exists(dbl_key, envir = bound, inherits = FALSE)) {
        assign(dbl_key, TRUE, envir = bound)
        observeEvent(input[[paste0("res_label_plot_dblclick_", t_normalized)]], {
          res_handle_label_dblclick(input[[paste0("res_label_plot_dblclick_", t_normalized)]], "2dgofcs", plot_key)
        }, ignoreInit = TRUE)
      }

      hover_key <- paste0("hover|", t_normalized)
      if (!exists(hover_key, envir = bound, inherits = FALSE)) {
        assign(hover_key, TRUE, envir = bound)
        observeEvent(input[[paste0("res_plot_hover_", t_normalized)]], {
          res_handle_plot_hover(input[[paste0("res_plot_hover_", t_normalized)]], "2dgofcs", plot_key)
        }, ignoreInit = TRUE)
      }
    }
  })

  output$res_right_mode_ui <- renderUI({
    req(rv$loaded)
    eng <- tolower(active_engine_id() %||% "")

    # Only PCA has header mode toggles now; volcano and 2dgofcs controls moved to style panel
    if (eng != "pca") return(div())

    tagList(
      res_switch_input(
        "res_pca_is_scree",
        left_label = "Scores",
        right_label = "Scree",
        value = isTRUE(input$res_pca_is_scree %||% FALSE)
      )
    )
  })

  # Status indicator (green = saved, yellow = saving, red = unsaved/failed)
  output$res_status_indicator <- renderUI({
    req(rv$loaded)
    status <- rv$save_status %||% "saved"

    # Derive status from save_status
    status_class <- switch(status,
      "saved" = "saved",
      "dirty" = "unsaved",
      "saving" = "saving",
      "failed" = "failed",
      "unsaved"  # fallback
    )

    status_text <- switch(status,
      "saved" = "Up to date",
      "dirty" = "Session saved",
      "saving" = "Saving...",
      "failed" = "Save failed",
      "Unsaved changes"  # fallback
    )

    status_title <- switch(status,
      "saved" = "All changes exported to .terpbook",
      "dirty" = "Changes are captured in this session. Download Updated .terpbook to persist.",
      "saving" = "Saving changes...",
      "failed" = paste("Save failed:", rv$save_error %||% "Unknown error"),
      "Unsaved changes"
    )

    div(
      class = "res-status-indicator-wrap",
      style = "display: flex; align-items: center; gap: 6px;",
      div(
        class = paste("res-status-light", status_class),
        title = status_title
      ),
      span(
        class = paste("res-status-text", status_class),
        style = "font-size: 11px; font-weight: 600;",
        status_text
      )
    )
  })

  # axis_style is now per-engine in style panel (registry.R mk_style hidden=FALSE)
  # The style panel handler (observeEvent(input$res_style_input, ...)) already persists
  # axis_style changes per-node via .commit_style_debounced

  .commit_style_debounced <- tb_debounce_ms(350, function(node_dir, payload) {
    tb_save_render_state_atomic(node_dir, payload)
  })

  # Download updated .terpbook with all changes
  output$res_save_terpbook <- downloadHandler(
    filename = function() {
      orig_name <- rv$terpbook_filename %||% "terpbook"
      orig_name <- sub("\\.terpbook$", "", orig_name, ignore.case = TRUE)
      paste0(orig_name, "_updated_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".terpbook")
    },
    content = function(file) {
      # Validate state
      if (!isTRUE(rv$loaded) || is.null(rv$run_root) || is.null(rv$nodes_df)) {
        stop("No terpbook loaded")
      }

      if (!dir.exists(rv$run_root)) {
        stop("Run directory no longer exists: ", rv$run_root)
      }

      # Set status to saving
      rv$save_status <- "saving"
      rv$save_error <- NULL

      tryCatch({
        # Write all cached changes to disk (in the extracted temp directory)
        # Use union of all three cache keys to ensure we save nodes that only have
        # plotly or visibility changes (not just style changes)
        all_cached_node_ids <- unique(c(
          names(rv$cache_style_by_node),
          names(rv$cache_plotly_by_node),
          names(rv$cache_vis_by_node)
        ))

        nodes_written <- 0
        for (node_id in all_cached_node_ids) {
          idx <- match(node_id, rv$nodes_df$node_id)
          if (!is.finite(idx)) next

          node_dir <- rv$nodes_df$node_dir[[idx]]
          if (!dir.exists(node_dir)) {
            warning("Node directory missing, skipping: ", node_dir)
            next
          }

          payload <- list(
            style = rv$cache_style_by_node[[node_id]] %||% list(),
            plotly = rv$cache_plotly_by_node[[node_id]],
            visibility = rv$cache_vis_by_node[[node_id]]
          )

          tryCatch({
            tb_save_render_state_atomic(node_dir, payload)
            nodes_written <- nodes_written + 1
          }, error = function(e) {
            warning("Failed to save node ", node_id, ": ", conditionMessage(e))
          })
        }

        # Zip the entire run directory
        parent_dir <- dirname(rv$run_root)
        if (!dir.exists(parent_dir)) {
          stop("Parent directory does not exist: ", parent_dir)
        }

        run_folder <- basename(rv$run_root)

        # Create zip directly to the download file path
        res_safe_zip(zipfile = file, files = run_folder, root = parent_dir)

        # Validate the created zip
        if (!file.exists(file)) {
          stop("Zip file was not created")
        }

        zip_size <- file.info(file)$size
        if (is.na(zip_size) || zip_size == 0) {
          stop("Zip file is empty or unreadable")
        }

        # Mark as saved after successful download
        rv$has_unsaved_changes <- FALSE
        rv$save_status <- "saved"
        rv$save_error <- NULL

        showNotification(
          paste0("Downloaded updated .terpbook (", nodes_written, " node(s) modified, ",
                 round(zip_size / 1024 / 1024, 2), " MB)"),
          type = "message",
          duration = 10
        )

      }, error = function(e) {
        rv$save_status <- "failed"
        rv$save_error <- conditionMessage(e)
        showNotification(
          paste("Failed to create .terpbook:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        stop(e)  # Re-throw to abort download
      })
    },
    contentType = "application/zip"
  )

  # ---- Main view -------------------------------------------------------------
  output$res_main <- renderUI({
    warns <- rv$load_warnings %||% character()
    warn_ui <- NULL
    if (length(warns) > 0) {
      warn_ui <- div(
        class = "alert alert-warning",
        style = "margin: 0 0 12px 0;",
        strong("Warning"),
        tags$ul(lapply(warns, function(w) tags$li(as.character(w))))
      )
    }

    # Special handling for Overview node
    if (identical(rv$active_node_id, "overview")) {
      return(tagList(
        warn_ui,
        res_render_overview(rv$run_root, rv$manifest, rv$nodes_df, rv$terpbook_filename, rv$has_unsaved_changes)
      ))
    }

    node <- active_node_row()

    if (is.null(node)) {
      node_count <- if (!is.null(rv$nodes_df)) nrow(rv$nodes_df) else 0
      run_name <- rv$manifest$run_name %||% rv$terpbook_filename %||% "Loaded run"
      return(
        div(
          class = "res-center-stack",
          div(
            class = "res-panel res-only-panel",
            div(class = "res-panel-head"),
            div(
              class = "res-panel-body res-plot-stage",
              div(
                style = "max-width: 520px; text-align: center; display: flex; flex-direction: column; gap: 8px;",
                div(class = "msterp-gate-title", run_name),
                div(class = "msterp-gate-help", paste0(node_count, " nodes available.")),
                div(class = "msterp-gate-help", "Pick a node from the left panel to view plots and tables.")
              )
            )
          )
        )
      )
    }

    eng  <- active_engine_id()
    rend <- active_rendered()

    registry <- res_registry()
    edef <- res_engine_def(eng, registry)

    # Prefer registry-driven UI: show table panel only when render_spec declares tables.
    # Fallback to a conservative allowlist when registry isn't available.
    engines_with_table_panel_fallback <- c(
      "volcano",
      "heatmap",
      "idquant", # legacy single-node
      "idquant_id_quant", "idquant_average_value", "idquant_cv_scatter", "idquant_cv_bar",
      "idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified",
      "goora", "1dgofcs", "2dgofcs",
      "dataprocessor", "subloc"
    )
    has_table_panel <- if (!is.null(edef)) {
      declared <- edef$render_spec$tables %||% character()
      length(declared) > 0
    } else {
      tolower(eng %||% "") %in% engines_with_table_panel_fallback
    }
    eng_lower <- tolower(eng %||% "")
    panel_style <- active_effective_state()$style %||% list()
    # Hide table panel for volcano when show_summary_cards is enabled (default TRUE) and not in interactive mode
    # Summary info is shown as subtitle on the plot when enabled
    hide_volcano_table <- identical(eng_lower, "volcano") &&
      isTRUE(panel_style$show_summary_cards %||% TRUE) &&
      !isTRUE(res_is_interactive_view(panel_style))
    if (identical(eng_lower, "ftest_heatmap") || isTRUE(hide_volcano_table)) {
      has_table_panel <- FALSE
    }

    # Decide whether this engine is plot-only, plot+table, or table-only
    table_only    <- identical(tolower(eng %||% ""), "dataprocessor")
    plot_and_table <- isTRUE(has_table_panel) && !table_only

    # Extract content safely
    plots  <- if (!is.null(rend) && !inherits(rend, "error")) res_extract_ggplots(rend)   else list()
    tables <- if (!is.null(rend) && !inherits(rend, "error")) res_extract_tables(rend)    else list()

    # Check if tabs are present
    has_tabs <- !is.null(rend$tabs) && length(rend$tabs) > 0
    available_tabs <- if (has_tabs) rend$tabs else NULL
    
    # ---- Plot panel content (never collapse) ---------------------------------
    plot_body <- NULL
    if (inherits(rend, "error")) {
      plot_body <- div(
        class = "res-panel-body",
        div(class = "res-plot-stage", paste("Render failed:", conditionMessage(rend)))
      )
    } else if (is.null(active_results())) {
      plot_body <- div(
        class = "res-panel-body",
        div(class = "res-plot-stage", "No results.rds for this node.")
      )
    } else if (length(plots) == 0) {
      plot_body <- div(
        class = "res-panel-body",
        div(class = "res-plot-stage", "No ggplot plots returned for this node.")
      )
    } else {
      plot_names <- names(plots) %||% character()

      # Store plot names for right panel selector (volcano/2dgofcs)
      rv$current_plot_names <- plot_names

      # For volcano/2dgofcs: selector moves to right panel; for others: show inline if multiple
      engines_with_right_panel_selector <- c("volcano", "2dgofcs")
      show_inline_pick <- length(plot_names) > 1 &&
                          !identical(eng, "pca") &&
                          !(tolower(eng %||% "") %in% engines_with_right_panel_selector)

      # Aspect ratio from style (keeps relative output sizes representative)
      st <- active_effective_state()$style %||% list()
      w  <- suppressWarnings(as.numeric(st$width  %||% 7))
      h  <- suppressWarnings(as.numeric(st$height %||% 5))
      if (!is.finite(w) || w <= 0) w <- 7
      if (!is.finite(h) || h <= 0) h <- 5
      ar <- w / h
      view_mode <- tolower(as.character(st$view_mode %||% "export_preview"))
      plot_box_class <- "res-plot-box"
      if (tolower(eng %||% "") %in% c("volcano", "2dgofcs") && view_mode == "interactive") {
        plot_box_class <- "res-plot-box res-plot-box-free"
      }

      plot_body <- div(
        class = "res-panel-body",

        div(
          class = "res-plot-controls",

          div(
            class = "res-plot-controls-left",
            style = "display: flex; gap: 8px; align-items: center;",
            if (isTRUE(show_inline_pick)) {
              # FIX: Preserve current selection when re-rendering
              current_pick <- input$res_plot_pick
              if (is.null(current_pick) || !(current_pick %in% plot_names)) {
                current_pick <- plot_names[[1]]
              }
              selectInput(
                "res_plot_pick", NULL,
                choices  = stats::setNames(plot_names, plot_names),
                selected = current_pick,
                width    = "auto"
              )
            } else {
              NULL
            }
          ),
          div(
            class = "res-plot-controls-right",
            div()
          )
        ),

        div(
          class = "res-plot-stage",
          div(
            class = plot_box_class,
            style = sprintf("--res-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
            uiOutput("res_plot_pub")
          )
        )
      )
    }
    
    # ---- Table panel content -------------------------------------------------
    table_body <- NULL
    if (inherits(rend, "error")) {
      table_body <- div(
        class = "res-panel-body",
        div(class = "res-table-stage", "Render failed; no tables.")
      )
    } else if (is.null(active_results())) {
      table_body <- div(
        class = "res-panel-body",
        div(class = "res-table-stage", "No results.rds for this node.")
      )
    } else if (length(tables) == 0) {
      table_body <- div(
        class = "res-panel-body",
        div(class = "res-table-stage", "No tables returned for this node.")
      )
    } else {
      tbl_names <- names(tables) %||% character()
      rv$current_table_names <- tbl_names
      show_pick <- length(tbl_names) > 1
      
      plot_names_for_sync <- names(plots) %||% character()
      eng_lower <- tolower(eng %||% "")
      show_table_selector <- show_pick &&
        !(length(plot_names_for_sync) > 1 && setequal(tbl_names, plot_names_for_sync)) &&
        !identical(eng_lower, "volcano")
      
      table_body <- div(
        class = "res-panel-body",
        div(
          class = "res-plot-controls",
          div(
            class = "res-plot-controls-left",
            if (isTRUE(show_table_selector)) {
              current_tbl_pick <- input$res_table_pick
              if (is.null(current_tbl_pick) || !(current_tbl_pick %in% tbl_names)) {
                current_tbl_pick <- tbl_names[[1]]
              }
              selectInput(
                "res_table_pick", NULL,
                choices  = stats::setNames(tbl_names, tbl_names),
                selected = current_tbl_pick,
                width    = "100%"
              )
            } else {
              div()
            }
          ),
          div(
            class = "res-plot-controls-right",
            if (tolower(eng %||% "") %in% c("idquant_cv_scatter", "idquant_cv_bar")) {
              downloadButton("res_dl_cv_export", "Export CV% (.xlsx)", class = "btn-sm", style = "width: auto;")
            } else {
              div()
            }
          )
        ),
        div(
          class = "res-table-stage",
          uiOutput("res_table_pub")
        )
      )
    }
    
    # ---- Tabs (GO enrichment with BP/MF/CC) -------------------------------------
    if (has_tabs) {
      # Remove the BP/MF/CC selector panel for GO viewers: render all tabs inline.
      eng_lower <- tolower(eng %||% "")
      if (eng_lower %in% c("goora", "1dgofcs", "2dgofcs")) {
        blocks <- lapply(available_tabs, function(tab_name) {
          tab_lower <- tolower(tab_name)
          tab_normalized <- gsub(" ", "_", tab_lower)
          tab_pattern <- paste0("(^|_)", tab_normalized, "(_|$)")
          tab_plots <- plots[grepl(tab_pattern, names(plots), ignore.case = TRUE)]
          tab_tables <- tables[grepl(tab_pattern, names(tables), ignore.case = TRUE)]

          tab_plot_body <- if (length(tab_plots) == 0) {
            div(class = "res-panel-body", div(class = "res-plot-stage", "No plots for this ontology."))
          } else {
            st <- active_effective_state()$style %||% list()
            w  <- suppressWarnings(as.numeric(st$width  %||% 7))
            h  <- suppressWarnings(as.numeric(st$height %||% 5))
            if (!is.finite(w) || w <= 0) w <- 7
            if (!is.finite(h) || h <= 0) h <- 5
            ar <- w / h
            view_mode <- tolower(as.character(st$view_mode %||% "export_preview"))
            plot_box_class <- "res-plot-box"
            if (eng_lower %in% c("2dgofcs") && view_mode == "interactive") {
              plot_box_class <- "res-plot-box res-plot-box-free"
            }

            div(
              class = "res-panel-body",
              div(
                class = "res-plot-stage",
                div(
                  class = plot_box_class,
                  style = sprintf("--res-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
                  uiOutput(paste0("res_plot_pub_", tab_normalized))
                )
              )
            )
          }

          tab_table_body <- if (length(tab_tables) == 0) {
            div(class = "res-panel-body", div(class = "res-table-stage", "No tables for this ontology."))
          } else {
            div(
              class = "res-panel-body",
              div(
                class = "res-table-stage",
                uiOutput(paste0("res_table_pub_", tab_normalized))
              )
            )
          }

          div(
            class = "res-center-stack",
            div(
              class = "res-panel res-plot-panel",
              div(class = "res-panel-head"),  # Removed tab_name header for GO engines (BP/MF/CC)
              tab_plot_body
            ),
            div(
              class = "res-panel res-table-panel",
              div(class = "res-panel-head"),
              tab_table_body
            )
          )
        })

        return(div(class = "res-center-stack", blocks))
      }

      # Create tab panels for each available tab
      tab_panels <- lapply(available_tabs, function(tab_name) {
        # Filter plots and tables for this tab
        # Normalize tab name: lowercase, spaces to underscores for matching plot/table keys
        tab_lower <- tolower(tab_name)
        tab_normalized <- gsub(" ", "_", tab_lower)
        # Match keys like heatmap_bp_up_plot or bp_up_plot (with or without heatmap_ prefix)
        tab_pattern <- paste0("(^|_)", tab_normalized, "(_|$)")
        tab_plots <- plots[grepl(tab_pattern, names(plots), ignore.case = TRUE)]
        tab_tables <- tables[grepl(tab_pattern, names(tables), ignore.case = TRUE)]

        # Build plot body for this tab
        tab_plot_body <- if (length(tab_plots) == 0) {
          div(class = "res-panel-body", div(class = "res-plot-stage", "No plots for this tab."))
        } else {
          st <- active_effective_state()$style %||% list()
          w  <- suppressWarnings(as.numeric(st$width  %||% 7))
          h  <- suppressWarnings(as.numeric(st$height %||% 5))
          if (!is.finite(w) || w <= 0) w <- 7
          if (!is.finite(h) || h <= 0) h <- 5
          ar <- w / h
          view_mode <- tolower(as.character(st$view_mode %||% "export_preview"))
          plot_box_class <- "res-plot-box"
          if (tolower(eng %||% "") %in% c("volcano", "2dgofcs") && view_mode == "interactive") {
            plot_box_class <- "res-plot-box res-plot-box-free"
          }

          div(
            class = "res-panel-body",
            div(
              class = "res-plot-stage",
                div(
                  class = plot_box_class,
                  style = sprintf("--res-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
                  uiOutput(paste0("res_plot_pub_", tab_normalized))
               )
             )
           )
        }

        # Build table body for this tab
        tab_table_body <- if (length(tab_tables) == 0) {
          div(class = "res-panel-body", div(class = "res-table-stage", "No tables for this tab."))
        } else {
          div(
            class = "res-panel-body",
           div(
             class = "res-table-stage",
             uiOutput(paste0("res_table_pub_", tab_normalized))
            )
          )
        }

        # Return tab panel with plot + table
        tabPanel(
          tab_name,
          div(
            class = "res-center-stack",
            div(
              class = "res-panel res-plot-panel",
              div(class = "res-panel-head"),
              tab_plot_body
            ),
            div(
              class = "res-panel res-table-panel",
              div(class = "res-panel-head"),
              tab_table_body
            )
          )
        )
      })

      # Determine which tab to select (preserve user selection or default to first)
      selected_tab <- isolate(rv$enrichment_tab_selected)
      if (is.null(selected_tab) || !(selected_tab %in% available_tabs)) {
        selected_tab <- available_tabs[[1]]
      }

      # Return tabsetPanel wrapped in container
      return(
        div(
          class = "res-center-stack",
          div(
            class = "res-panel res-only-panel",
            div(class = "res-panel-head"),
             div(
               class = "res-panel-body",
               div(
                 class = "res-plot-controls",
                 div(class = "res-plot-controls-left", div()),
                 div(
                   class = "res-plot-controls-right",
                   div()
                 )
               ),
               do.call(tabsetPanel, c(list(id = "res_enrichment_tabs", selected = selected_tab), tab_panels))
             )
           )
         )
       )
    }

    # ---- Layout center stack (no tabs) -------------------------------------------
    if (table_only) {
      return(
        div(
          class = "res-center-stack",
          warn_ui,
          div(
            class = "res-panel res-only-panel",
            div(class = "res-panel-head"),
            table_body
          )
        )
      )
    }

    if (plot_and_table) {
      return(
        div(
          class = "res-center-stack",
          warn_ui,
          div(
            class = "res-panel res-plot-panel",
            div(class = "res-panel-head"),
            plot_body
          ),
          div(
            class = "res-panel res-table-panel",
            div(class = "res-panel-head"),
            table_body
          )
        )
      )
    }

    # Plot-only engines
    div(
      class = "res-center-stack",
      warn_ui,
      div(
        class = "res-panel res-only-panel",
        div(class = "res-panel-head"),
        plot_body
      )
    )
  })
  
  output$res_download_ui <- renderUI({
    rend  <- active_rendered()
    plots <- if (!is.null(rend) && !inherits(rend, "error")) res_extract_ggplots(rend) else list()
    has_plot <- length(plots) > 0
    eng <- tolower(active_engine_id() %||% "")

    tagList(
      if (has_plot) {
        div(
          class = "res-download-wrap",
          div(
            class = "res-download-buttons",
            actionButton("res_download_plot_modal", "Download plot", class = "btn-primary")
          )
        )
      },
      if (identical(eng, "dataprocessor")) {
        uiOutput("res_dp_substep_download_ui")
      },
      div(
        class = "res-download-wrap",
        style = "margin-top: 10px;",
        div(
          class = "res-download-buttons",
          if (has_plot) {
            actionButton("res_copy_plot", "Copy plot", class = "btn-secondary")
          },
          downloadButton(
            "res_save_terpbook",
            if (isTRUE(rv$has_unsaved_changes)) "Download Updated .terpbook *" else "Download Updated .terpbook",
            class = if (isTRUE(rv$has_unsaved_changes)) "btn-primary" else ""
          )
        )
      )
    )
  })

  output$res_dp_substep_download_ui <- renderUI({
    eng <- tolower(active_engine_id() %||% "")
    if (!identical(eng, "dataprocessor")) return(NULL)

    res <- active_results()
    ss <- res$data$substep_summary %||% NULL
    if (is.null(ss) || !is.data.frame(ss) || nrow(ss) == 0) {
      return(div(class = "res-download-wrap", div(class = "res-download-buttons", tags$small("No data processor substeps found."))))
    }

    steps <- suppressWarnings(as.integer(ss$Step %||% seq_len(nrow(ss))))
    ops <- as.character(ss$Operation %||% "")

    btns <- lapply(seq_len(nrow(ss)), function(i) {
      step_i <- steps[[i]]
      if (!is.finite(step_i) || step_i < 1) step_i <- as.integer(i)
      op_i <- ops[[i]]
      lab <- if (is.finite(step_i) && nzchar(op_i)) {
        paste0("Substep ", step_i, ": ", op_i, " (.xlsx)")
      } else if (is.finite(step_i)) {
        paste0("Substep ", step_i, " (.xlsx)")
      } else {
        paste0("Substep ", i, " (.xlsx)")
      }
      downloadButton(paste0("res_dl_dp_step_", step_i), lab, class = "btn-sm", style = "width: 100%;")
    })

    div(
      class = "res-download-wrap",
      style = "margin-top: 10px;",
      div(
        class = "res-download-buttons",
        tags$div(style = "font-weight: 700; margin-bottom: 6px;", "Data Processor (per substep)"),
        btns
      )
    )
  })
  
  res_active_plot_name <- reactive({
    rend <- active_rendered()
    if (inherits(rend, "error") || is.null(rend)) return(NULL)
    
    plots <- res_extract_ggplots(rend)
    if (length(plots) == 0) return(NULL)
    
    eng <- active_engine_id()
    
    if (identical(eng, "pca")) {
      is_scree <- isTRUE(input$res_pca_is_scree %||% FALSE)
      if (is_scree && ("pca_scree" %in% names(plots))) return("pca_scree")
      if (!is_scree && ("pca_scores" %in% names(plots))) return("pca_scores")
    }
    
    pick <- input$res_plot_pick
    if (!is.null(pick) && nzchar(pick) && pick %in% names(plots)) return(pick)
    
    names(plots)[[1]]
  })

  res_current_plot_output_id <- function() {
    dpi <- suppressWarnings(as.integer(rv$preview_dpi %||% 150L))
    use_hi <- is.finite(dpi) && dpi >= 300

    rend <- active_rendered()
    eng <- tolower(active_engine_id() %||% "")

    if (!is.null(rend) && !inherits(rend, "error") && !is.null(rend$tabs) && length(rend$tabs) > 0) {
      tab_name <- input$res_enrichment_tabs %||% rend$tabs[[1]]
      tab_norm <- gsub(" ", "_", tolower(tab_name))
      return(if (use_hi) paste0("res_plot_hi_", tab_norm) else paste0("res_plot_", tab_norm))
    }

    plot_key <- res_active_plot_name() %||% ""
    if (grepl("^(bp|mf|cc)_plot$", plot_key, ignore.case = TRUE)) {
      tab_norm <- sub("_plot$", "", tolower(plot_key))
      return(if (use_hi) paste0("res_plot_hi_", tab_norm) else paste0("res_plot_", tab_norm))
    }

    if (use_hi) "res_plot_hi" else "res_plot"
  }

  res_export_label <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("_", " ", x, fixed = TRUE)
    x <- gsub("\\s+", " ", x, perl = TRUE)
    trimws(x)
  }

  res_export_engine_label <- function(eng_id, registry) {
    eng_id <- tolower(as.character(eng_id %||% ""))
    registry <- registry %||% list()
    engines <- registry$engines %||% list()
    edef <- engines[[eng_id]] %||% list()
    label <- as.character(edef$label %||% eng_id)
    label <- gsub("\\s+", " ", label, perl = TRUE)
    trimws(label)
  }

  res_export_plot_label <- function(eng_id, plot_key, res, style, force_plot_label = FALSE) {
    eng_id <- tolower(as.character(eng_id %||% ""))
    plot_key <- as.character(plot_key %||% "")
    res <- res %||% list()
    res$data <- res$data %||% list()
    res$params <- res$params %||% list()
    style <- style %||% list()
    if (!nzchar(plot_key)) return(NULL)

    if (eng_id == "volcano") {
      if (identical(plot_key, "volcano_plot")) return(NULL)
      return(res_export_label(plot_key))
    }

    if (eng_id %in% c("goora", "1dgofcs", "2dgofcs")) {
      if (grepl("^(bp|mf|cc)_plot$", plot_key, ignore.case = TRUE)) {
        return(toupper(sub("_plot$", "", plot_key, ignore.case = TRUE)))
      }
    }

    if (eng_id == "pca") {
      if (identical(plot_key, "pca_scores")) return("Scores")
      if (identical(plot_key, "pca_scree")) return("Scree")
    }

    if (eng_id %in% c("idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified")) {
      plot_type <- tolower(as.character(style$overlap_plot_type %||% res$params$overlap_plot_type %||% "upset"))
      group_count <- NA_integer_
      if (!is.null(res$data$sample_meta)) {
        smeta <- res$data$sample_meta
        grp_col <- smeta$group_name %||% smeta$group
        grp_vals <- unique(as.character(grp_col %||% character()))
        grp_vals <- grp_vals[nzchar(grp_vals)]
        group_count <- length(grp_vals)
      } else if (!is.null(res$data$group_colors)) {
        group_count <- length(res$data$group_colors)
      }
      if (is.finite(group_count) && group_count > 6) plot_type <- "upset"
      return(if (plot_type == "venn") "Venn" else "Upset")
    }

    if (isTRUE(force_plot_label)) {
      label <- res_export_label(plot_key)
      label <- sub("\\s*plot$", "", label, ignore.case = TRUE)
      return(label)
    }

    NULL
  }

  res_export_graph_name <- function(node_id, eng_id, plot_key, registry, res, style, node_numbers, force_plot_label = FALSE) {
    node_id <- as.character(node_id %||% "")
    eng_id <- tolower(as.character(eng_id %||% ""))
    plot_key <- as.character(plot_key %||% "")
    node_numbers <- node_numbers %||% list()

    step_num <- as.character(node_numbers[[node_id]] %||% "")
    step_num <- gsub("\\[|\\]", "", step_num)
    if (!nzchar(step_num)) step_num <- node_id

    eng_label <- res_export_engine_label(eng_id, registry)
    plot_label <- res_export_plot_label(eng_id, plot_key, res, style, force_plot_label = force_plot_label)

    parts <- c(step_num, eng_label, plot_label)
    parts <- parts[nzchar(parts)]
    paste(parts, collapse = "-")
  }

  res_export_filename <- function(base) {
    base <- as.character(base %||% "")
    base <- gsub("[:/\\\\<>\"?*|]", "-", base)
    base <- gsub("\\s+", " ", base, perl = TRUE)
    trimws(base)
  }

  res_build_effective_state <- function(node_id, node_dir) {
    disk_state <- tb_load_render_state(node_dir)
    effective_state <- list(
      style = disk_state$style %||% list(),
      plotly = disk_state$plotly %||% list(),
      visibility = disk_state$visibility %||% list()
    )

    if (node_id %in% names(rv$cache_style_by_node)) {
      effective_state$style <- modifyList(effective_state$style, rv$cache_style_by_node[[node_id]] %||% list())
    }
    if (node_id %in% names(rv$cache_plotly_by_node)) {
      effective_state$plotly <- modifyList(effective_state$plotly %||% list(), rv$cache_plotly_by_node[[node_id]] %||% list())
    }
    if (node_id %in% names(rv$cache_vis_by_node)) {
      effective_state$visibility <- modifyList(effective_state$visibility %||% list(), rv$cache_vis_by_node[[node_id]] %||% list())
    }

    effective_state
  }
  
  res_safe_num <- function(x, default, label) {
    if (is.null(x)) return(default)
    if (length(x) != 1) {
      message("[Result Viewer] ", label, " length=", length(x), " value=", paste(x, collapse = ","))
    }
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) {
      message("[Result Viewer] Non-numeric ", label, ": class=", paste(class(x), collapse = "/"),
              " value=", paste(x, collapse = ","))
      return(default)
    }
    v
  }

  res_safe_int <- function(x, default, label) {
    v <- res_safe_num(x, default, label)
    v <- suppressWarnings(as.integer(v))
    if (!is.finite(v) || v <= 0) default else v
  }

  res_plot_dim_px <- function(output_id, dpi, st, use_client = FALSE) {
    if (isTRUE(use_client)) {
      w_px <- suppressWarnings(as.integer(session$clientData[[paste0("output_", output_id, "_width")]]))
      h_px <- suppressWarnings(as.integer(session$clientData[[paste0("output_", output_id, "_height")]]))
      if (is.finite(w_px) && w_px > 0 && is.finite(h_px) && h_px > 0) {
        return(list(w = w_px, h = h_px))
      }
    }

    w_in <- res_safe_num(st$width, 7, "style$width")
    h_in <- res_safe_num(st$height, 5, "style$height")

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(w = as.integer(round(w_px * scale)), h = as.integer(round(h_px * scale)))
  }

  # NOTE: The `res` parameter must be a static numeric value, not a function.

  # In Shiny's renderPlot, `width` and `height` can be functions, but `res` cannot.
  # Passing a function to `res` causes "non-numeric argument to binary operator" error
 # because Shiny multiplies width*res internally without calling the function.
  # See: docs/KNOWN_ISSUES.md for details.
  output$res_plot <- renderPlot({
    rend <- active_rendered()

    if (inherits(rend, "error") || is.null(rend)) {
      plot.new()
      text(0.5, 0.5, "Render error or no plot.")
      return(invisible(NULL))
    }

    plots <- res_extract_ggplots(rend)
    if (length(plots) == 0) {
      plot.new()
      text(0.5, 0.5, "No plot available.")
      return(invisible(NULL))
    }

    which <- res_active_plot_name()
    if (is.null(which) || !(which %in% names(plots))) which <- names(plots)[[1]]

    # FIX: Suppress messages during plot rendering (e.g., stat_ellipse "Too few points...")
    # stat_ellipse uses message() not warning(), so suppressWarnings() doesn't work
    suppressMessages(print(plots[[which]]))
  },
  # Render at export-like pixel dimensions, then shrink-to-fit via CSS
  width = function() {
    st <- active_effective_state()$style %||% list()
    eng <- tolower(active_engine_id() %||% "")
    use_client <- eng %in% c("volcano", "2dgofcs") && isTRUE(res_is_interactive_view(st))
    dims <- res_plot_dim_px("res_plot", 150L, st, use_client = use_client)
    dims$w
  },
  height = function() {
    st <- active_effective_state()$style %||% list()
    eng <- tolower(active_engine_id() %||% "")
    use_client <- eng %in% c("volcano", "2dgofcs") && isTRUE(res_is_interactive_view(st))
    dims <- res_plot_dim_px("res_plot", 150L, st, use_client = use_client)
    dims$h
  },
  res = 150  # Must be static numeric, not a function (see note above)
  )

  output$res_plot_hi <- renderPlot({
    rend <- active_rendered()

    if (inherits(rend, "error") || is.null(rend)) {
      plot.new()
      text(0.5, 0.5, "Render error or no plot.")
      return(invisible(NULL))
    }

    plots <- res_extract_ggplots(rend)
    if (length(plots) == 0) {
      plot.new()
      text(0.5, 0.5, "No plot available.")
      return(invisible(NULL))
    }

    which <- res_active_plot_name()
    if (is.null(which) || !(which %in% names(plots))) which <- names(plots)[[1]]

    suppressMessages(print(plots[[which]]))
  },
  width = function() {
    st <- active_effective_state()$style %||% list()
    eng <- tolower(active_engine_id() %||% "")
    use_client <- eng %in% c("volcano", "2dgofcs") && isTRUE(res_is_interactive_view(st))
    dims <- res_plot_dim_px("res_plot_hi", 300L, st, use_client = use_client)
    dims$w
  },
  height = function() {
    st <- active_effective_state()$style %||% list()
    eng <- tolower(active_engine_id() %||% "")
    use_client <- eng %in% c("volcano", "2dgofcs") && isTRUE(res_is_interactive_view(st))
    dims <- res_plot_dim_px("res_plot_hi", 300L, st, use_client = use_client)
    dims$h
  },
  res = 300  # Must be static numeric, not a function (see note above)
  )

  output$res_plot_pub <- renderUI({
    dpi <- suppressWarnings(as.integer(rv$preview_dpi %||% 150L))
    eng <- tolower(active_engine_id() %||% "")
    click_id <- NULL
    dblclick_id <- NULL
    hover_id <- NULL

    st <- active_effective_state()$style %||% list()
    is_interactive <- eng %in% c("volcano", "2dgofcs") && isTRUE(res_is_interactive_view(st))

    # Use plotly for interactive mode on volcano/2dgofcs
    if (is_interactive) {
      return(
        div(
          class = "res-plot-interact res-plotly-wrap",
          tabindex = "0",
          plotly::plotlyOutput("res_plotly_interactive", height = "500px", width = "100%")
        )
      )
    }

    # Non-interactive: standard ggplot rendering
    if (eng %in% c("volcano", "2dgofcs")) {
      click_id <- "res_label_plot_click"
      dblclick_id <- "res_label_plot_dblclick"
    } else if (eng %in% c("goora", "1dgofcs")) {
      click_id <- "res_go_plot_click"
    }

    plot_id <- if (is.finite(dpi) && dpi >= 300) "res_plot_hi" else "res_plot"

    div(
      class = "res-plot-interact",
      tabindex = "0",
      if (is.null(hover_id)) {
        plotOutput(plot_id, height = "100%", click = click_id, dblclick = dblclick_id)
      } else {
        plotOutput(
          plot_id,
          height = "100%",
          click = click_id,
          dblclick = dblclick_id,
          hover = hover_id
        )
      },
      uiOutput("res_hover_ui")
    )
  })

  # ============================================================
  # Plotly-based interactive plot for volcano and 2dgofcs
  # - Draggable annotations for label positioning
  # - Click events for uniprot search (volcano) or label toggle (2dgofcs)
  # ============================================================
  output$res_plotly_interactive <- plotly::renderPlotly({
    eng <- tolower(active_engine_id() %||% "")
    if (!(eng %in% c("volcano", "2dgofcs"))) return(NULL)

    st <- active_effective_state()
    style <- st$style %||% list()
    if (!isTRUE(res_is_interactive_view(style))) return(NULL)

    res <- active_results()
    if (is.null(res)) return(NULL)

    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()
    plot_key <- res_active_plot_name() %||% ""

    if (identical(eng, "volcano")) {
      state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(NULL)

      # Get comparison metadata for thresholds
      comparisons <- res$data$comparisons %||% NULL
      comp <- if (!is.null(comparisons) && plot_key %in% names(comparisons)) comparisons[[plot_key]] else NULL

      apply_fdr <- state$apply_fdr
      fc_threshold <- comp$comparison$fc_threshold %||% res$params$fc_threshold
      p_threshold <- comp$comparison$sig_threshold %||% res$params$p_threshold

      p <- tb_volcano_plotly(
        df = state$df,
        style = style,
        meta = list(visibility = visibility, plotly = plotly_state),
        xlim = state$xlim,
        ylim = state$ylim,
        labs = state$labs,
        saved = state$saved,
        comparison_name = plot_key,
        apply_fdr = apply_fdr,
        fc_threshold = fc_threshold,
        p_threshold = p_threshold
      )
      return(p)
    }

    if (identical(eng, "2dgofcs")) {
      state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
      if (is.null(state)) return(NULL)

      # Get axis labels from data
      data_obj <- res$data %||% list()
      analyses <- data_obj$analyses %||% NULL
      x_label <- "Score X"
      y_label <- "Score Y"

      if (!is.null(analyses) && plot_key %in% names(analyses)) {
        analysis <- analyses[[plot_key]]
        x_comp <- analysis$x_comparison %||% "Comparison X"
        y_comp <- analysis$y_comparison %||% "Comparison Y"
        x_label <- gsub("_vs_", "/", x_comp)
        y_label <- gsub("_vs_", "/", y_comp)
        x_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_label, ignore.case = TRUE)
        y_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_label, ignore.case = TRUE)
      } else {
        x_label <- data_obj$x_score_label %||% res$params$x_score_label %||% "Score X"
        y_label <- data_obj$y_score_label %||% res$params$y_score_label %||% "Score Y"
        x_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_label, ignore.case = TRUE)
        y_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_label, ignore.case = TRUE)
      }

      p <- tb_2dgofcs_plotly(
        df_plot = state$df_plot,
        style = style,
        meta = list(visibility = visibility, plotly = plotly_state),
        xlim = state$xlim,
        ylim = state$ylim,
        labs = state$labs,
        saved = state$saved,
        x_label = x_label,
        y_label = y_label,
        plot_key = plot_key
      )
      return(p)
    }

    NULL
  })

  # ============================================================
  # Plotly click handler - volcano: uniprot search only
  # ============================================================
  observeEvent(event_data("plotly_click", source = "volcano_plotly"), {
    click <- event_data("plotly_click", source = "volcano_plotly")
    if (is.null(click)) return()

    eng <- tolower(active_engine_id() %||% "")
    if (!identical(eng, "volcano")) return()

    st <- active_effective_state()
    style <- st$style %||% list()
    if (!isTRUE(res_is_interactive_view(style))) return()

    res <- active_results()
    if (is.null(res)) return()

    plot_key <- res_active_plot_name() %||% ""
    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()
    state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
    if (is.null(state)) return()

    # Find clicked point by index
    idx <- click$pointNumber + 1  # R is 1-indexed
    df <- state$df
    if (idx < 1 || idx > nrow(df)) return()

    gene_id <- df$gene[idx]
    if (!is.null(gene_id) && nzchar(gene_id)) {
      # Trigger uniprot search (same as before)
      res_show_uniprot_summary(gene_id)
    }
  }, ignoreInit = TRUE)

  # ============================================================
  # Plotly annotation relayout handler - capture label drag events
  # ============================================================
  observeEvent(event_data("plotly_relayout", source = "volcano_plotly"), {
    relayout_data <- event_data("plotly_relayout", source = "volcano_plotly")
    if (is.null(relayout_data)) return()

    eng <- tolower(active_engine_id() %||% "")
    if (!identical(eng, "volcano")) return()

    plot_key <- res_active_plot_name() %||% ""
    st <- active_effective_state()
    style <- st$style %||% list()
    plotly_state <- st$plotly %||% list()

    # Get current label list - must match the order annotations were created in tb_volcano_plotly
    # Annotations are created in df row order (filtered by labs), not in labs order
    res <- active_results()
    if (is.null(res)) return()
    visibility <- st$visibility %||% list()
    state <- res_volcano_plot_state(res, style, visibility, plot_key, plotly_state)
    if (is.null(state)) return()

    # Get labels in the same order as annotations (df row order, not text input order)
    # Must replicate the exact filtering done in tb_volcano_plotly
    df <- state$df
    labs_input <- res_volcano_label_list(style, plot_key)
    df_lab <- df[df$gene %in% labs_input, , drop = FALSE]
    if (nrow(df_lab) == 0) return()
    # Apply the same hide_nonsig filter as tb_volcano_plotly (line 979-981)
    if (identical(style$label_mode %||% "color_sig", "hide_nonsig")) {
      df_lab <- df_lab[df_lab$sig != "nonsig", , drop = FALSE]
    }
    if (nrow(df_lab) == 0) return()
    # This is the order annotations were created in tb_volcano_plotly
    labs <- as.character(df_lab$gene)

    # Check for annotation position changes
    # Format: annotations[0].x, annotations[0].y, etc.
    updated <- FALSE
    for (key in names(relayout_data)) {
      if (grepl("^annotations\\[\\d+\\]\\.(x|y)$", key)) {
        matches <- regmatches(key, regexec("annotations\\[(\\d+)\\]\\.(x|y)", key))[[1]]
        if (length(matches) == 3) {
          idx <- as.integer(matches[2]) + 1  # Convert to 1-indexed
          coord <- matches[3]
          value <- relayout_data[[key]]

          if (idx >= 1 && idx <= length(labs)) {
            gene <- labs[idx]

            # Update plotly state with new position (normalized from data coords to [0,1])
            res_update_plotly_labels(plot_key, function(cur_labels) {
              if (is.null(cur_labels[[gene]])) cur_labels[[gene]] <- list()

              # Normalize the new coordinate value from data coords to [0,1]
              if (coord == "x") {
                norm <- tb_normalize_coords(value, 0, state$xlim, state$ylim)
                cur_labels[[gene]]$x <- norm$x[[1]]
              } else if (coord == "y") {
                norm <- tb_normalize_coords(0, value, state$xlim, state$ylim)
                cur_labels[[gene]]$y <- norm$y[[1]]
              }

              # Store current axis ranges for denormalization on render
              cur_labels[[gene]]$x_range <- state$xlim
              cur_labels[[gene]]$y_range <- state$ylim
              cur_labels
            })
            updated <- TRUE
          }
        }
      }
    }

    if (updated) {
      rv$has_unsaved_changes <- TRUE
      rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)
    }
  }, ignoreInit = TRUE)

  # 2D GOFCS plotly relayout handlers for each standard ontology tab (BP, MF, CC)
  # These are created once at session start and listen for annotation drag events.
  # Using the same pattern as the volcano handler (observeEvent on event_data).

  # BP tab relayout handler
  observeEvent(event_data("plotly_relayout", source = "gofcs_plotly_bp_plot"), {
    relayout_data <- event_data("plotly_relayout", source = "gofcs_plotly_bp_plot")
    if (is.null(relayout_data)) return()
    res_handle_2dgofcs_relayout(relayout_data, "bp_plot")
  }, ignoreInit = TRUE)

  # MF tab relayout handler
  observeEvent(event_data("plotly_relayout", source = "gofcs_plotly_mf_plot"), {
    relayout_data <- event_data("plotly_relayout", source = "gofcs_plotly_mf_plot")
    if (is.null(relayout_data)) return()
    res_handle_2dgofcs_relayout(relayout_data, "mf_plot")
  }, ignoreInit = TRUE)

  # CC tab relayout handler
  observeEvent(event_data("plotly_relayout", source = "gofcs_plotly_cc_plot"), {
    relayout_data <- event_data("plotly_relayout", source = "gofcs_plotly_cc_plot")
    if (is.null(relayout_data)) return()
    res_handle_2dgofcs_relayout(relayout_data, "cc_plot")
  }, ignoreInit = TRUE)

  # Shared handler for 2D GOFCS relayout events (annotation dragging)
  res_handle_2dgofcs_relayout <- function(relayout_data, plot_key) {
    eng <- isolate(tolower(active_engine_id() %||% ""))
    if (!identical(eng, "2dgofcs")) return()

    st <- isolate(active_effective_state())
    style <- st$style %||% list()
    if (!isTRUE(res_is_interactive_view(style))) return()

    res <- isolate(active_results())
    if (is.null(res)) return()

    visibility <- st$visibility %||% list()
    plotly_state <- st$plotly %||% list()
    state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
    if (is.null(state)) return()

    labs <- state$labs
    if (length(labs) == 0) return()

    updated <- FALSE
    for (key in names(relayout_data)) {
      if (grepl("^annotations\\[\\d+\\]\\.(x|y)$", key)) {
        matches <- regmatches(key, regexec("annotations\\[(\\d+)\\]\\.(x|y)", key))[[1]]
        if (length(matches) == 3) {
          idx <- as.integer(matches[2]) + 1
          coord <- matches[3]
          value <- relayout_data[[key]]

          if (idx >= 1 && idx <= length(labs)) {
            df_plot <- state$df_plot
            df_lab <- df_plot[df_plot$term %in% labs, , drop = FALSE]
            if (idx <= nrow(df_lab)) {
              term_row <- df_lab[idx, , drop = FALSE]
              term_id <- as.character(term_row$term_id[1] %||% term_row$term_original[1] %||% term_row$term[1])

              res_update_plotly_labels(plot_key, function(cur_labels) {
                if (is.null(cur_labels[[term_id]])) cur_labels[[term_id]] <- list()

                if (coord == "x") {
                  norm <- tb_normalize_coords(value, 0, state$xlim, state$ylim)
                  cur_labels[[term_id]]$x <- norm$x[[1]]
                } else if (coord == "y") {
                  norm <- tb_normalize_coords(0, value, state$xlim, state$ylim)
                  cur_labels[[term_id]]$y <- norm$y[[1]]
                }

                cur_labels[[term_id]]$x_range <- state$xlim
                cur_labels[[term_id]]$y_range <- state$ylim
                cur_labels
              })
              updated <- TRUE
            }
          }
        }
      }
    }

    if (updated) {
      rv$has_unsaved_changes <- TRUE
      rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)
    }
  }

  output$res_hover_ui <- renderUI({
    eng <- tolower(active_engine_id() %||% "")
    if (!(eng %in% c("volcano", "2dgofcs"))) return(NULL)

    st <- active_effective_state()$style %||% list()
    if (!isTRUE(res_is_interactive_view(st))) return(NULL)

    plot_key <- res_active_plot_name() %||% ""
    info <- rv$hover_info_by_plot[[plot_key]] %||% res_hover_empty_state()
    res_hover_card_ui(info)
  })

  output$res_table_pub <- renderUI({
    rend <- active_rendered()
    if (is.null(rend) || inherits(rend, "error")) {
      return(div(class = "text-muted", "No tables available."))
    }

    tbls <- res_extract_tables(rend)
    if (length(tbls) == 0) {
      return(div(class = "text-muted", "No tables available."))
    }

    eng <- tolower(active_engine_id() %||% "")
    which_tbl <- input$res_table_pick %||% input$res_plot_pick
    if (is.null(which_tbl) || !nzchar(which_tbl) || !(which_tbl %in% names(tbls))) which_tbl <- names(tbls)[[1]]

    # Editable GO table (1D GOFCS / GO-ORA / 2D GOFCS)
    if (eng %in% c("1dgofcs", "goora", "2dgofcs")) {
      tbl_df_raw <- as.data.frame(tbls[[which_tbl]])
      if (!("term_id" %in% names(tbl_df_raw))) {
        return(div(class = "text-danger", "GO table is missing required column: term_id"))
      }

      term_col <- intersect(c("term", "term_name"), names(tbl_df_raw))[1]
      if (is.na(term_col) || !nzchar(term_col)) {
        return(div(class = "text-danger", "GO table is missing required term name column."))
      }

      # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
      # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().
      # DO NOT re-format here as the values are already strings and would fail as.numeric().

      term_ids <- as.character(tbl_df_raw$term_id %||% character())
      term_origs <- as.character(tbl_df_raw[[term_col]] %||% character())
      term_orig_by_id <- stats::setNames(term_origs, term_ids)

      eff_state <- active_effective_state()
      hidden_terms <- eff_state$visibility$hidden_terms %||% character()
      term_labels <- eff_state$visibility$term_labels %||% list()

      hidden_term_ids <- unique(term_ids[term_origs %in% hidden_terms])

      term_labels_by_id <- list()
      if (length(term_labels) > 0) {
        for (i in seq_along(term_ids)) {
          tid <- term_ids[[i]]
          orig <- term_origs[[i]]
          if (!nzchar(tid) || !nzchar(orig)) next
          lbl <- term_labels[[orig]] %||% NULL
          if (!is.null(lbl) && nzchar(as.character(lbl))) term_labels_by_id[[tid]] <- as.character(lbl)
        }
      }

      df_edit <- tbl_df_raw
      if (identical(term_col, "term")) {
        df_edit$term_name <- df_edit$term
        df_edit$term <- NULL
      }

      # Extract gene column data for Search button BEFORE filtering columns
      gene_col_data <- NULL
      for (gcol in c("protein_ids", "genes", "gene_ids", "geneID", "Genes")) {
        if (gcol %in% names(df_edit)) {
          gene_col_data <- as.character(df_edit[[gcol]])
          break
        }
      }

      # Remove columns that should be hidden from display (protein_ids, neglog10_fdr, etc.)
      hide_cols <- res_go_hidden_cols(eng)
      df_edit <- df_edit[, setdiff(names(df_edit), hide_cols), drop = FALSE]

      id_prefix <- "res_go_tbl"
      res_bind_editable_go_table(
        id_prefix = id_prefix,
        df = df_edit,
        term_id_col = "term_id",
        input = input,
        session = session,
        on_term_name_change = function(term_id, new_value) {
          key <- as.character(rv$active_node_id %||% "")
          if (!nzchar(key)) return()

          orig <- as.character(term_orig_by_id[[term_id]] %||% "")
          if (!nzchar(orig)) return()

          vis <- rv$cache_vis_by_node[[key]] %||% list()
          term_labels <- vis$term_labels %||% list()

          new_value <- as.character(new_value %||% "")
          if (nzchar(new_value) && new_value != orig) {
            term_labels[[orig]] <- new_value
          } else {
            term_labels[[orig]] <- NULL
          }

          vis$term_labels <- term_labels
          rv$cache_vis_by_node[[key]] <- vis

          nd <- active_node_dir()
          if (!is.null(nd)) {
            .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
          }

          rv$has_unsaved_changes <- TRUE
          rv$save_status <- "dirty"
          style_rev(isolate(style_rev()) + 1L)
        },
        on_visibility_change = function(term_id, is_visible) {
          key <- as.character(rv$active_node_id %||% "")
          if (!nzchar(key)) return()

          orig <- as.character(term_orig_by_id[[term_id]] %||% "")
          if (!nzchar(orig)) return()

          vis <- rv$cache_vis_by_node[[key]] %||% list()
          hidden_terms <- vis$hidden_terms %||% character()

          if (isTRUE(is_visible)) {
            vis$hidden_terms <- setdiff(hidden_terms, orig)
          } else {
            vis$hidden_terms <- unique(c(hidden_terms, orig))
          }
          rv$cache_vis_by_node[[key]] <- vis

          nd <- active_node_dir()
          if (!is.null(nd)) {
            .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
          }

          rv$has_unsaved_changes <- TRUE
          rv$save_status <- "dirty"
          style_rev(isolate(style_rev()) + 1L)
        }
      )

      return(
        res_editable_go_table_ui(
          id_prefix = id_prefix,
          df = df_edit,
          term_id_col = "term_id",
          term_name_col = "term_name",
          hidden_term_ids = hidden_term_ids,
          term_labels_by_id = term_labels_by_id,
          gene_col_data = gene_col_data
        )
      )
    }

    # Default: show DT table if available, else base renderTable
    if (.has_dt) {
      DT::DTOutput("res_table")
    } else {
      tableOutput("res_table_base")
    }
  })
  
  # FIX: Row click handler for GO-ORA and 1dgofcs DISABLED

    # Per task 6: Clicking term-name row should NOT trigger copy action
    # Copy/search functionality moved to 'Search for Details' modal (task 7)
    # The handler below was causing unintended modal triggers on row clicks
    # which interfered with term name editing
    #
    # observeEvent(input$res_table_rows_selected, {
    #   ... handler disabled - use Search for Details button instead ...
    # }

    # Legacy handler - disabled but kept for reference
    if (FALSE) observeEvent(input$res_table_rows_selected, {
      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs"))) return()

      row_idx <- input$res_table_rows_selected
      if (is.null(row_idx) || length(row_idx) == 0) return()

      # Get current table data
      rend <- active_rendered()
      if (is.null(rend) || inherits(rend, "error")) return()

      tbls <- res_extract_tables(rend)
      if (length(tbls) == 0) return()

      which_tbl <- input$res_table_pick
      if (is.null(which_tbl) || !nzchar(which_tbl) || !(which_tbl %in% names(tbls))) {
        which_tbl <- names(tbls)[[1]]
      }

      df <- tbls[[which_tbl]]
      if (is.null(df) || !is.data.frame(df) || nrow(df) < row_idx) return()

      # Extract term name and genes
      term_name <- as.character(df$term[row_idx])
      gene_str <- as.character(
        df$genes[row_idx] %||% df$geneID[row_idx] %||% df$core_enrichment[row_idx] %||%
        df$geneNames[row_idx] %||% df$gene_id[row_idx] %||% df$Genes[row_idx] %||% ""
      )

      if (!nzchar(gene_str)) {
        showNotification("No genes available for this term.", type = "warning")
        return()
      }

      # Parse gene list
      genes <- trimws(unlist(strsplit(gene_str, "[,;|/]")))
      genes <- genes[nzchar(genes)]

      if (length(genes) == 0) {
        showNotification("Gene list is empty for this term.", type = "warning")
        return()
      }

      # Get species
      res <- active_results()
      species <- res$params$species %||% res$meta$species %||% "human"

      # Show loading modal
      showModal(modalDialog(
        title = paste("Fetching gene info for:", term_name),
        div(
          class = "text-center",
          tags$p(paste("Looking up", length(genes), "genes in UniProt...")),
          tags$div(class = "spinner-border", role = "status")
        ),
        footer = NULL
      ))

      # Fetch UniProt info
      tryCatch({
        # Batch lookup accessions
        accs <- vapply(genes, function(g) {
          lookup_uniprot_acc_fallback(g, species)
        }, character(1))

        # Fetch info for found accessions
        gene_info_list <- lapply(seq_along(genes), function(i) {
          gene <- genes[i]
          acc <- accs[i]

          if (is.na(acc) || !nzchar(acc)) {
            return(list(gene = gene, acc = NA, function_text = "No UniProt entry"))
          }

          info <- fetch_uniprot_info(acc)
          gene_symbol <- if (!is.na(info$gene) && nzchar(info$gene)) info$gene else gene
          function_text <- if (!is.na(info$function_text) && nzchar(info$function_text)) {
            substr(info$function_text, 1, 200)  # truncate for display
          } else {
            "No annotation"
          }

          list(gene = gene_symbol, acc = acc, function_text = function_text)
        })

        # Build gene table UI
        gene_rows <- lapply(gene_info_list, function(g) {
          tags$tr(
            tags$td(tags$strong(g$gene)),
            tags$td(if (!is.na(g$acc)) tags$a(href = paste0("https://www.uniprot.org/uniprotkb/", g$acc), target = "_blank", g$acc) else tags$em("N/A")),
            tags$td(g$function_text, class = "res-uniprot-function")
          )
        })

        showModal(modalDialog(
          title = paste("Genes in term:", term_name),
          tags$div(
            tags$p(paste("Found", length(genes), "genes. Species:", species)),
            tags$div(
              class = "mb-2",
              actionButton("copy_genes_btn", "Copy Gene List", class = "btn-sm btn-primary"),
              actionButton("hide_term_btn", "Hide Term", class = "btn-sm btn-warning", style = "margin-left: 5px;"),
              tags$span(id = "copy_feedback", style = "margin-left: 10px; color: green; display: none;", "Copied!")
            ),
            tags$textarea(
              id = "gene_list_text",
              style = "position: absolute; left: -9999px;",
              paste(genes, collapse = "\n")
            ),
            tags$input(
              id = "hidden_term_name",
              type = "hidden",
              value = term_name
            ),
            tags$hr(),
            tags$div(
              style = "max-height: 400px; overflow-y: auto;",
              tags$table(
                class = "table table-sm table-striped",
                tags$thead(
                  tags$tr(
                    tags$th("Gene"),
                    tags$th("UniProt"),
                    tags$th("Function")
                  )
                ),
                tags$tbody(gene_rows)
              )
            )
          ),
          footer = modalButton("Close"),
          size = "xl",
          easyClose = TRUE
        ))
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Failed to fetch gene data:", conditionMessage(e)),
          footer = modalButton("Close")
        ))
      })
    }, ignoreInit = TRUE)

    # Hide term handlers for table clicks
    observeEvent(input$hide_term_btn, {
      term_to_hide <- input$hidden_term_name
      if (is.null(term_to_hide) || !nzchar(term_to_hide)) return()

      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs", "2dgofcs"))) return()

      # Update visibility cache
      key <- rv$active_node_id
      vis <- rv$cache_vis_by_node[[key]] %||% list()
      hidden_terms <- vis$hidden_terms %||% character()
      vis$hidden_terms <- unique(c(hidden_terms, term_to_hide))
      rv$cache_vis_by_node[[key]] <- vis

      # Save to file
      nd <- active_node_dir()
      if (!is.null(nd)) {
        .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
      }

      # Mark as changed and trigger re-render
      rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)

      # Close modal and show feedback
      removeModal()
      showNotification(paste("Term hidden:", term_to_hide), type = "message")
    }, ignoreInit = TRUE)

    # Hide term handler for tab-based tables
    observeEvent(input$hide_term_btn_tab, {
      term_to_hide <- input$hidden_term_name_tab
      if (is.null(term_to_hide) || !nzchar(term_to_hide)) return()

      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs", "2dgofcs"))) return()

      # Update visibility cache
      key <- rv$active_node_id
      vis <- rv$cache_vis_by_node[[key]] %||% list()
      hidden_terms <- vis$hidden_terms %||% character()
      vis$hidden_terms <- unique(c(hidden_terms, term_to_hide))
      rv$cache_vis_by_node[[key]] <- vis

      # Save to file
      nd <- active_node_dir()
      if (!is.null(nd)) {
        .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
      }

      # Mark as changed and trigger re-render
      rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)

      # Close modal and show feedback
      removeModal()
      showNotification(paste("Term hidden:", term_to_hide), type = "message")
    }, ignoreInit = TRUE)

    # Visibility toggle handler for checkbox clicks in GO tables (reversible show/hide)
    observeEvent(input$go_term_visibility_toggle, {
      evt <- input$go_term_visibility_toggle
      if (is.null(evt) || is.null(evt$term)) return()
      term_name <- evt$term
      is_visible <- isTRUE(evt$visible)

      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs", "2dgofcs"))) return()

      # Update visibility cache
      key <- rv$active_node_id
      vis <- rv$cache_vis_by_node[[key]] %||% list()
      hidden_terms <- vis$hidden_terms %||% character()

      if (is_visible) {
        # Remove from hidden list (show the term)
        vis$hidden_terms <- setdiff(hidden_terms, term_name)
      } else {
        # Add to hidden list (hide the term)
        vis$hidden_terms <- unique(c(hidden_terms, term_name))
      }
      rv$cache_vis_by_node[[key]] <- vis

      # Save to file
      nd <- active_node_dir()
      if (!is.null(nd)) {
        .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
      }

      # Mark as changed and trigger re-render
      rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)

      action <- if (is_visible) "shown" else "hidden"
      showNotification(paste("Term", action, ":", term_name), type = "message")
    }, ignoreInit = TRUE)

    # Track GO enrichment tab selection to preserve after re-render
    observeEvent(input$res_enrichment_tabs, {
      selected <- input$res_enrichment_tabs
      if (!is.null(selected) && nzchar(selected)) {
        rv$enrichment_tab_selected <- selected
      }
    }, ignoreInit = TRUE)

    res_show_uniprot_summary <- function(gene_id, term_name = NULL, acc = NULL) {
      gene_id <- as.character(gene_id %||% "")
      term_name <- as.character(term_name %||% "")
      acc <- as.character(acc %||% NA_character_)
      if (!nzchar(gene_id)) return(invisible(NULL))

      # Show loading modal while fetching UniProt data
      showModal(modalDialog(
        title = paste("Fetching information for:", gene_id),
        div(
          class = "text-center",
          tags$p("Looking up UniProt data..."),
          tags$div(class = "spinner-border", role = "status",
                   tags$span(class = "sr-only", "Loading..."))
        ),
        footer = NULL,
        easyClose = FALSE
      ))

      species <- rv$manifest$species %||% "human"
      if (is.na(acc) || !nzchar(acc)) acc <- .uniprot_acc_cached(gene_id, species)
      info <- .uniprot_info_cached(acc)
      summary <- as.character(info$function_text %||% "")

      if (!nzchar(summary) || is.na(summary)) summary <- "No summary available."

      # Replace loading modal with results
      showModal(modalDialog(
        title = "UniProt Summary",
        div(
          if (nzchar(term_name)) tags$p(tags$strong("GO term:"), " ", term_name),
          tags$p(tags$strong("Gene:"), " ", gene_id),
          tags$p(
            tags$strong("UniProt:"),
            " ",
            if (!is.na(acc) && nzchar(acc)) {
              tags$a(href = paste0("https://www.uniprot.org/uniprotkb/", acc), target = "_blank", acc)
            } else {
              tags$span("Not found")
            }
          ),
          tags$hr(),
          tags$div(class = "res-uniprot-summary", summary),
          tags$hr(),
          tags$p(style = "font-size: 12px; color: #666;", "Source: UniProt REST API (uniprot.org)")
        ),
        footer = modalButton("Close"),
        size = "l",
        easyClose = TRUE
      ))
      invisible(NULL)
    }

    # Handle search genes button click from GO enrichment tables
    # Shows UniProt lookup modal for the genes in the selected term
    observeEvent(input$go_uniprot_summary_click, {
      evt <- input$go_uniprot_summary_click
      if (is.null(evt) || is.null(evt$gene)) return()

      term_name <- as.character(evt$term %||% "")
      gene_id <- as.character(evt$gene %||% "")
      acc <- as.character(evt$acc %||% NA_character_)

      if (!nzchar(gene_id) || is.na(gene_id)) return()

      res_show_uniprot_summary(gene_id, term_name = term_name, acc = acc)
    }, ignoreInit = TRUE)

    .go_gene_state <- reactiveValues(term = NULL, genes = character(), page = 1L, species = NULL)

    show_go_term_modal <- function(term_name, gene_list, species) {
      term_name <- as.character(term_name %||% "")
      gene_list <- as.character(gene_list %||% "")
      species <- as.character(species %||% "human")

      if (!nzchar(term_name)) return()

      genes <- trimws(unlist(strsplit(gene_list, "[,;|/[:space:]]+")))
      genes <- unique(genes[nzchar(genes)])
      if (length(genes) == 0) {
        showModal(modalDialog(
          title = paste0("Genes in term: ", term_name),
          tags$p("No genes found."),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
        return()
      }

      .go_gene_state$term <- term_name
      .go_gene_state$genes <- genes
      .go_gene_state$page <- 1L
      .go_gene_state$species <- species
      # FIX: Generate new session ID and set active_session to enable fetching
      # This is critical - active_session must be non-NULL for fetching to work
      new_session <- as.character(Sys.time())
      .go_gene_state$fetch_session <- new_session
      .go_gene_state$active_session <- new_session  # Reset active_session for new search
      .show_go_gene_modal()
    }

    .show_go_gene_modal <- function() {
      term_name <- as.character(.go_gene_state$term %||% "")
      genes <- .go_gene_state$genes %||% character()
      genes <- genes[nzchar(genes)]
      if (!nzchar(term_name) || length(genes) == 0) return()

      per_page <- 25L
      n_pages <- max(1L, as.integer(ceiling(length(genes) / per_page)))
      page <- suppressWarnings(as.integer(.go_gene_state$page %||% 1L))
      if (!is.finite(page) || page < 1L) page <- 1L
      if (page > n_pages) page <- n_pages
      .go_gene_state$page <- page

      idx_start <- (page - 1L) * per_page + 1L
      idx_end <- min(length(genes), page * per_page)
      genes_page <- genes[idx_start:idx_end]

      species <- as.character(.go_gene_state$species %||% rv$manifest$species %||% "human")

      genes_str <- paste(genes, collapse = "\n")
      genes_attr <- gsub("\"", "&quot;", genes_str)

      # Mark that we're loading data - this triggers the reactive uiOutput
      .go_gene_state$loading <- TRUE
      .go_gene_state$genes_page <- genes_page

      # Show modal immediately with loading indicator via uiOutput
      showModal(modalDialog(
        title = paste0("Genes in term: ", term_name, " (", page, "/", n_pages, ")"),
        tags$div(
          tags$p(paste("Showing", length(genes_page), "of", length(genes), "genes. Species:", species)),
          tags$div(
            class = "mb-2",
            tags$button(
              type = "button",
              class = "btn btn-sm btn-default go-copy-genes-btn",
              `data-genes` = genes_attr,
              title = "Copy gene list",
              tags$i(class = "fa fa-copy"),
              " Copy gene list"
            )
          ),
          tags$hr(),
          tags$div(
            style = "max-height: 400px; overflow-y: auto;",
            uiOutput("go_gene_modal_content")
          )
        ),
        footer = tagList(
          actionButton("res_go_gene_prev", "Previous", class = "btn-sm"),
          actionButton("res_go_gene_next", "Next", class = "btn-sm"),
          modalButton("Close")
        ),
        size = "xl",
        easyClose = TRUE
      ))
      # FIX: Add JavaScript to notify server when modal is closed (for cancelling fetch)
      # Use tags$script instead of shinyjs::runjs since shinyjs is not initialized
      session$sendCustomMessage("go_gene_modal_init", list(timestamp = as.numeric(Sys.time())))
    }

    # Reactive output for modal content - progressive loading with visible status
    # FIX: This renderUI uses invalidateLater for progressive fetching.
    # Cancellation is checked via .go_gene_state$active_session which is set
    # to NULL when the modal closes.
    output$go_gene_modal_content <- renderUI({
      # FIX: Take a reactive dependency on active_session so we re-run when it changes
      # This is CRITICAL for cancellation to work properly
      active_session <- .go_gene_state$active_session
      if (is.null(active_session)) {
        # Session cancelled - stop fetching immediately, return nothing
        return(NULL)
      }

      # FIX: Also depend on loading to trigger initial render
      loading <- .go_gene_state$loading

      # Check if we have valid state (use isolate for these as they don't change during fetch)
      term_name <- as.character(isolate(.go_gene_state$term) %||% "")
      if (!nzchar(term_name)) return(NULL)

      genes_page <- isolate(.go_gene_state$genes_page) %||% character()
      if (length(genes_page) == 0) return(NULL)

      species <- as.character(isolate(.go_gene_state$species) %||% rv$manifest$species %||% "human")

      # Store current fetch session to detect if a NEW search started
      current_session <- isolate(.go_gene_state$fetch_session)

      # Initialize fetched results cache if starting fresh
      if (isTRUE(loading)) {
        .go_gene_state$loading <- FALSE
        .go_gene_state$fetched_idx <- 0L
        .go_gene_state$fetched_results <- list()
        .go_gene_state$active_session <- current_session  # Track which session is active
        shiny::invalidateLater(10)  # Start fetching very quickly
        return(tags$div(
          class = "text-center py-4",
          tags$i(class = "fa fa-spinner fa-spin fa-2x"),
          tags$p(class = "mt-2 text-muted", "Retrieving UniProt annotations..."),
          tags$p(class = "text-muted", tags$small(0, " / ", length(genes_page), " genes"))
        ))
      }

      # Check if a different search session started (user clicked a different term)
      if (!identical(active_session, current_session)) {
        # Session changed - stop fetching this one
        return(tags$p(class = "text-muted", "Search cancelled."))
      }

      # Progressive fetching: fetch one gene per render cycle
      fetched_idx <- isolate(.go_gene_state$fetched_idx) %||% 0L
      fetched_results <- isolate(.go_gene_state$fetched_results) %||% list()

      if (fetched_idx < length(genes_page)) {
        # Fetch next gene
        next_idx <- fetched_idx + 1L
        gene <- genes_page[[next_idx]]

        # FIX: Re-check cancellation (active_session already read as reactive dependency)
        if (is.null(active_session)) {
          return(NULL)  # Cancelled - stop immediately
        }

        acc <- .uniprot_acc_cached(gene, species)

        # FIX: Re-check cancellation after API call (read fresh value)
        if (is.null(isolate(.go_gene_state$active_session))) {
          return(NULL)  # Cancelled - stop immediately
        }

        if (is.na(acc) || !nzchar(acc)) {
          info_result <- list(gene = gene, acc = NA, function_text = "No UniProt entry")
        } else {
          info <- .uniprot_info_cached(acc)

          # FIX: Check cancellation after second API call
          if (is.null(isolate(.go_gene_state$active_session))) {
            return(NULL)  # Cancelled - stop immediately
          }

          gene_symbol <- if (!is.na(info$gene) && nzchar(info$gene)) info$gene else gene
          function_text <- if (!is.na(info$function_text) && nzchar(info$function_text)) {
            substr(info$function_text, 1, 200)
          } else {
            "No annotation"
          }
          info_result <- list(gene = gene_symbol, acc = acc, function_text = function_text)
        }

        fetched_results[[next_idx]] <- info_result
        .go_gene_state$fetched_idx <- next_idx
        .go_gene_state$fetched_results <- fetched_results

        # If more genes to fetch, schedule next fetch (but check cancellation first)
        if (next_idx < length(genes_page)) {
          # FIX: Re-check cancellation before scheduling next fetch
          if (is.null(isolate(.go_gene_state$active_session))) {
            return(NULL)  # Cancelled - stop immediately
          }
          shiny::invalidateLater(10)  # Fetch next gene after 10ms (fast iteration)
          return(tags$div(
            class = "text-center py-4",
            tags$i(class = "fa fa-spinner fa-spin fa-2x"),
            tags$p(class = "mt-2 text-muted", "Retrieving UniProt annotations..."),
            tags$p(class = "text-muted", tags$small(next_idx, " / ", length(genes_page), " genes"))
          ))
        }
      }

      # All genes fetched - render the table
      gene_info_list <- isolate(.go_gene_state$fetched_results) %||% list()
      if (length(gene_info_list) == 0) {
        return(tags$p(class = "text-muted", "No gene information available."))
      }

      gene_rows <- lapply(gene_info_list, function(g) {
        tags$tr(
          tags$td(tags$strong(g$gene)),
          tags$td(if (!is.na(g$acc)) tags$a(href = paste0("https://www.uniprot.org/uniprotkb/", g$acc), target = "_blank", g$acc) else tags$em("N/A")),
          tags$td(g$function_text, class = "res-uniprot-function")
        )
      })

      tags$table(
        class = "table table-sm table-striped",
        tags$thead(
          tags$tr(
            tags$th("Gene"),
            tags$th("UniProt"),
            tags$th("Function")
          )
        ),
        tags$tbody(gene_rows)
      )
    })

    # Bar chart click handler for GO-ORA and 1dGOFCS
    # NOTE: Due to limitations in Shiny's plotOutput click coordinate mapping for discrete
    # Y-axes, clicking on bar charts is not reliably mapped to the correct term.
    # Users should use the Search button in the table instead for UniProt lookup.
    .handle_go_bar_click <- function(click, tab_normalized = NULL) {
      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs"))) return(invisible(NULL))

      # Show helpful notification directing users to use the table's Search button
      showNotification(
        "Use the Search button in the table to look up gene details.",
        type = "message",
        duration = 4
      )
      invisible(NULL)
    }

    # Keep helper functions for potential future use
    .res_go_resolve_original_term <- function(display_term, term_labels) {
      display_term <- as.character(display_term %||% "")
      if (!nzchar(display_term) || length(term_labels) == 0) return(display_term)

      labels <- unlist(term_labels, use.names = TRUE)
      if (display_term %in% labels) {
        nm <- names(labels)[match(display_term, labels)]
        if (!is.na(nm) && nzchar(nm)) return(nm)
      }

      display_term
    }

    .res_go_gene_str_from_table_row <- function(df, row_idx) {
      as.character(
        df$genes[row_idx] %||% df$geneID[row_idx] %||% df$core_enrichment[row_idx] %||%
          df$geneNames[row_idx] %||% df$gene_id[row_idx] %||% df$Genes[row_idx] %||% ""
      )
    }

    # Legacy click coordinate mapping - kept for reference but not used
    # Shiny's plotOutput click coordinates for discrete Y-axes are unreliable
    .res_go_term_from_plot_click_legacy <- function(p, click) {
      # This function attempted to map click Y coordinates to discrete axis levels
      # but Shiny does not reliably report the correct coordinates for discrete axes
      NULL
    }

    # Placeholder for old handler signature
    .handle_go_bar_click_legacy <- function(click, tab_normalized, plots, tables, p, df) {
      # Legacy implementation that attempted direct click-to-term mapping
      # Replaced with table-based Search button approach
      invisible(NULL)
    }

    observeEvent(input$res_go_plot_click, {
      .handle_go_bar_click(click = input$res_go_plot_click, tab_normalized = NULL)
    }, ignoreInit = TRUE)

    observeEvent(input$go_search_genes_click, {
      evt <- input$go_search_genes_click
      if (is.null(evt) || is.null(evt$term) || is.null(evt$genes)) return()

      term_name <- as.character(evt$term %||% "")
      genes_str <- as.character(evt$genes %||% "")
      species <- rv$manifest$species %||% "human"
      show_go_term_modal(term_name = term_name, gene_list = genes_str, species = species)
    }, ignoreInit = TRUE)

    observeEvent(input$res_go_gene_prev, {
      if (is.null(.go_gene_state$term)) return()
      .go_gene_state$page <- max(1L, as.integer(.go_gene_state$page %||% 1L) - 1L)
      removeModal()
      .show_go_gene_modal()
    }, ignoreInit = TRUE)

    observeEvent(input$res_go_gene_next, {
      if (is.null(.go_gene_state$term)) return()
      .go_gene_state$page <- as.integer(.go_gene_state$page %||% 1L) + 1L
      removeModal()
      .show_go_gene_modal()
    }, ignoreInit = TRUE)

    # FIX: Cancel UniProt fetch when modal is closed
    # This observer watches for the modal close button click or backdrop click
    observeEvent(input$go_gene_modal_closed, {
      # Invalidate the active session to stop fetching
      .go_gene_state$active_session <- NULL
      .go_gene_state$term <- NULL
    }, ignoreInit = TRUE)

    # Term label edit handler for GO enrichment tables (base table)
    observeEvent(input$res_table_cell_edit, {
      info <- input$res_table_cell_edit
      if (is.null(info)) return()

      eng <- tolower(active_engine_id() %||% "")
      if (!(eng %in% c("goora", "1dgofcs", "2dgofcs"))) return()

      # Get the current table data to find original term
      rend <- active_rendered()
      if (is.null(rend) || inherits(rend, "error")) return()
      tbls <- res_extract_tables(rend)
      if (length(tbls) == 0) return()

      which_tbl <- input$res_table_pick %||% input$res_plot_pick
      if (is.null(which_tbl) || !nzchar(which_tbl) || !(which_tbl %in% names(tbls))) which_tbl <- names(tbls)[[1]]

      tbl_df_raw <- as.data.frame(tbls[[which_tbl]])
      if (!"term" %in% names(tbl_df_raw)) return()

      # Reconstruct the displayed table to get correct column mapping
      # (DT uses the column order after res_add_go_table_actions reorders columns)
      eff_state <- active_effective_state()
      hidden_terms <- eff_state$visibility$hidden_terms %||% character()
      term_labels_current <- eff_state$visibility$term_labels %||% list()
      include_search <- TRUE  # Include Search button for all GO engines
      tbl_df_display <- res_add_go_table_actions(tbl_df_raw, hidden_terms, term_labels_current, include_search = include_search)

      # Get row and column from edit info
      row_idx <- info$row
      col_name <- names(tbl_df_display)[info$col]  # info$col is 1-based from DT
      new_value <- info$value

      if (is.null(row_idx) || row_idx < 1 || row_idx > nrow(tbl_df_display)) return()
      if (col_name != "term") return()  # Only handle term column edits

      # Get original term name from the .original_term column (preserves original even after label edits)
      original_term <- tbl_df_display$.original_term[row_idx]

      # Update term_labels in visibility cache and persist to disk
      key <- as.character(rv$active_node_id)
      vis <- rv$cache_vis_by_node[[key]] %||% list()
      term_labels <- vis$term_labels %||% list()

      if (nzchar(new_value) && new_value != original_term) {
        term_labels[[original_term]] <- new_value
      } else {
        # If set back to original or empty, remove the override
        term_labels[[original_term]] <- NULL
      }
      vis$term_labels <- term_labels
      rv$cache_vis_by_node[[key]] <- vis

      # Persist to render_state.json
      nd <- active_node_dir()
      if (!is.null(nd)) {
        .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
      }

      # Mark as changed and trigger re-render
      rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)

      showNotification("Term label updated", type = "message")
    }, ignoreInit = TRUE)

    # Term label edit handlers for tab-based GO enrichment tables (BP/MF/CC)
    lapply(c("bp", "mf", "cc"), function(t_lower) {
      observeEvent(input[[paste0("res_table_", t_lower, "_cell_edit")]], {
        info <- input[[paste0("res_table_", t_lower, "_cell_edit")]]
        if (is.null(info)) return()

        eng <- tolower(active_engine_id() %||% "")
        if (!(eng %in% c("goora", "1dgofcs", "2dgofcs"))) return()

        # Get the current table data for this tab
        rend <- active_rendered()
        if (is.null(rend) || inherits(rend, "error")) return()
        if (is.null(rend$tabs) || !toupper(t_lower) %in% rend$tabs) return()

        tables <- res_extract_tables(rend)
        # Match keys like bp_table or heatmap_bp_table (with or without prefix)
        t_pattern <- paste0("(^|_)", t_lower, "(_|$)")
        t_tables <- tables[grepl(t_pattern, names(tables), ignore.case = TRUE)]
        if (length(t_tables) == 0) return()

        tbl_df_raw <- as.data.frame(t_tables[[1]])
        if (!"term" %in% names(tbl_df_raw)) return()

        # Reconstruct the displayed table to get correct column mapping
        # (DT uses the column order after res_add_go_table_actions reorders columns)
        eff_state <- active_effective_state()
        hidden_terms <- eff_state$visibility$hidden_terms %||% character()
        term_labels_current <- eff_state$visibility$term_labels %||% list()
        include_search <- TRUE  # Include Search button for all GO engines
        tbl_df_display <- res_add_go_table_actions(tbl_df_raw, hidden_terms, term_labels_current, include_search = include_search)

        # Get row and column from edit info
        row_idx <- info$row
        col_name <- names(tbl_df_display)[info$col]  # info$col is 1-based from DT
        new_value <- info$value

        if (is.null(row_idx) || row_idx < 1 || row_idx > nrow(tbl_df_display)) return()
        if (col_name != "term") return()

        # Get original term name from the .original_term column (preserves original even after label edits)
        original_term <- tbl_df_display$.original_term[row_idx]

        # Update term_labels in visibility cache and persist to disk
        key <- as.character(rv$active_node_id)
        vis <- rv$cache_vis_by_node[[key]] %||% list()
        term_labels <- vis$term_labels %||% list()

        if (nzchar(new_value) && new_value != original_term) {
          term_labels[[original_term]] <- new_value
        } else {
          term_labels[[original_term]] <- NULL
        }
        vis$term_labels <- term_labels
        rv$cache_vis_by_node[[key]] <- vis

        # Persist to render_state.json
        nd <- active_node_dir()
        if (!is.null(nd)) {
          .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
        }

        # Mark as changed and trigger re-render
        rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
        style_rev(isolate(style_rev()) + 1L)

        showNotification("Term label updated", type = "message")
      }, ignoreInit = TRUE)
    })

    # Subloc bin toggle handler (show/hide categories)
    observeEvent(input$subloc_bin_toggle, {
      evt <- input$subloc_bin_toggle
      if (is.null(evt) || is.null(evt$bin)) return()
      bin_name <- evt$bin
      is_visible <- isTRUE(evt$visible)

      eng <- tolower(active_engine_id() %||% "")
      if (eng != "subloc") return()

      # Update visibility cache
      key <- rv$active_node_id
      vis <- rv$cache_vis_by_node[[key]] %||% list()
      hidden_bins <- vis$hidden_bins %||% character()

      if (is_visible) {
        # Remove from hidden list (show)
        vis$hidden_bins <- setdiff(hidden_bins, bin_name)
      } else {
        # Add to hidden list (hide)
        vis$hidden_bins <- unique(c(hidden_bins, bin_name))
      }
      rv$cache_vis_by_node[[key]] <- vis

      # Save to file
      nd <- active_node_dir()
      if (!is.null(nd)) {
        .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
      }

      # Mark as changed and trigger re-render
      rv$has_unsaved_changes <- TRUE; rv$save_status <- "dirty"
      style_rev(isolate(style_rev()) + 1L)

      action <- if (is_visible) "shown" else "hidden"
      showNotification(paste("Category", action, ":", bin_name), type = "message")
    }, ignoreInit = TRUE)

  if (.has_dt) {
    output$res_table <- DT::renderDataTable({
      rend <- active_rendered()
      req(!is.null(rend))
      req(!inherits(rend, "error"))

      tbls <- res_extract_tables(rend)
      req(length(tbls) > 0)

      eng <- tolower(active_engine_id() %||% "")
      
      which <- input$res_table_pick %||% input$res_plot_pick
      if (is.null(which) || !nzchar(which) || !(which %in% names(tbls))) which <- names(tbls)[[1]]

      clickable <- eng %in% c("goora", "1dgofcs")
      is_go_engine <- eng %in% c("goora", "1dgofcs", "2dgofcs")

      tbl_df <- as.data.frame(tbls[[which]])

      # Add Show/Copy/Search columns for GO enrichment tables with visibility state
      term_col_check <- intersect(c("term", "term_name"), names(tbl_df))
      if (is_go_engine && length(term_col_check) > 0) {
        # Get current visibility settings from cache
        eff_state <- active_effective_state()
        hidden_terms <- eff_state$visibility$hidden_terms %||% character()
        term_labels <- eff_state$visibility$term_labels %||% list()
        include_search <- TRUE  # Include Search button for all GO engines
        tbl_df <- res_add_go_table_actions(tbl_df, hidden_terms, term_labels, include_search = include_search)
      }

      # Determine which columns to hide (.hidden and .original_term are internal; GO gene lists should not be displayed)
      hide_names <- c(".hidden", ".original_term")
      if (is_go_engine) {
        # Hide internal columns, gene lists (Search button provides access), and redundant columns
        hide_names <- unique(c(hide_names,
          "genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",  # Gene lists
          "neglog10_fdr", "neglog10fdr",  # Redundant with FDR
          "n_term",  # Internal metadata
          "n"  # Hide normalized n (use n_genes instead if present)
        ))
        # Engine-specific: hide fold_enrichment for FCS engines, hide score for ORA
        if (eng == "goora") {
          hide_names <- c(hide_names, "score", "score_x", "score_y")  # ORA doesn't use score
        } else {
          hide_names <- c(hide_names, "fold_enrichment")  # FCS engines don't use fold_enrichment
        }
      }

      # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
      # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().
      # DO NOT re-format here as the values are already strings.

      hide_cols <- which(names(tbl_df) %in% hide_names) - 1  # DT uses 0-based indexing
      num_cols <- length(names(tbl_df))

      # Find term column index for editing (0-based)
      term_col_idx <- which(names(tbl_df) == "term") - 1
      # Non-editable columns: Visible, Copy, Search, and internal columns
      # FIX Issue 10: Guard against non-GO tables that lack action columns
      action_cols <- c("Visible", "Copy", "Search")
      if (any(action_cols %in% names(tbl_df))) {
        non_editable_cols <- which(names(tbl_df) %in% c(action_cols, ".hidden", ".original_term")) - 1
      } else {
        non_editable_cols <- integer(0)
      }

      col_headers <- names(tbl_df)
      if (is_go_engine) {
        col_headers[col_headers == "term_id"] <- "Term ID"
        col_headers[col_headers == "term"] <- "Term name"
        col_headers[col_headers == "term_name"] <- "Term name"
        col_headers[col_headers == "fdr"] <- "FDR"
        col_headers[col_headers == "fold_enrichment"] <- if (eng == "goora") "Fold enrichment" else "Score"
        col_headers[col_headers == "n"] <- "n_genes"
      }

      # Find Search column index for width constraint
      search_col_idx <- which(names(tbl_df) == "Search") - 1  # 0-based

      n_rows <- nrow(tbl_df)
      disable_table_controls <- eng %in% c("idquant", "idquant_id_quant", "idquant_average_value", "volcano")
      dt_options <- list(
        pageLength = if (is.finite(n_rows) && n_rows > 0) min(25, n_rows) else 25,
        lengthMenu = list(c(10, 25, 50, 100), c(10, 25, 50, 100)),
        scrollX = TRUE,
        scrollY = "400px",  # Enable vertical scrolling with fixed height
        scrollCollapse = TRUE,  # Collapse to content height if smaller
        deferRender = TRUE,
        scroller = TRUE,
        searchDelay = 350,
        autoWidth = FALSE,
        destroy = TRUE,  # Fix column-mismatch warnings when switching nodes/tables
        columnDefs = list(
          list(visible = FALSE, targets = hide_cols),  # Hide internal/hidden columns
          if (length(search_col_idx) > 0 && search_col_idx >= 0)
            list(width = "40px", targets = search_col_idx, className = "dt-center")
        )
      )
      # Remove NULL entries from columnDefs
      dt_options$columnDefs <- Filter(Negate(is.null), dt_options$columnDefs)

      if (isTRUE(disable_table_controls)) {
        dt_options$searching <- FALSE
        dt_options$lengthChange <- FALSE
        dt_options$dom <- "tip"
      }

      # Only apply GO-specific row styling when the internal visibility column exists.
      if (".hidden" %in% names(tbl_df)) {
        dt_options$rowCallback <- DT::JS(
          "function(row, data, index) {",
          paste0("  var hiddenColIdx = ", num_cols - 2, ";"),  # .hidden is second to last
          "  if (data[hiddenColIdx] === true || data[hiddenColIdx] === 'TRUE') {",
          "    $(row).css('opacity', '0.5');",
          "    $(row).css('background-color', '#f5f5f5');",
          "  }",
          "}"
        )
      }

      DT::datatable(
        tbl_df,
        colnames = col_headers,
        selection = if (clickable) 'single' else 'none',
        escape = FALSE,  # Allow HTML in action columns
        editable = if (is_go_engine && length(term_col_idx) > 0) {
          list(target = 'cell', disable = list(columns = non_editable_cols))
        } else {
          FALSE
        },
        options = dt_options
      )
    })
  } else {
    output$res_table_base <- renderTable({
      rend <- active_rendered()
      req(!is.null(rend))
      req(!inherits(rend, "error"))

      tbls <- res_extract_tables(rend)
      req(length(tbls) > 0)

      eng <- tolower(active_engine_id() %||% "")
      which <- input$res_table_pick %||% input$res_plot_pick
      if (is.null(which) || !nzchar(which) || !(which %in% names(tbls))) which <- names(tbls)[[1]]

      tbl_df_base <- as.data.frame(tbls[[which]])
      is_go_engine <- eng %in% c("goora", "1dgofcs", "2dgofcs")

      # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
      # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().

      if (is_go_engine) {
        # Hide columns that are hidden in DT version
        hide_names <- c(".hidden", ".original_term",
          "genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",
          "neglog10_fdr", "neglog10fdr",
          "n_term",
          "n", "fold_enrichment")
        tbl_df_base <- tbl_df_base[, setdiff(names(tbl_df_base), hide_names), drop = FALSE]
      }

      tbl_df_base
    }, striped = TRUE, bordered = TRUE, spacing = "s")
  }

  # ---- Dynamic tab outputs (GO enrichment BP/MF/CC) -----------------------------
  observe({
    rend <- active_rendered()
    if (is.null(rend) || inherits(rend, "error")) return()

    has_tabs <- !is.null(rend$tabs) && length(rend$tabs) > 0
    if (!has_tabs) return()

    plots  <- res_extract_ggplots(rend)
    tables <- res_extract_tables(rend)

    for (tab_name in rend$tabs) {
      # Normalize tab name: lowercase, spaces to underscores for matching plot/table keys
      tab_lower <- tolower(tab_name)
      tab_normalized <- gsub(" ", "_", tab_lower)

      # Create plot output for this tab
      local({
        t_normalized <- tab_normalized
        # Match keys like heatmap_bp_up_plot or bp_up_plot (with or without heatmap_ prefix)
        t_pattern <- paste0("(^|_)", t_normalized, "(_|$)")
        t_plots <- plots[grepl(t_pattern, names(plots), ignore.case = TRUE)]

        # NOTE: res must be static numeric, not a function (Shiny limitation)
        output[[paste0("res_plot_", t_normalized)]] <- renderPlot({
          if (length(t_plots) == 0) {
            plot.new()
            text(0.5, 0.5, "No plot for this tab.")
            return(invisible(NULL))
          }
          # FIX: Suppress messages during plot rendering
          suppressMessages(print(t_plots[[1]]))
        },
        width = function() {
          st <- active_effective_state()$style %||% list()
          eng <- tolower(active_engine_id() %||% "")
          use_client <- eng %in% c("2dgofcs") && isTRUE(res_is_interactive_view(st))
          dims <- res_plot_dim_px(paste0("res_plot_", t_normalized), 150L, st, use_client = use_client)
          dims$w
        },
        height = function() {
          st <- active_effective_state()$style %||% list()
          eng <- tolower(active_engine_id() %||% "")
          use_client <- eng %in% c("2dgofcs") && isTRUE(res_is_interactive_view(st))
          dims <- res_plot_dim_px(paste0("res_plot_", t_normalized), 150L, st, use_client = use_client)
          dims$h
        },
        res = 150)

        output[[paste0("res_plot_hi_", t_normalized)]] <- renderPlot({
          if (length(t_plots) == 0) {
            plot.new()
            text(0.5, 0.5, "No plot for this tab.")
            return(invisible(NULL))
          }
          suppressMessages(print(t_plots[[1]]))
        },
        width = function() {
          st <- active_effective_state()$style %||% list()
          eng <- tolower(active_engine_id() %||% "")
          use_client <- eng %in% c("2dgofcs") && isTRUE(res_is_interactive_view(st))
          dims <- res_plot_dim_px(paste0("res_plot_hi_", t_normalized), 300L, st, use_client = use_client)
          dims$w
        },
        height = function() {
          st <- active_effective_state()$style %||% list()
          eng <- tolower(active_engine_id() %||% "")
          use_client <- eng %in% c("2dgofcs") && isTRUE(res_is_interactive_view(st))
          dims <- res_plot_dim_px(paste0("res_plot_hi_", t_normalized), 300L, st, use_client = use_client)
          dims$h
        },
        res = 300)

        output[[paste0("res_plot_pub_", t_normalized)]] <- renderUI({
          dpi <- suppressWarnings(as.integer(rv$preview_dpi %||% 150L))
          eng <- tolower(active_engine_id() %||% "")
          click_id <- NULL
          dblclick_id <- NULL
          hover_id <- NULL

          st <- active_effective_state()$style %||% list()
          is_interactive <- eng %in% c("2dgofcs") && isTRUE(res_is_interactive_view(st))

          # Use plotly for interactive mode on 2dgofcs
          if (is_interactive) {
            return(
              div(
                class = "res-plot-interact res-plotly-wrap",
                tabindex = "0",
                plotly::plotlyOutput(paste0("res_plotly_interactive_", t_normalized), height = "500px", width = "100%")
              )
            )
          }

          if (eng %in% c("2dgofcs")) {
            click_id <- paste0("res_label_plot_click_", t_normalized)
            dblclick_id <- paste0("res_label_plot_dblclick_", t_normalized)
          } else {
            click_id <- paste0("res_go_plot_click_", t_normalized)
          }

          plot_id <- if (is.finite(dpi) && dpi >= 300) {
            paste0("res_plot_hi_", t_normalized)
          } else {
            paste0("res_plot_", t_normalized)
          }

          div(
            class = "res-plot-interact",
            tabindex = "0",
            if (is.null(hover_id)) {
              plotOutput(plot_id, height = "100%", click = click_id, dblclick = dblclick_id)
            } else {
              plotOutput(
                plot_id,
                height = "100%",
                click = click_id,
                dblclick = dblclick_id,
                hover = hover_id
              )
            },
            uiOutput(paste0("res_hover_ui_", t_normalized))
          )
        })

        # Plotly output for interactive 2dgofcs tabbed views
        output[[paste0("res_plotly_interactive_", t_normalized)]] <- plotly::renderPlotly({
          eng <- tolower(active_engine_id() %||% "")
          if (!identical(eng, "2dgofcs")) return(NULL)

          st <- active_effective_state()
          style <- st$style %||% list()
          if (!isTRUE(res_is_interactive_view(style))) return(NULL)

          res <- active_results()
          if (is.null(res)) return(NULL)

          visibility <- st$visibility %||% list()
          plotly_state <- st$plotly %||% list()
          plot_key <- paste0(t_normalized, "_plot")

          state <- res_2dgofcs_plot_state(res, style, visibility, plot_key, plotly_state)
          if (is.null(state)) return(NULL)

          # Get axis labels from data
          data_obj <- res$data %||% list()
          analyses <- data_obj$analyses %||% NULL
          x_label <- "Score X"
          y_label <- "Score Y"

          if (!is.null(analyses) && plot_key %in% names(analyses)) {
            analysis <- analyses[[plot_key]]
            x_comp <- analysis$x_comparison %||% "Comparison X"
            y_comp <- analysis$y_comparison %||% "Comparison Y"
            x_label <- gsub("_vs_", "/", x_comp)
            y_label <- gsub("_vs_", "/", y_comp)
            x_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_label, ignore.case = TRUE)
            y_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_label, ignore.case = TRUE)
          } else {
            x_label <- data_obj$x_score_label %||% res$params$x_score_label %||% "Score X"
            y_label <- data_obj$y_score_label %||% res$params$y_score_label %||% "Score Y"
            x_label <- sub("\\s*Top\\s*\\d+\\s*$", "", x_label, ignore.case = TRUE)
            y_label <- sub("\\s*Top\\s*\\d+\\s*$", "", y_label, ignore.case = TRUE)
          }

          p <- tb_2dgofcs_plotly(
            df_plot = state$df_plot,
            style = style,
            meta = list(visibility = visibility, plotly = plotly_state),
            xlim = state$xlim,
            ylim = state$ylim,
            labs = state$labs,
            saved = state$saved,
            x_label = x_label,
            y_label = y_label,
            plot_key = plot_key
          )
          return(p)
        })

        output[[paste0("res_hover_ui_", t_normalized)]] <- renderUI({
          st <- active_effective_state()$style %||% list()
          if (!isTRUE(res_is_interactive_view(st))) return(NULL)

          plot_key <- paste0(t_normalized, "_plot")
          info <- rv$hover_info_by_plot[[plot_key]] %||% res_hover_empty_state()
          res_hover_card_ui(info)
        })

        if (is.null(session$userData$res_go_plot_click_bound)) {
          session$userData$res_go_plot_click_bound <- new.env(parent = emptyenv())
        }
        click_bound <- session$userData$res_go_plot_click_bound
        click_key <- paste0("tab|", t_normalized)
        if (!exists(click_key, envir = click_bound, inherits = FALSE)) {
          assign(click_key, TRUE, envir = click_bound)
          observeEvent(input[[paste0("res_go_plot_click_", t_normalized)]], {
            .handle_go_bar_click(click = input[[paste0("res_go_plot_click_", t_normalized)]],
                                tab_normalized = t_normalized)
          }, ignoreInit = TRUE)
        }
      })

      # Create table output for this tab
      local({
        t_normalized <- tab_normalized
        # Match keys like heatmap_bp_up_table or bp_up_table (with or without heatmap_ prefix)
        t_pattern <- paste0("(^|_)", t_normalized, "(_|$)")
        t_tables <- tables[grepl(t_pattern, names(tables), ignore.case = TRUE)]

        if (.has_dt) {
          output[[paste0("res_table_", t_normalized)]] <- DT::renderDataTable({
            if (length(t_tables) == 0) return(data.frame())

            eng <- tolower(active_engine_id() %||% "")
            clickable <- eng %in% c("goora", "1dgofcs")
            is_go_engine <- eng %in% c("goora", "1dgofcs", "2dgofcs")

            tbl_df_tab <- as.data.frame(t_tables[[1]])

            # Add Show/Copy/Search columns for GO enrichment tables with visibility state
            # Search button is excluded for 2dgofcs
            term_col_check_tab <- intersect(c("term", "term_name"), names(tbl_df_tab))
            if (is_go_engine && length(term_col_check_tab) > 0) {
              # Get current visibility settings from cache
              eff_state <- active_effective_state()
              hidden_terms <- eff_state$visibility$hidden_terms %||% character()
              term_labels <- eff_state$visibility$term_labels %||% list()
              include_search <- TRUE  # Include Search button for all GO engines
              tbl_df_tab <- res_add_go_table_actions(tbl_df_tab, hidden_terms, term_labels, include_search = include_search)
            }

            # Determine which columns to hide (.hidden and .original_term are internal; GO gene lists should not be displayed)
            hide_names <- c(".hidden", ".original_term")
            if (is_go_engine) {
              # Hide internal columns, gene lists (Search button provides access), and redundant columns
              hide_names <- unique(c(hide_names,
                "genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",  # Gene lists
                "neglog10_fdr", "neglog10fdr",  # Redundant with FDR
                "n_term",  # Internal metadata
                "n"  # Hide normalized n (use n_genes instead if present)
              ))
              # Engine-specific: hide fold_enrichment for FCS engines, hide score for ORA
              if (eng == "goora") {
                hide_names <- c(hide_names, "score", "score_x", "score_y")  # ORA doesn't use score
              } else {
                hide_names <- c(hide_names, "fold_enrichment")  # FCS engines don't use fold_enrichment
              }
            }

            # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
            # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().
            # DO NOT re-format here as the values are already strings.

            hide_cols <- which(names(tbl_df_tab) %in% hide_names) - 1  # DT uses 0-based indexing
            num_cols <- length(names(tbl_df_tab))

            # Find term column index for editing (0-based)
            term_col_idx <- which(names(tbl_df_tab) == "term") - 1
            # Non-editable columns: Visible, Copy, Search, and internal columns
            # FIX Issue 10: Guard against non-GO tables that lack action columns
            action_cols <- c("Visible", "Copy", "Search")
            if (any(action_cols %in% names(tbl_df_tab))) {
              non_editable_cols <- which(names(tbl_df_tab) %in% c(action_cols, ".hidden", ".original_term")) - 1
            } else {
              non_editable_cols <- integer(0)
            }

            col_headers <- names(tbl_df_tab)
            if (is_go_engine) {
              col_headers[col_headers == "term_id"] <- "Term ID"
              col_headers[col_headers == "term"] <- "Term name"
              col_headers[col_headers == "term_name"] <- "Term name"
              col_headers[col_headers == "fdr"] <- "FDR"
              col_headers[col_headers == "fold_enrichment"] <- if (eng == "goora") "Fold enrichment" else "Score"
              col_headers[col_headers == "n"] <- "n_genes"
            }

            # Find Search column index for width constraint
            search_col_idx_tab <- which(names(tbl_df_tab) == "Search") - 1  # 0-based

            # Build columnDefs list
            col_defs <- list(
              list(visible = FALSE, targets = hide_cols)  # Hide internal columns
            )
            if (length(search_col_idx_tab) > 0 && search_col_idx_tab >= 0) {
              col_defs <- c(col_defs, list(list(width = "40px", targets = search_col_idx_tab, className = "dt-center")))
            }

            # Build DT options
            n_rows_tab <- nrow(tbl_df_tab)
            dt_options_tab <- list(
              pageLength = if (is.finite(n_rows_tab) && n_rows_tab > 0) min(25, n_rows_tab) else 25,
              lengthMenu = list(c(10, 25, 50, 100), c(10, 25, 50, 100)),
              scrollX = TRUE,
              scrollY = "400px",  # Enable vertical scrolling with fixed height
              scrollCollapse = TRUE,  # Collapse to content height if smaller
              deferRender = TRUE,
              scroller = TRUE,
              searchDelay = 350,
              autoWidth = FALSE,
              searching = TRUE,
              ordering = TRUE,
              destroy = TRUE,  # FIX: Prevent column-mismatch warnings when switching nodes/tables
              columnDefs = col_defs
            )

            # Only apply GO-specific row styling when the internal visibility column exists
            if (".hidden" %in% names(tbl_df_tab)) {
              dt_options_tab$rowCallback <- DT::JS(
                "function(row, data, index) {",
                paste0("  var hiddenColIdx = ", num_cols - 2, ";"),  # .hidden is second to last
                "  if (data[hiddenColIdx] === true || data[hiddenColIdx] === 'TRUE') {",
                "    $(row).css('opacity', '0.5');",
                "    $(row).css('background-color', '#f5f5f5');",
                "  }",
                "}"
              )
            }

            DT::datatable(
              tbl_df_tab,
              colnames = col_headers,
              selection = if (clickable) 'single' else 'none',
              escape = FALSE,  # Allow HTML in action columns
              editable = if (is_go_engine && length(term_col_idx) > 0) {
                list(target = 'cell', disable = list(columns = non_editable_cols))
              } else {
                FALSE
              },
              options = dt_options_tab
            )
          })
        } else {
          output[[paste0("res_table_base_", t_normalized)]] <- renderTable({
            if (length(t_tables) == 0) return(data.frame())

            eng <- tolower(active_engine_id() %||% "")
            is_go_engine <- eng %in% c("goora", "1dgofcs", "2dgofcs")

            tbl_df_base_tab <- as.data.frame(t_tables[[1]])

            # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
            # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().

            if (is_go_engine) {
              # Hide columns that are hidden in DT version
              hide_names <- c(".hidden", ".original_term",
                "genes", "gene_ids", "geneID", "protein_ids", "core_enrichment", "Genes",
                "neglog10_fdr", "neglog10fdr",
                "n_term",
                "n", "fold_enrichment")
              tbl_df_base_tab <- tbl_df_base_tab[, setdiff(names(tbl_df_base_tab), hide_names), drop = FALSE]
            }

            tbl_df_base_tab
          }, striped = TRUE, bordered = TRUE, spacing = "s")
        }

        output[[paste0("res_table_pub_", t_normalized)]] <- renderUI({
          eng <- tolower(active_engine_id() %||% "")
          if (length(t_tables) == 0) return(div(class = "text-muted", "No tables for this tab."))

          if (eng %in% c("1dgofcs", "goora", "2dgofcs")) {
            tbl_df_raw <- as.data.frame(t_tables[[1]])
            if (!("term_id" %in% names(tbl_df_raw))) {
              return(div(class = "text-danger", "GO table is missing required column: term_id"))
            }
            term_col <- intersect(c("term", "term_name"), names(tbl_df_raw))[1]
            if (is.na(term_col) || !nzchar(term_col)) {
              return(div(class = "text-danger", "GO table is missing required term name column."))
            }

            # NOTE: Numeric columns (fdr, score, fold_enrichment, etc.) are already formatted
            # by tb_render_go_tab() in terpbook.R using tb_format_fdr() and tb_format_sig().
            # DO NOT re-format here as the values are already strings.

            term_ids <- as.character(tbl_df_raw$term_id %||% character())
            term_origs <- as.character(tbl_df_raw[[term_col]] %||% character())
            term_orig_by_id <- stats::setNames(term_origs, term_ids)

            eff_state <- active_effective_state()
            hidden_terms <- eff_state$visibility$hidden_terms %||% character()
            term_labels <- eff_state$visibility$term_labels %||% list()

            hidden_term_ids <- unique(term_ids[term_origs %in% hidden_terms])

            term_labels_by_id <- list()
            if (length(term_labels) > 0) {
              for (i in seq_along(term_ids)) {
                tid <- term_ids[[i]]
                orig <- term_origs[[i]]
                if (!nzchar(tid) || !nzchar(orig)) next
                lbl <- term_labels[[orig]] %||% NULL
                if (!is.null(lbl) && nzchar(as.character(lbl))) term_labels_by_id[[tid]] <- as.character(lbl)
              }
            }

            df_edit <- tbl_df_raw
            if (identical(term_col, "term")) {
              df_edit$term_name <- df_edit$term
              df_edit$term <- NULL
            }

            # Extract gene column data for Search button BEFORE filtering columns
            gene_col_data <- NULL
            for (gcol in c("protein_ids", "genes", "gene_ids", "geneID", "Genes")) {
              if (gcol %in% names(df_edit)) {
                gene_col_data <- as.character(df_edit[[gcol]])
                break
              }
            }

            # Remove columns that should be hidden from display (protein_ids, neglog10_fdr, etc.)
            hide_cols <- res_go_hidden_cols(eng)
            df_edit <- df_edit[, setdiff(names(df_edit), hide_cols), drop = FALSE]

            id_prefix <- paste0("res_go_tbl_", t_normalized)
            res_bind_editable_go_table(
              id_prefix = id_prefix,
              df = df_edit,
              term_id_col = "term_id",
              input = input,
              session = session,
              on_term_name_change = function(term_id, new_value) {
                key <- as.character(rv$active_node_id %||% "")
                if (!nzchar(key)) return()

                orig <- as.character(term_orig_by_id[[term_id]] %||% "")
                if (!nzchar(orig)) return()

                vis <- rv$cache_vis_by_node[[key]] %||% list()
                term_labels <- vis$term_labels %||% list()

                new_value <- as.character(new_value %||% "")
                if (nzchar(new_value) && new_value != orig) {
                  term_labels[[orig]] <- new_value
                } else {
                  term_labels[[orig]] <- NULL
                }

                vis$term_labels <- term_labels
                rv$cache_vis_by_node[[key]] <- vis

                nd <- active_node_dir()
                if (!is.null(nd)) {
                  .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
                }

                rv$has_unsaved_changes <- TRUE
                rv$save_status <- "dirty"
                style_rev(isolate(style_rev()) + 1L)
              },
              on_visibility_change = function(term_id, is_visible) {
                key <- as.character(rv$active_node_id %||% "")
                if (!nzchar(key)) return()

                orig <- as.character(term_orig_by_id[[term_id]] %||% "")
                if (!nzchar(orig)) return()

                vis <- rv$cache_vis_by_node[[key]] %||% list()
                hidden_terms <- vis$hidden_terms %||% character()

                if (isTRUE(is_visible)) {
                  vis$hidden_terms <- setdiff(hidden_terms, orig)
                } else {
                  vis$hidden_terms <- unique(c(hidden_terms, orig))
                }
                rv$cache_vis_by_node[[key]] <- vis

                nd <- active_node_dir()
                if (!is.null(nd)) {
                  .commit_style_debounced(node_dir = nd, payload = list(visibility = vis))
                }

                rv$has_unsaved_changes <- TRUE
                rv$save_status <- "dirty"
                style_rev(isolate(style_rev()) + 1L)
              }
            )

            return(
              res_editable_go_table_ui(
                id_prefix = id_prefix,
                df = df_edit,
                term_id_col = "term_id",
                term_name_col = "term_name",
                hidden_term_ids = hidden_term_ids,
                term_labels_by_id = term_labels_by_id,
                gene_col_data = gene_col_data
              )
            )
          }

          if (.has_dt) {
            DT::DTOutput(paste0("res_table_", t_normalized))
          } else {
            tableOutput(paste0("res_table_base_", t_normalized))
          }
        })
      })
    }
  })

  # NOTE: Table row clicks should NOT trigger gene search modals for GO engines.
  # Use ggplot bar clicks (or other explicit actions) instead.
  if (FALSE && .has_dt) {
    lapply(c("bp", "mf", "cc"), function(t_lower) {
      observeEvent(input[[paste0("res_table_", t_lower, "_rows_selected")]], {
        # Check if this tab actually exists in current rendered output
        rend <- active_rendered()
        if (is.null(rend) || inherits(rend, "error")) return()
        if (is.null(rend$tabs) || !toupper(t_lower) %in% rend$tabs) return()
            eng <- tolower(active_engine_id() %||% "")
            if (!(eng %in% c("goora", "1dgofcs"))) return()

            row_idx <- input[[paste0("res_table_", t_lower, "_rows_selected")]]
            if (is.null(row_idx) || length(row_idx) == 0) return()

            # Get table data REACTIVELY
            rend <- active_rendered()
            if (is.null(rend) || inherits(rend, "error")) return()

            tbls <- res_extract_tables(rend)
            # Match keys like heatmap_bp_up_table or bp_up_table (with or without prefix)
            t_pattern <- paste0("(^|_)", t_lower, "(_|$)")
            t_tables <- tbls[grepl(t_pattern, names(tbls), ignore.case = TRUE)]
            if (length(t_tables) == 0) return()

            df <- as.data.frame(t_tables[[1]])
            if (is.null(df) || !is.data.frame(df) || nrow(df) < row_idx) return()

            # Extract term name and genes
            term_name <- as.character(df$term[row_idx])
            gene_str <- as.character(
        df$genes[row_idx] %||% df$geneID[row_idx] %||% df$core_enrichment[row_idx] %||%
        df$geneNames[row_idx] %||% df$gene_id[row_idx] %||% df$Genes[row_idx] %||% ""
      )

            if (!nzchar(gene_str)) {
              showNotification("No genes available for this term.", type = "warning")
              return()
            }

            # Parse gene list
            genes <- trimws(unlist(strsplit(gene_str, "[,;|/]")))
            genes <- genes[nzchar(genes)]

            if (length(genes) == 0) {
              showNotification("Gene list is empty for this term.", type = "warning")
              return()
            }

            # Get species
            res <- active_results()
            species <- res$params$species %||% res$meta$species %||% "human"

            # Show loading modal
            showModal(modalDialog(
              title = paste("Fetching gene info for:", term_name),
              div(
                class = "text-center",
                tags$p(paste("Looking up", length(genes), "genes in UniProt...")),
                tags$div(class = "spinner-border", role = "status")
              ),
              footer = NULL
            ))

            # Fetch UniProt info
            tryCatch({
              # Batch lookup accessions
              accs <- vapply(genes, function(g) {
                lookup_uniprot_acc_fallback(g, species)
              }, character(1))

              # Fetch info for found accessions
              gene_info_list <- lapply(seq_along(genes), function(i) {
                gene <- genes[i]
                acc <- accs[i]

                if (is.na(acc) || !nzchar(acc)) {
                  return(list(gene = gene, acc = NA, function_text = "No UniProt entry"))
                }

                info <- fetch_uniprot_info(acc)
                gene_symbol <- if (!is.na(info$gene) && nzchar(info$gene)) info$gene else gene
                function_text <- if (!is.na(info$function_text) && nzchar(info$function_text)) {
                  substr(info$function_text, 1, 200)  # truncate for display
                } else {
                  "No annotation"
                }

                list(gene = gene_symbol, acc = acc, function_text = function_text)
              })

              # Build gene table UI
              gene_rows <- lapply(gene_info_list, function(g) {
                tags$tr(
                  tags$td(tags$strong(g$gene)),
                  tags$td(if (!is.na(g$acc)) tags$a(href = paste0("https://www.uniprot.org/uniprotkb/", g$acc), target = "_blank", g$acc) else tags$em("N/A")),
                  tags$td(g$function_text, class = "res-uniprot-function")
                )
              })

              showModal(modalDialog(
                title = paste("Genes in term:", term_name),
                tags$div(
                  tags$p(paste("Found", length(genes), "genes. Species:", species)),
                  tags$div(
                    class = "mb-2",
                    actionButton("copy_genes_btn_tab", "Copy Gene List", class = "btn-sm btn-primary"),
                    actionButton("hide_term_btn_tab", "Hide Term", class = "btn-sm btn-warning", style = "margin-left: 5px;"),
                    tags$span(id = "copy_feedback_tab", style = "margin-left: 10px; color: green; display: none;", "Copied!")
                  ),
                  tags$textarea(
                    id = "gene_list_text_tab",
                    style = "position: absolute; left: -9999px;",
                    paste(genes, collapse = "\n")
                  ),
                  tags$input(
                    id = "hidden_term_name_tab",
                    type = "hidden",
                    value = term_name
                  ),
                  tags$hr(),
                  tags$div(
                    style = "max-height: 400px; overflow-y: auto;",
                    tags$table(
                      class = "table table-sm table-striped",
                      tags$thead(
                        tags$tr(
                          tags$th("Gene"),
                          tags$th("UniProt"),
                          tags$th("Function")
                        )
                      ),
                      tags$tbody(gene_rows)
                    )
                  )
                ),
                footer = modalButton("Close"),
                size = "xl",
                easyClose = TRUE
              ))
            }, error = function(e) {
              showModal(modalDialog(
                title = "Error",
                paste("Failed to fetch gene data:", conditionMessage(e)),
                footer = modalButton("Close")
              ))
            })
          }, ignoreInit = TRUE)
    })
  }

  # Show download options modal
  observeEvent(input$res_download_plot_modal, {
    showModal(modalDialog(
      title = "Download Plot",
      tags$div(
        selectInput("download_format", "Format", choices = c("PNG", "PDF", "SVG"), selected = "PNG"),
        selectInput("download_dpi", "DPI (PNG only)", choices = c(150, 300, 600), selected = 300),
        tags$hr(),
        downloadButton("res_download_confirm", "Download", class = "btn-primary"),
        tags$span(style = "margin-left: 10px;"),
        actionButton("download_close", "Cancel", class = "btn-secondary")
      ),
      footer = NULL,
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  # Close modal on cancel
  observeEvent(input$download_close, {
    removeModal()
  }, ignoreInit = TRUE)

  # Actual download handler
  output$res_download_confirm <- downloadHandler(
    filename = function() {
      eng <- active_engine_id() %||% "plot"
      fmt <- tolower(input$download_format %||% "png")

      rend <- active_rendered()
      plots <- if (!is.null(rend) && !inherits(rend, "error")) res_extract_ggplots(rend) else list()
      plot_key <- res_active_plot_name() %||% ""

      res <- active_results()
      st <- active_effective_state()$style %||% list()
      registry <- res_registry()
      node_numbers <- res_compute_node_numbers(rv$nodes_df)
      force_plot_label <- length(plots) > 1

      base <- res_export_graph_name(rv$active_node_id, eng, plot_key, registry, res, st, node_numbers,
                                    force_plot_label = force_plot_label)
      base <- res_export_filename(base)
      if (!nzchar(base)) {
        safe_plot <- if (nzchar(plot_key)) plot_key else "plot"
        base <- paste0(eng, "_", safe_plot)
      }

      paste0(base, ".", fmt)
    },
    content = function(file) {
      rend <- active_rendered()
      if (inherits(rend, "error") || is.null(rend)) {
        stop("Render error; cannot download.")
      }

      plots <- res_extract_ggplots(rend)
      if (length(plots) == 0) {
        stop("No plot available to download.")
      }

      which <- res_active_plot_name()
      if (is.null(which) || !nzchar(which) || !(which %in% names(plots))) {
        which <- names(plots)[[1]]
      }

      p <- plots[[which]]
      st <- active_effective_state()$style %||% list()
      defs <- tb_publication_export_defaults(st)
      w <- defs$width
      h <- defs$height

      fmt <- input$download_format %||% "PNG"
      dpi <- as.numeric(input$download_dpi %||% defs$dpi)

      if (toupper(fmt) == "PDF") {
        ggplot2::ggsave(file, p, width = w, height = h, units = defs$units, device = defs$pdf_device)
      } else if (toupper(fmt) == "SVG") {
        ggplot2::ggsave(file, p, width = w, height = h, units = defs$units, device = defs$svg_device)
      } else {
        png_type <- if (capabilities("cairo")) "cairo-png" else NULL
        ggplot2::ggsave(file, p, width = w, height = h, units = defs$units, dpi = dpi,
                        device = grDevices::png, type = png_type)
      }

      # Close modal after download starts
      removeModal()
    }
  )

  observeEvent(input$res_copy_plot, {
    rend <- active_rendered()
    if (is.null(rend) || inherits(rend, "error")) {
      showNotification("No plot available to copy.", type = "warning")
      return()
    }

    plots <- res_extract_ggplots(rend)
    if (length(plots) == 0) {
      showNotification("No plot available to copy.", type = "warning")
      return()
    }

    plot_id <- res_current_plot_output_id()
    if (!nzchar(plot_id)) {
      showNotification("No plot available to copy.", type = "warning")
      return()
    }

    session$sendCustomMessage("copy_plot", list(id = plot_id))
  }, ignoreInit = TRUE)

  # ---- DataProcessor per-substep downloads -----------------------------------
  observe({
    eng <- tolower(active_engine_id() %||% "")
    if (!identical(eng, "dataprocessor")) return()

    res <- active_results()
    states <- res$data$substep_states %||% list()
    if (length(states) == 0) return()

    steps <- suppressWarnings(as.integer(vapply(states, function(s) s$Step %||% NA_integer_, integer(1))))
    steps <- steps[is.finite(steps) & steps > 0]
    if (length(steps) == 0) return()

    for (step_num in unique(steps)) {
      local({
        s_step <- as.integer(step_num)
        out_id <- paste0("res_dl_dp_step_", s_step)

        output[[out_id]] <- downloadHandler(
          filename = function() {
            paste0("dataprocessor_substep_", s_step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
          },
          content = function(file) {
            res2 <- active_results()
            states2 <- res2$data$substep_states %||% list()
            idx <- which(vapply(states2, function(s) isTRUE(as.integer(s$Step %||% -1L) == s_step), logical(1)))
            if (length(idx) == 0) stop("Requested substep state not found: ", s_step)
            st <- states2[[idx[[1]]]]

            mat <- st$mat %||% NULL
            ids <- st$ids %||% NULL
            op  <- as.character(st$Operation %||% "")

            if (!requireNamespace("openxlsx", quietly = TRUE)) {
              writeLines("openxlsx package not installed.", file)
              return()
            }

            wb <- openxlsx::createWorkbook()

            info <- data.frame(
              Step = s_step,
              Operation = op,
              rows = if (!is.null(mat)) nrow(mat) else NA_integer_,
              cols = if (!is.null(mat)) ncol(mat) else NA_integer_,
              stringsAsFactors = FALSE
            )
            openxlsx::addWorksheet(wb, "Info")
            openxlsx::writeData(wb, "Info", info)

            if (!is.null(mat)) {
              if (!is.null(ids) && is.data.frame(ids)) {
                out_df <- cbind(ids, as.data.frame(mat))
              } else {
                out_df <- as.data.frame(mat)
              }
              openxlsx::addWorksheet(wb, "data")
              openxlsx::writeData(wb, "data", out_df)
            }

            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          }
        )
      })
    }
  })

  # ---- Overview download handlers ---------------------------------------------

  # Download log.txt
  output$res_dl_log <- downloadHandler(
    filename = function() {
      paste0("run_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      log_path <- file.path(rv$run_root, "log.txt")
      if (file.exists(log_path)) {
        file.copy(log_path, file)
      } else {
        writeLines("No log.txt found in run.", file)
      }
    }
  )

  # Bulk download graphs modal
  observeEvent(input$res_bulk_download_modal, {
    req(rv$nodes_df)

    # Build list of available graphs by plot
    df <- rv$nodes_df
    graph_choices <- list()
    registry <- res_registry()
    should_hide <- res_should_hide_nodes(df, registry)
    node_numbers <- res_compute_node_numbers(df)

    collect_plot_keys <- function(res, eng_id, registry) {
      eng_id <- tolower(as.character(eng_id %||% ""))
      data_obj <- res$data %||% list()

      if (eng_id == "volcano") {
        comps <- data_obj$comparisons %||% list()
        if (is.list(comps) && length(comps) > 0) return(names(comps))
        return("volcano_plot")
      }

      if (eng_id %in% c("goora", "1dgofcs")) {
        if (eng_id == "1dgofcs") {
          analyses <- data_obj$analyses %||% NULL
          if (is.list(analyses) && length(analyses) > 0) {
            return(paste0(names(analyses), "_plot"))
          }
        }
        tabs <- character()
        for (tab in c("BP", "MF", "CC")) {
          if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
            tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) == 0 && !is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
            "ontology" %in% names(data_obj$terms)) {
          ontologies_present <- unique(as.character(data_obj$terms$ontology))
          for (tab in c("BP", "MF", "CC")) {
            if (tab %in% ontologies_present) tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
        return(if (eng_id == "goora") "goora_plot" else "1dgofcs_plot")
      }

      if (eng_id == "2dgofcs") {
        analyses <- data_obj$analyses %||% NULL
        if (is.list(analyses) && length(analyses) > 0) {
          return(names(analyses))
        }
        tabs <- character()
        for (tab in c("BP", "MF", "CC")) {
          if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
            tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
        return("2dgofcs_plot")
      }

      spec_plots <- registry$engines[[eng_id]]$render_spec$plots %||% character()
      spec_plots <- spec_plots[nzchar(spec_plots)]
      if (length(spec_plots) > 0) return(spec_plots)

      character()
    }

    for (i in seq_len(nrow(df))) {
      node_id <- df$node_id[i]
      eng_id <- tolower(df$engine_id[i] %||% "")
      node_dir <- df$node_dir[i]

      # Skip overview, dataprocessor, and hidden nodes (same logic as sidebar)
      if (eng_id == "dataprocessor" || node_id == "overview") next
      if (should_hide[i]) next

      res <- tb_load_results(node_dir)
      if (is.null(res)) next

      effective_state <- res_build_effective_state(node_id, node_dir)
      style <- effective_state$style %||% list()

      plot_keys <- collect_plot_keys(res, eng_id, registry)
      if (length(plot_keys) == 0) next

      force_plot_label <- length(plot_keys) > 1
      for (plot_key in plot_keys) {
        base <- res_export_graph_name(node_id, eng_id, plot_key, registry, res, style, node_numbers,
                                      force_plot_label = force_plot_label)
        label <- base
        if (!nzchar(label)) label <- paste0(node_id, "-", plot_key)

        value <- paste(node_id, plot_key, sep = "::")
        if (label %in% names(graph_choices)) {
          label <- paste0(label, " (", plot_key, ")")
        }
        graph_choices[[label]] <- value
      }
    }

    if (length(graph_choices) == 0) {
      showNotification("No graphs available for download.", type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "Bulk Download Graphs",
      div(
        style = "max-height: 300px; overflow-y: auto;",
        checkboxGroupInput(
          "bulk_dl_graphs",
          "Select graphs to download:",
          choices = graph_choices,
          selected = unlist(graph_choices)  # Select all by default
        )
      ),
      div(
        style = "display: flex; gap: 16px; margin-top: 16px; flex-wrap: wrap;",
        div(
          style = "flex: 1; min-width: 140px;",
          selectInput(
            "bulk_dl_dpi",
            "Resolution (DPI):",
            choices = c("150 (preview)" = "150", "300 (print)" = "300", "600 (high-res)" = "600"),
            selected = "300",
            width = "100%"
          )
        ),
        div(
          style = "flex: 1; min-width: 120px;",
          selectInput(
            "bulk_dl_format",
            "Format:",
            choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
            selected = "png",
            width = "100%"
          )
        )
      ),
      footer = tagList(
        downloadButton("res_dl_bulk_graphs", "Download Selected", class = "btn-primary"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  # Download bulk graphs
  output$res_dl_bulk_graphs <- downloadHandler(
    filename = function() {
      paste0("graphs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root, input$bulk_dl_graphs)

      msterp_set_busy(session, TRUE, "Generating graphs...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      selected_plots <- input$bulk_dl_graphs
      dpi <- as.numeric(input$bulk_dl_dpi %||% 300)
      fmt <- input$bulk_dl_format %||% "png"

      if (length(selected_plots) == 0) {
        stop("No graphs selected for download.")
      }

      # Create temp directory for graphs
      tmp_dir <- tempfile("graphs_")
      dir.create(tmp_dir, recursive = TRUE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      registry <- res_registry()
      df <- rv$nodes_df
      node_numbers <- res_compute_node_numbers(df)

      selected_pairs <- strsplit(selected_plots, "::", fixed = TRUE)
      selected_df <- data.frame(
        node_id = vapply(selected_pairs, function(x) x[[1]] %||% "", character(1)),
        plot_key = vapply(selected_pairs, function(x) if (length(x) >= 2) x[[2]] else "", character(1)),
        stringsAsFactors = FALSE
      )
      selected_df <- selected_df[nzchar(selected_df$node_id) & nzchar(selected_df$plot_key), , drop = FALSE]
      if (nrow(selected_df) == 0) stop("No valid plots selected for download.")

      node_numbers <- res_compute_node_numbers(df)

      # Iterate through selected nodes and export requested plots
      for (node_id in unique(selected_df$node_id)) {
        idx <- which(df$node_id == node_id)
        if (length(idx) == 0) next

        node_dir <- df$node_dir[idx]
        eng_id <- tolower(df$engine_id[idx] %||% "")

        # Load results
        res <- tb_load_results(node_dir)
        if (is.null(res)) next

        effective_state <- res_build_effective_state(node_id, node_dir)
        if (eng_id %in% c("goora", "1dgofcs", "2dgofcs")) {
          effective_state$style$ontology_filter <- "all"
        }
        node_meta <- tb_node_meta(node_dir)

        # Render with full effective_state (style + plotly + visibility)
        rend <- tryCatch(
          terpbook_render_node(eng_id, res, effective_state, registry = registry, node_meta = node_meta),
          error = function(e) NULL
        )
        if (is.null(rend)) next

        plots <- res_extract_ggplots(rend)
        if (length(plots) == 0) next

        plot_rows <- selected_df[selected_df$node_id == node_id, , drop = FALSE]
        if (nrow(plot_rows) == 0) next

        force_plot_label <- length(plots) > 1
        for (i_plot in seq_len(nrow(plot_rows))) {
          plot_key <- plot_rows$plot_key[[i_plot]]
          if (!nzchar(plot_key) || !(plot_key %in% names(plots))) next

          p <- plots[[plot_key]]
          base <- res_export_graph_name(node_id, eng_id, plot_key, registry, res, effective_state$style, node_numbers,
                                        force_plot_label = force_plot_label)
          base <- res_export_filename(base)
          if (!nzchar(base)) base <- paste0(node_id, "-", plot_key)

          fname <- sprintf("%s.%s", base, fmt)
          fpath <- file.path(tmp_dir, fname)
          if (file.exists(fpath)) {
            idx <- 2L
            repeat {
              fname <- sprintf("%s-%d.%s", base, idx, fmt)
              fpath <- file.path(tmp_dir, fname)
              if (!file.exists(fpath)) break
              idx <- idx + 1L
            }
          }

          defs <- tb_publication_export_defaults(effective_state$style)
          w <- defs$width
          h <- defs$height

          tryCatch({
            if (fmt == "pdf") {
              ggplot2::ggsave(fpath, p, width = w, height = h, units = defs$units, device = defs$pdf_device)
            } else if (fmt == "svg") {
              ggplot2::ggsave(fpath, p, width = w, height = h, units = defs$units, device = defs$svg_device)
            } else {
              png_type <- if (capabilities("cairo")) "cairo-png" else NULL
              ggplot2::ggsave(fpath, p, width = w, height = h, units = defs$units, dpi = dpi,
                              device = grDevices::png, type = png_type)
            }
          }, error = function(e) NULL)
        }
      }

      # Find generated files
      pattern <- sprintf("\\.%s$", fmt)
      files <- list.files(tmp_dir, pattern = pattern)

      if (length(files) == 0) {
        stop("No graphs could be generated. Check that the selected steps contain renderable plots.")
      }

      # Create zip using the cross-platform helper
      res_safe_zip(file, files, root = tmp_dir)

      removeModal()
    }
  )

  # Terpbook PDF pre-filter modal
  observeEvent(input$res_dl_terpbook_pdf_modal, {
    req(rv$nodes_df)

    df <- rv$nodes_df
    graph_choices <- list()
    registry <- res_registry()
    should_hide <- res_should_hide_nodes(df, registry)
    node_numbers <- res_compute_node_numbers(df)

    collect_plot_keys <- function(res, eng_id, registry) {
      eng_id <- tolower(as.character(eng_id %||% ""))
      data_obj <- res$data %||% list()

      if (eng_id == "volcano") {
        comps <- data_obj$comparisons %||% list()
        if (is.list(comps) && length(comps) > 0) return(names(comps))
        return("volcano_plot")
      }

      if (eng_id %in% c("goora", "1dgofcs")) {
        if (eng_id == "1dgofcs") {
          analyses <- data_obj$analyses %||% NULL
          if (is.list(analyses) && length(analyses) > 0) {
            return(paste0(names(analyses), "_plot"))
          }
        }
        tabs <- character()
        for (tab in c("BP", "MF", "CC")) {
          if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
            tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) == 0 && !is.null(data_obj$terms) && is.data.frame(data_obj$terms) &&
            "ontology" %in% names(data_obj$terms)) {
          ontologies_present <- unique(as.character(data_obj$terms$ontology))
          for (tab in c("BP", "MF", "CC")) {
            if (tab %in% ontologies_present) tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
        return(if (eng_id == "goora") "goora_plot" else "1dgofcs_plot")
      }

      if (eng_id == "2dgofcs") {
        analyses <- data_obj$analyses %||% NULL
        if (is.list(analyses) && length(analyses) > 0) {
          return(names(analyses))
        }
        tabs <- character()
        for (tab in c("BP", "MF", "CC")) {
          if (!is.null(data_obj[[tab]]) && length(data_obj[[tab]]) > 0) {
            tabs <- c(tabs, tab)
          }
        }
        if (length(tabs) > 0) return(paste0(tolower(tabs), "_plot"))
        return("2dgofcs_plot")
      }

      spec_plots <- registry$engines[[eng_id]]$render_spec$plots %||% character()
      spec_plots <- spec_plots[nzchar(spec_plots)]
      if (length(spec_plots) > 0) return(spec_plots)

      character()
    }

    for (i in seq_len(nrow(df))) {
      node_id <- df$node_id[i]
      eng_id <- tolower(df$engine_id[i] %||% "")
      node_dir <- df$node_dir[i]

      # Skip overview, dataprocessor, and hidden nodes (same logic as sidebar)
      if (eng_id == "dataprocessor" || node_id == "overview") next
      if (should_hide[i]) next

      res <- tb_load_results(node_dir)
      if (is.null(res)) next

      effective_state <- res_build_effective_state(node_id, node_dir)
      style <- effective_state$style %||% list()

      plot_keys <- collect_plot_keys(res, eng_id, registry)
      if (length(plot_keys) == 0) next

      force_plot_label <- length(plot_keys) > 1
      for (plot_key in plot_keys) {
        base <- res_export_graph_name(node_id, eng_id, plot_key, registry, res, style, node_numbers,
                                      force_plot_label = force_plot_label)
        label <- base
        if (!nzchar(label)) label <- paste0(node_id, "-", plot_key)

        value <- paste(node_id, plot_key, sep = "::")
        if (label %in% names(graph_choices)) {
          label <- paste0(label, " (", plot_key, ")")
        }
        graph_choices[[label]] <- value
      }
    }

    if (length(graph_choices) == 0) {
      showNotification("No graphs available for PDF export.", type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "Download Terpbook as PDF",
      div(
        style = "max-height: 300px; overflow-y: auto;",
        checkboxGroupInput(
          "pdf_dl_graphs",
          "Select graphs to include:",
          choices = graph_choices,
          selected = unlist(graph_choices)
        )
      ),
      footer = tagList(
        downloadButton("res_dl_terpbook_pdf_filtered", "Download PDF", class = "btn-primary"),
        modalButton("Cancel")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  # Download terpbook PDF with selected graphs
  output$res_dl_terpbook_pdf_filtered <- downloadHandler(
    filename = function() {
      name <- rv$terpbook_filename %||% "terpbook"
      name <- gsub("\\.terpbook$", "", name)
      paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root, input$pdf_dl_graphs)

      msterp_set_busy(session, TRUE, "Generating PDF...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      selected_plots <- input$pdf_dl_graphs
      if (length(selected_plots) == 0) {
        stop("No graphs selected for PDF export.")
      }

      selected_pairs <- strsplit(selected_plots, "::", fixed = TRUE)
      selected_df <- data.frame(
        node_id = vapply(selected_pairs, function(x) x[[1]] %||% "", character(1)),
        plot_key = vapply(selected_pairs, function(x) if (length(x) >= 2) x[[2]] else "", character(1)),
        stringsAsFactors = FALSE
      )
      selected_df <- selected_df[nzchar(selected_df$node_id) & nzchar(selected_df$plot_key), , drop = FALSE]
      if (nrow(selected_df) == 0) stop("No valid plots selected for PDF export.")

      tryCatch({
        registry <- res_registry()
        df <- rv$nodes_df
        node_numbers <- res_compute_node_numbers(df)

        # Collect selected plots
        all_plots <- list()

        for (i in seq_len(nrow(df))) {
          node_id <- df$node_id[i]
          node_dir <- df$node_dir[i]
          eng_id <- tolower(df$engine_id[i] %||% "")

          if (!(node_id %in% selected_df$node_id)) next

          # Skip overview and dataprocessor
          if (eng_id == "dataprocessor" || node_id == "overview") next

          # Load results
          res <- tb_load_results(node_dir)
          if (is.null(res)) next

          effective_state <- res_build_effective_state(node_id, node_dir)
          if (eng_id %in% c("goora", "1dgofcs", "2dgofcs")) {
            effective_state$style$ontology_filter <- "all"
          }
          node_meta <- tb_node_meta(node_dir)

          # Render with full effective_state (style + plotly + visibility)
          rend <- tryCatch(
            terpbook_render_node(eng_id, res, effective_state, registry = registry, node_meta = node_meta),
            error = function(e) NULL
          )
          if (is.null(rend)) next

          plots <- res_extract_ggplots(rend)
          if (length(plots) == 0) next

          plot_rows <- selected_df[selected_df$node_id == node_id, , drop = FALSE]
          plot_keys <- plot_rows$plot_key
          plot_keys <- plot_keys[nzchar(plot_keys)]
          plot_keys <- plot_keys[!duplicated(plot_keys)]
          if (length(plot_keys) == 0) next

          force_plot_label <- length(plots) > 1
          for (plot_key in plot_keys) {
            if (!nzchar(plot_key) || !(plot_key %in% names(plots))) next

            p <- plots[[plot_key]]
            w <- suppressWarnings(as.numeric(effective_state$style$width %||% 7))
            h <- suppressWarnings(as.numeric(effective_state$style$height %||% 5))
            if (!is.finite(w) || w <= 0) w <- 7
            if (!is.finite(h) || h <= 0) h <- 5

            title <- res_export_graph_name(node_id, eng_id, plot_key, registry, res, effective_state$style,
                                           node_numbers = node_numbers,
                                           force_plot_label = force_plot_label)
            if (!nzchar(title)) title <- paste0(node_id, "-", plot_key)

            all_plots[[length(all_plots) + 1]] <- list(
              plot = p,
              width = w,
              height = h,
              engine_id = eng_id,
              title = title
            )
          }
        }

        if (length(all_plots) == 0) {
          # Create empty PDF with message
          # Use cairo_pdf for Unicode support (Greek letters like rho)
          if (capabilities("cairo")) {
            grDevices::cairo_pdf(file, width = 8.5, height = 11)
          } else {
            grDevices::pdf(file, width = 8.5, height = 11)
          }
          graphics::plot.new()
          graphics::text(0.5, 0.5, "No graphs available in terpbook.", cex = 1.5)
          grDevices::dev.off()
          return()
        }

        # Use standard page size that fits most plots (letter size with margins)
        # cairo_pdf supports Unicode characters (Greek letters like rho in Spearman plots)
        if (capabilities("cairo")) {
          grDevices::cairo_pdf(file, width = 11, height = 8.5, onefile = TRUE)
        } else {
          grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
        }

        for (item in all_plots) {
          p <- item$plot

          # Add a PDF page title for most plots, but defer to renderer-level title control
          # for engines that explicitly manage title suppression (e.g., IDQuant Overlap).
          if (!(item$engine_id %in% c("idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified"))) {
            p <- p +
              ggplot2::labs(title = item$title) +
              ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5))
          }

          # Print each plot on its own page
          print(p)
        }

        grDevices::dev.off()
      }, error = function(e) {
        # Ensure any open device is closed on error
        try(grDevices::dev.off(), silent = TRUE)
        stop(e)
      })
    }
  )

  # Download terpbook as single PDF with all graphs
  output$res_dl_terpbook_pdf <- downloadHandler(
    filename = function() {
      name <- rv$terpbook_filename %||% "terpbook"
      name <- gsub("\\.terpbook$", "", name)
      paste0(name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root)

      msterp_set_busy(session, TRUE, "Generating PDF...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      tryCatch({
        registry <- res_registry()
        df <- rv$nodes_df
        node_numbers <- res_compute_node_numbers(df)

        # Collect all plots
        all_plots <- list()

        for (i in seq_len(nrow(df))) {
          node_id <- df$node_id[i]
          node_dir <- df$node_dir[i]
          eng_id <- tolower(df$engine_id[i] %||% "")

          # Skip overview and dataprocessor
          if (eng_id == "dataprocessor" || node_id == "overview") next

          # Load results
          res <- tb_load_results(node_dir)
          if (is.null(res)) next

          effective_state <- res_build_effective_state(node_id, node_dir)
          if (eng_id %in% c("goora", "1dgofcs", "2dgofcs")) {
            effective_state$style$ontology_filter <- "all"
          }
          node_meta <- tb_node_meta(node_dir)

          # Render with full effective_state (style + plotly + visibility)
          rend <- tryCatch(
            terpbook_render_node(eng_id, res, effective_state, registry = registry, node_meta = node_meta),
            error = function(e) NULL
          )
          if (is.null(rend)) next

          plots <- res_extract_ggplots(rend)
          if (length(plots) == 0) next

          # Store plots with metadata
          force_plot_label <- length(plots) > 1
          for (pname in names(plots)) {
            p <- plots[[pname]]
            w <- suppressWarnings(as.numeric(effective_state$style$width %||% 7))
            h <- suppressWarnings(as.numeric(effective_state$style$height %||% 5))
            if (!is.finite(w) || w <= 0) w <- 7
            if (!is.finite(h) || h <= 0) h <- 5

            title <- res_export_graph_name(node_id, eng_id, pname, registry, res, effective_state$style,
                                           node_numbers = node_numbers,
                                           force_plot_label = force_plot_label)
            if (!nzchar(title)) title <- paste0(node_id, "-", pname)

            all_plots[[length(all_plots) + 1]] <- list(
              plot = p,
              width = w,
              height = h,
              engine_id = eng_id,
              title = title
            )
          }
        }

        if (length(all_plots) == 0) {
          # Create empty PDF with message
          # Use cairo_pdf for Unicode support (Greek letters like rho)
          if (capabilities("cairo")) {
            grDevices::cairo_pdf(file, width = 8.5, height = 11)
          } else {
            grDevices::pdf(file, width = 8.5, height = 11)
          }
          graphics::plot.new()
          graphics::text(0.5, 0.5, "No graphs available in terpbook.", cex = 1.5)
          grDevices::dev.off()
          return()
        }

        # Use standard page size that fits most plots (letter size with margins)
        # cairo_pdf supports Unicode characters (Greek letters like rho in Spearman plots)
        if (capabilities("cairo")) {
          grDevices::cairo_pdf(file, width = 11, height = 8.5, onefile = TRUE)
        } else {
          grDevices::pdf(file, width = 11, height = 8.5, onefile = TRUE)
        }

        for (item in all_plots) {
          p <- item$plot

          # Add a PDF page title for most plots, but defer to renderer-level title control
          # for engines that explicitly manage title suppression (e.g., IDQuant Overlap).
          if (!(item$engine_id %in% c("idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified"))) {
            p <- p +
              ggplot2::labs(title = item$title) +
              ggplot2::theme(plot.title = ggplot2::element_text(size = 10, hjust = 0.5))
          }

          # Print each plot on its own page
          print(p)
        }

        grDevices::dev.off()
      }, error = function(e) {
        # Ensure any open device is closed on error
        try(grDevices::dev.off(), silent = TRUE)
        stop(e)
      })
    }
  )

  # Legacy download all graphs as ZIP (kept for backward compatibility)
  output$res_dl_all_png <- downloadHandler(
    filename = function() {
      paste0("all_graphs_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root)

      # Create temp directory for graphs
      tmp_dir <- tempfile("graphs_")
      dir.create(tmp_dir, recursive = TRUE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

      registry <- res_registry()
      df <- rv$nodes_df

      # Iterate through all nodes and export graphs
      for (i in seq_len(nrow(df))) {
        node_dir <- df$node_dir[i]
        eng_id <- tolower(df$engine_id[i] %||% "")

        # Skip overview and dataprocessor
        if (eng_id == "dataprocessor" || df$node_id[i] == "overview") next

        node_id <- df$node_id[i]

        # Load results
        res <- tb_load_results(node_dir)
        if (is.null(res)) next

        effective_state <- res_build_effective_state(node_id, node_dir)
        if (eng_id %in% c("goora", "1dgofcs", "2dgofcs")) {
          effective_state$style$ontology_filter <- "all"
        }
        node_meta <- tb_node_meta(node_dir)

        # Render with full effective_state (style + plotly + visibility)
        rend <- tryCatch(
          terpbook_render_node(eng_id, res, effective_state, registry = registry, node_meta = node_meta),
          error = function(e) NULL
        )
        if (is.null(rend)) next

        plots <- res_extract_ggplots(rend)
        if (length(plots) == 0) next

        # Save each plot
        force_plot_label <- length(plots) > 1
        for (pname in names(plots)) {
          p <- plots[[pname]]
          base <- res_export_graph_name(node_id, eng_id, pname, registry, res, effective_state$style, node_numbers,
                                        force_plot_label = force_plot_label)
          base <- res_export_filename(base)
          if (!nzchar(base)) base <- paste0(node_id, "-", pname)
          fname <- sprintf("%s.png", base)
          fpath <- file.path(tmp_dir, fname)
          if (file.exists(fpath)) {
            idx <- 2L
            repeat {
              fname <- sprintf("%s-%d.png", base, idx)
              fpath <- file.path(tmp_dir, fname)
              if (!file.exists(fpath)) break
              idx <- idx + 1L
            }
          }

          w <- suppressWarnings(as.numeric(effective_state$style$width %||% 7))
          h <- suppressWarnings(as.numeric(effective_state$style$height %||% 5))
          if (!is.finite(w)) w <- 7
          if (!is.finite(h)) h <- 5

          tryCatch(
            ggplot2::ggsave(fpath, p, width = w, height = h, units = "in", dpi = 150),
            error = function(e) NULL
          )
        }
      }

      # Create zip using the cross-platform helper
      files <- list.files(tmp_dir, pattern = "\\.png$")
      if (length(files) == 0) {
        stop("No graphs could be generated.")
      }

      res_safe_zip(file, files, root = tmp_dir)
    }
  )

  # Download processed data (from dataprocessor step)
  output$res_dl_processed <- downloadHandler(
    filename = function() {
      paste0("processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root)

      msterp_set_busy(session, TRUE, "Exporting processed data...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      df <- rv$nodes_df
      dp_idx <- which(tolower(df$engine_id) == "dataprocessor" & df$kind == "step")

      if (length(dp_idx) == 0) {
        # No dataprocessor step - create empty file
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Info")
          openxlsx::writeData(wb, "Info", data.frame(Message = "No processed data available"))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        } else {
          writeLines("No processed data available and openxlsx not installed.", file)
        }
        return()
      }

      # Load dataprocessor results
      dp_dir <- df$node_dir[dp_idx[1]]
      res <- tb_load_results(dp_dir)

      if (is.null(res) || is.null(res$data)) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Info")
          openxlsx::writeData(wb, "Info", data.frame(Message = "No processed data in results"))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
        return()
      }

      if (requireNamespace("openxlsx", quietly = TRUE)) {
        wb <- openxlsx::createWorkbook()

        # Add matrix data as "data" sheet (for re-runnable format)
        if (!is.null(res$data$mat)) {
          mat <- res$data$mat
          ids <- res$data$ids

          # Combine IDs and matrix
          if (!is.null(ids) && is.data.frame(ids)) {
            out_df <- cbind(ids, as.data.frame(mat))
          } else {
            out_df <- as.data.frame(mat)
          }

          openxlsx::addWorksheet(wb, "data")
          openxlsx::writeData(wb, "data", out_df)
        }

        # Build and add "design" sheet for re-runnable format
        metadata <- res$data$metadata
        samples <- res$data$samples
        groups <- res$data$groups

        if (!is.null(metadata) && !is.null(samples)) {
          lvl <- tolower(metadata$analysis_level %||% "protein")
          if (!lvl %in% c("protein", "peptide")) lvl <- "protein"

          id_primary_type_default <- if (identical(lvl, "peptide")) "Peptide ID" else "Protein ID"
          id_primary_col_default <- if (identical(lvl, "peptide")) {
            metadata$id_peptide_col %||% metadata$id_primary_col %||% "PeptideID"
          } else {
            metadata$id_primary_col %||% metadata$id_protein_col %||% "ProteinID"
          }

          # Build meta rows
          meta_list <- list(
            schema_version = "1.0",
            created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            analysis_level = lvl,
            id_primary_type = metadata$id_primary_type %||% id_primary_type_default,
            id_primary_col = id_primary_col_default,
            id_protein_col = metadata$id_protein_col %||% "ProteinID",
            id_gene_col = metadata$id_gene_col %||% "",
            id_peptide_col = if (identical(lvl, "peptide")) (metadata$id_peptide_col %||% id_primary_col_default) else (metadata$id_peptide_col %||% ""),
            processed = "true"
          )

          meta_df <- data.frame(
            record_type = "meta",
            key = names(meta_list),
            value = vapply(meta_list, function(x) as.character(x %||% ""), character(1)),
            group_id = NA_character_,
            group_name = NA_character_,
            color = NA_character_,
            is_control = NA,
            data_col = NA_character_,
            display_name = NA_character_,
            replicate = NA_integer_,
            include = NA,
            stringsAsFactors = FALSE
          )

          # Build group rows - use full groups data from metadata if available
          meta_groups <- metadata$groups  # Full data.frame with group_id, group_name, color, is_control
          if (!is.null(meta_groups) && is.data.frame(meta_groups) && nrow(meta_groups) > 0) {
            # Use the full metadata groups with is_control and color
            group_df <- data.frame(
              record_type = "group",
              key = NA_character_,
              value = NA_character_,
              group_id = meta_groups$group_id %||% paste0("group_", seq_len(nrow(meta_groups))),
              group_name = meta_groups$group_name %||% groups,
              color = meta_groups$color %||% NA_character_,
              is_control = if (!is.null(meta_groups$is_control)) meta_groups$is_control else FALSE,
              data_col = NA_character_,
              display_name = NA_character_,
              replicate = NA_integer_,
              include = NA,
              stringsAsFactors = FALSE
            )
          } else if (!is.null(groups) && length(groups) > 0) {
            # Fallback to simple groups list
            group_df <- data.frame(
              record_type = "group",
              key = NA_character_,
              value = NA_character_,
              group_id = paste0("group_", seq_along(groups)),
              group_name = groups,
              color = NA_character_,
              is_control = FALSE,
              data_col = NA_character_,
              display_name = NA_character_,
              replicate = NA_integer_,
              include = NA,
              stringsAsFactors = FALSE
            )
          } else {
            group_df <- NULL
          }

          # Build column rows from samples
          if (is.data.frame(samples) && nrow(samples) > 0) {
            col_df <- data.frame(
              record_type = "column",
              key = NA_character_,
              value = NA_character_,
              group_id = paste0("group_", match(samples$group_name, groups)),
              group_name = samples$group_name,
              color = NA_character_,
              is_control = NA,
              data_col = samples$sample_col,
              display_name = samples$sample_col,
              replicate = as.integer(samples$replicate %||% seq_len(nrow(samples))),
              include = TRUE,
              stringsAsFactors = FALSE
            )
          } else {
            col_df <- NULL
          }

          # Combine into design sheet
          design_df <- meta_df
          if (!is.null(group_df)) design_df <- rbind(design_df, group_df)
          if (!is.null(col_df)) design_df <- rbind(design_df, col_df)

          openxlsx::addWorksheet(wb, "design")
          openxlsx::writeData(wb, "design", design_df)
        }

        # Add summary if present (extra sheet, tolerated)
        if (!is.null(res$data$summary)) {
          openxlsx::addWorksheet(wb, "Summary")
          openxlsx::writeData(wb, "Summary", res$data$summary)
        }

        # Add log if present (extra sheet, tolerated)
        if (!is.null(res$data$log)) {
          openxlsx::addWorksheet(wb, "Log")
          openxlsx::writeData(wb, "Log", res$data$log)
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        writeLines("openxlsx package not installed.", file)
      }
    }
  )

  # Download marker sets (significant proteins from volcano + GO enrichments)
  output$res_dl_markers <- downloadHandler(
    filename = function() {
      paste0("marker_sets_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$nodes_df, rv$run_root)

      msterp_set_busy(session, TRUE, "Exporting marker sets...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      df <- rv$nodes_df

      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        writeLines("openxlsx package not installed.", file)
        return()
      }

      wb <- openxlsx::createWorkbook()
      sheet_counter <- 0

      # Helper to create unique sheet name (max 31 chars)
      make_sheet_name <- function(base_name) {
        clean_name <- substr(gsub("[^A-Za-z0-9_ -]", "_", base_name), 1, 28)
        sheet_counter <<- sheet_counter + 1
        paste0(sheet_counter, "_", clean_name)
      }

      # Helper to get parent label for context
      get_parent_label <- function(node_id) {
        if (is.null(node_id) || is.na(node_id)) return("")
        parent_row <- df[df$node_id == node_id, , drop = FALSE]
        if (nrow(parent_row) == 0) return("")
        lbl <- parent_row$label[1] %||% parent_row$step_label[1] %||% node_id
        res_pretty_label(lbl, engine_id = parent_row$engine_id[1])
      }

      go_engines <- c("goora", "1dgofcs", "2dgofcs")
      heatmap_engines <- c("heatmap", "ftest_heatmap")
      overlap_engines <- c("idquant_overlap_detected", "idquant_overlap_quantified")

      for (i in seq_len(nrow(df))) {
        eng <- tolower(df$engine_id[i] %||% "")
        kind <- as.character(df$kind[i] %||% "")

        # --- Volcano marker sets ---
        if (identical(eng, "volcano") && identical(kind, "step")) {
          v_dir <- df$node_dir[i]
          v_label <- df$label[i] %||% df$step_label[i] %||% "Volcano"
          v_label <- res_pretty_label(v_label, engine_id = "volcano")
          res <- tb_load_results(v_dir)

          if (is.null(res) || is.null(res$data)) next

          comparisons <- res$data$comparisons %||% list()

          for (comp_name in names(comparisons)) {
            comp <- comparisons[[comp_name]]
            sets <- comp$sets %||% list()

            sig_up <- sets$sig_up %||% character(0)
            sig_down <- sets$sig_down %||% character(0)

            points <- comp$points
            if (!is.null(points) && nrow(points) > 0) {
              up_data <- points[points$protein_id %in% sig_up, , drop = FALSE]
              down_data <- points[points$protein_id %in% sig_down, , drop = FALSE]

              if (nrow(up_data) > 0) up_data$direction <- "up"
              if (nrow(down_data) > 0) down_data$direction <- "down"

              marker_data <- rbind(up_data, down_data)

              if (nrow(marker_data) > 0) {
                # Add source context columns
                marker_data$source_analysis <- v_label
                marker_data$comparison <- comp_name

                sheet_name <- make_sheet_name(paste0(v_label, " ", comp_name))
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet_name, marker_data)
              }
            }
          }
        }

        # --- GO enrichment results (goora, 1dgofcs, 2dgofcs) ---
        if (eng %in% go_engines) {
          g_dir <- df$node_dir[i]
          g_label <- df$label[i] %||% df$step_label[i] %||% df$engine_id[i]
          g_label <- res_pretty_label(g_label, engine_id = df$engine_id[i])
          g_engine <- eng

          # Get parent context
          parent_id <- df$parent_id[i]
          parent_label <- get_parent_label(parent_id)

          # For paired GO analyses under volcano, try to get comparison and config context
          pairing_context <- ""
          config_name <- ""
          if (!is.null(parent_id) && !is.na(parent_id)) {
            parent_row <- df[df$node_id == parent_id, , drop = FALSE]
            if (nrow(parent_row) > 0 && tolower(parent_row$engine_id[1]) == "volcano") {
              # The label often contains the direction (up/down)
              if (grepl("up", g_label, ignore.case = TRUE)) {
                pairing_context <- "upregulated"
              } else if (grepl("down", g_label, ignore.case = TRUE)) {
                pairing_context <- "downregulated"
              }
              # Extract config name from node_id pattern: cfg_N__comparison_direction
              # or from the beginning of the label before the comparison
              node_id_parts <- strsplit(basename(df$node_id[i]), "__")[[1]]
              if (length(node_id_parts) >= 2) {
                config_name <- node_id_parts[1]  # e.g., "cfg_1"
              }
            }
          }

          res <- tb_load_results(g_dir)
          if (is.null(res) || is.null(res$data)) next

          # Get terms data
          terms <- res$data$terms %||% NULL
          if (is.null(terms) || !is.data.frame(terms) || nrow(terms) == 0) next

          # Add source context columns to terms
          terms$source_analysis <- g_label
          terms$parent_analysis <- parent_label
          if (nzchar(pairing_context)) {
            terms$direction_context <- pairing_context
          }
          if (nzchar(config_name)) {
            terms$config_id <- config_name
          }
          terms$engine_type <- g_engine

          # BP/MF/CC share a page - all terms from this GO analysis go on one sheet
          sheet_name <- make_sheet_name(paste0(parent_label, " ", g_label))
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, terms)
        }

        # --- Heatmap gene lists (heatmap, ftest_heatmap) ---
        if (eng %in% heatmap_engines) {
          h_dir <- df$node_dir[i]
          h_label <- df$label[i] %||% df$step_label[i] %||% df$engine_id[i]
          h_label <- res_pretty_label(h_label, engine_id = df$engine_id[i])
          h_engine <- eng

          res <- tb_load_results(h_dir)
          if (is.null(res) || is.null(res$data)) next

          # Get gene list from heatmap results
          # heatmap uses matched_genes, ftest_heatmap uses gene_order
          gene_list <- res$data$matched_genes %||% res$data$gene_order %||% character(0)
          if (length(gene_list) == 0) next

          # Build marker data frame with z-scores and abundance values
          marker_data <- data.frame(
            gene = gene_list,
            source_analysis = h_label,
            engine_type = h_engine,
            stringsAsFactors = FALSE
          )

          # Add z-score and abundance data per sample if available
          mat_zscore <- res$data$mat_zscore
          mat_log <- res$data$mat_log
          sample_order <- res$data$sample_order %||% character(0)
          group_annotations <- res$data$group_annotations

          # Add mean z-score per gene (row mean of z-scores)
          if (!is.null(mat_zscore) && is.matrix(mat_zscore) && nrow(mat_zscore) > 0) {
            gene_idx <- match(gene_list, rownames(mat_zscore))
            valid_idx <- !is.na(gene_idx)
            marker_data$mean_zscore <- NA_real_
            if (any(valid_idx)) {
              marker_data$mean_zscore[valid_idx] <- rowMeans(mat_zscore[gene_idx[valid_idx], , drop = FALSE], na.rm = TRUE)
            }

            # Add per-sample z-scores as columns (prefixed with "zscore_")
            if (length(sample_order) > 0 && ncol(mat_zscore) == length(sample_order)) {
              for (si in seq_along(sample_order)) {
                col_name <- paste0("zscore_", sample_order[si])
                marker_data[[col_name]] <- NA_real_
                if (any(valid_idx)) {
                  marker_data[[col_name]][valid_idx] <- mat_zscore[gene_idx[valid_idx], si]
                }
              }
            }
          }

          # Add mean abundance per gene (row mean of log-transformed values)
          if (!is.null(mat_log) && is.matrix(mat_log) && nrow(mat_log) > 0) {
            gene_idx <- match(gene_list, rownames(mat_log))
            valid_idx <- !is.na(gene_idx)
            marker_data$mean_abundance <- NA_real_
            if (any(valid_idx)) {
              marker_data$mean_abundance[valid_idx] <- rowMeans(mat_log[gene_idx[valid_idx], , drop = FALSE], na.rm = TRUE)
            }

            # Add per-sample abundance as columns (prefixed with "abundance_")
            if (length(sample_order) > 0 && ncol(mat_log) == length(sample_order)) {
              for (si in seq_along(sample_order)) {
                col_name <- paste0("abundance_", sample_order[si])
                marker_data[[col_name]] <- NA_real_
                if (any(valid_idx)) {
                  marker_data[[col_name]][valid_idx] <- mat_log[gene_idx[valid_idx], si]
                }
              }
            }
          }

          # For ftest_heatmap, include stats if available
          if (identical(h_engine, "ftest_heatmap") && !is.null(res$data$stats_table)) {
            stats <- res$data$stats_table
            if (is.data.frame(stats) && nrow(stats) > 0 && "gene" %in% names(stats)) {
              # Merge stats with marker data (pval, padj, sig_label)
              marker_data <- merge(marker_data, stats, by = "gene", all.x = TRUE)
            }
          }

          sheet_name <- make_sheet_name(h_label)
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, marker_data)
        }

        # --- Overlap unique proteins per group ---
        if (eng %in% overlap_engines) {
          o_dir <- df$node_dir[i]
          o_label <- df$label[i] %||% df$step_label[i] %||% df$engine_id[i]
          o_label <- res_pretty_label(o_label, engine_id = df$engine_id[i])
          o_engine <- eng

          res <- tb_load_results(o_dir)
          if (is.null(res) || is.null(res$data)) next

          mat <- res$data$intensity_mat
          smeta <- res$data$sample_meta
          ids_df <- res$data$ids

          if (is.null(mat) || !is.matrix(mat) || is.null(smeta) || !is.data.frame(smeta)) next

          # Get protein IDs from matrix rownames (same logic as tb_render_idquant_cv)
          protein_id <- rownames(mat)
          rownames_were_null <- is.null(protein_id)
          if (rownames_were_null) {
            protein_id <- paste0("protein_", seq_len(nrow(mat)))
          } else {
            # Only replace empty/missing protein IDs with placeholders, not all of them
            empty_idx <- which(!nzchar(protein_id) | is.na(protein_id))
            if (length(empty_idx) > 0) {
              protein_id[empty_idx] <- paste0("protein_", empty_idx)
            }
          }

          # Build gene symbol mapping using same logic as CV Scatter (tb_render_idquant_cv)
          label_id <- protein_id
          if (!is.null(ids_df) && is.data.frame(ids_df) && nrow(ids_df) > 0) {
            # Find primary ID column (protein ID) - same candidates as CV Scatter
            primary_candidates <- c("protein_id", "proteinid", "uniprot", "uniprot_id", "accession",
                                    "pg.proteingroups", "proteingroups")
            primary_col <- NULL
            for (cand in primary_candidates) {
              if (cand %in% tolower(names(ids_df))) {
                primary_col <- names(ids_df)[tolower(names(ids_df)) == cand][1]
                break
              }
            }
            # Fallback to first column
            if (is.null(primary_col)) primary_col <- names(ids_df)[1]

            # Find gene symbol column - same candidates as CV Scatter
            cand_gene_cols <- c("gene_symbol", "gene", "symbol", "geneid", "gene_id", "gene_name",
                                "pg.genes", "genes")
            gene_col <- NULL
            for (cand in cand_gene_cols) {
              matches <- names(ids_df)[tolower(names(ids_df)) == cand]
              if (length(matches) > 0) {
                gene_col <- matches[1]
                break
              }
            }
            if (is.null(gene_col)) {
              # Fallback: look for columns containing "gene" or "symbol" (case-insensitive)
              gene_matches <- names(ids_df)[grepl("gene|symbol", names(ids_df), ignore.case = TRUE)]
              gene_col <- setdiff(gene_matches, primary_col)
              gene_col <- gene_col[1] %||% NULL
            }

            # If rownames were NULL/missing and row counts match, use direct 1:1 mapping first.
            # This handles cases where the matrix doesn't have rownames but ids table has the data.
            use_direct_mapping <- rownames_were_null && nrow(ids_df) == length(protein_id)

            if (use_direct_mapping && !is.null(gene_col) && gene_col %in% names(ids_df)) {
              # Direct 1:1 correspondence: row i of matrix corresponds to row i of ids
              cand <- as.character(ids_df[[gene_col]])
              ok <- !is.na(cand) & nzchar(cand)
              label_id[ok] <- cand[ok]
              # Also update protein_id from ids table for better display
              if (!is.null(primary_col) && primary_col %in% names(ids_df)) {
                prot_cand <- as.character(ids_df[[primary_col]])
                prot_ok <- !is.na(prot_cand) & nzchar(prot_cand)
                protein_id[prot_ok] <- prot_cand[prot_ok]
              }
            } else if (!is.null(gene_col) && gene_col %in% names(ids_df) &&
                       !is.null(primary_col) && primary_col %in% names(ids_df)) {
              # Build lookup map: primary_id -> gene_symbol
              map <- stats::setNames(as.character(ids_df[[gene_col]]), as.character(ids_df[[primary_col]]))
              mapped <- unname(map[protein_id])
              ok <- !is.na(mapped) & nzchar(mapped)
              label_id[ok] <- mapped[ok]
            } else if (nrow(ids_df) == length(protein_id)) {
              # Fallback: if row counts match, assume 1:1 correspondence
              if (!is.null(gene_col) && gene_col %in% names(ids_df)) {
                cand <- as.character(ids_df[[gene_col]])
                ok <- !is.na(cand) & nzchar(cand)
                label_id[ok] <- cand[ok]
              }
            }
          }

          # Setup sample metadata
          smeta$sample_col <- as.character(smeta$sample_col)
          smeta$group_name <- as.character(smeta$group_name %||% smeta$group)
          smeta <- smeta[!is.na(smeta$sample_col) & nzchar(smeta$sample_col), , drop = FALSE]
          smeta <- smeta[smeta$sample_col %in% colnames(mat), , drop = FALSE]

          groups <- unique(smeta$group_name)
          groups <- groups[nzchar(groups)]

          # Determine overlap metric from engine type
          overlap_metric <- if (grepl("quantified", o_engine)) "quantified" else "detected"
          use_all_reps <- identical(overlap_metric, "quantified")

          # Build per-group presence sets (same logic as tb_render_idquant_overlap)
          # Use indices to track which proteins are present, so we can map to both protein_id and label_id
          sets_idx <- list()
          for (g in groups) {
            cols <- smeta$sample_col[smeta$group_name == g]
            cols <- intersect(as.character(cols), colnames(mat))
            if (length(cols) == 0) next

            grp_mat <- mat[, cols, drop = FALSE]
            present <- if (isTRUE(use_all_reps)) {
              apply(grp_mat, 1, function(x) all(is.finite(x) & !is.na(x) & x > 0))
            } else {
              apply(grp_mat, 1, function(x) any(is.finite(x) & !is.na(x) & x > 0))
            }
            sets_idx[[g]] <- which(present)
          }

          if (length(sets_idx) == 0 || length(groups) < 2) next

          # Compute unique proteins for each group (proteins in this group but not in any other)
          for (g in names(sets_idx)) {
            other_groups <- setdiff(names(sets_idx), g)
            other_idx <- unique(unlist(sets_idx[other_groups], use.names = FALSE))
            unique_idx <- setdiff(sets_idx[[g]], other_idx)

            if (length(unique_idx) == 0) next

            # Get protein IDs and gene symbols for unique proteins
            unique_proteins <- protein_id[unique_idx]
            gene_ids <- label_id[unique_idx]

            # Build export dataframe
            marker_data <- data.frame(
              protein_id = unique_proteins,
              gene_id = gene_ids,
              source_analysis = o_label,
              group = g,
              overlap_type = if (grepl("quantified", o_engine)) "Quantified" else "Identified",
              stringsAsFactors = FALSE
            )

            sheet_name <- make_sheet_name(paste0(g, " Unique"))
            openxlsx::addWorksheet(wb, sheet_name)
            openxlsx::writeData(wb, sheet_name, marker_data)
          }
        }
      }

      # If no sheets were added, add info sheet
      if (length(openxlsx::sheets(wb)) == 0) {
        openxlsx::addWorksheet(wb, "Info")
        openxlsx::writeData(wb, "Info", data.frame(Message = "No marker sets or GO enrichments found"))
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Download CV% export for idquant CV nodes (scatter/bar)
  output$res_dl_cv_export <- downloadHandler(
    filename = function() {
      paste0("cv_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$active_node_id, rv$run_root)

      msterp_set_busy(session, TRUE, "Exporting CV% data...")
      on.exit(msterp_set_busy(session, FALSE), add = TRUE)

      # Load results for active node
      node_dir <- file.path(rv$run_root, rv$active_node_id)
      res <- tb_load_results(node_dir)

      if (is.null(res) || is.null(res$data)) {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Info")
          openxlsx::writeData(wb, "Info", data.frame(Message = "No CV% data available"))
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        } else {
          writeLines("No CV% data available and openxlsx not installed.", file)
        }
        return()
      }

      # Get current style from effective state
      st <- active_effective_state()$style %||% list()

      # Build export dataframe using the pure function
      export_df <- build_cv_export_df(res, st)

      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        writeLines("openxlsx package not installed. Please install it with: install.packages('openxlsx')", file)
        return()
      }

      if (nrow(export_df) == 0) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Info")
        openxlsx::writeData(wb, "Info", data.frame(Message = "No CV% data to export (all proteins may have been filtered by min_replicates setting)"))
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        return()
      }

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "CV_Data")
      openxlsx::writeData(wb, "CV_Data", export_df)

      # Auto-size columns for readability
      openxlsx::setColWidths(wb, "CV_Data", cols = seq_len(ncol(export_df)), widths = "auto")

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

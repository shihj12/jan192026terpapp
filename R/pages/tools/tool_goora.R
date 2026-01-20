# R/pages/tools/tool_goora.R
# GO-ORA Tool - Gene Ontology Over-Representation Analysis

tools_goora_defaults <- function() {

  eng <- msterp_engine_get("goora")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# Build protein_to_go and go_terms from annot_long if not already present
# This mirrors the logic in run_utils.R for pipeline execution
tools_ensure_terpbase_go_mappings <- function(tb) {
  if (is.null(tb)) return(tb)


  # Build protein_to_go from annot_long if missing
  if (is.null(tb$protein_to_go) && !is.null(tb$annot_long)) {
    annot <- tb$annot_long
    if ("gene" %in% names(annot) && "ID" %in% names(annot)) {
      genes <- unique(annot$gene)
      protein_to_go <- lapply(genes, function(g) {
        unique(annot$ID[annot$gene == g])
      })
      names(protein_to_go) <- genes
      tb$protein_to_go <- protein_to_go
    }
  }

  # Build go_terms from terms_by_id if missing
  if (is.null(tb$go_terms) && !is.null(tb$terms_by_id)) {
    terms <- tb$terms_by_id
    if (all(c("ID", "Description", "ONTOLOGY") %in% names(terms))) {
      go_terms <- lapply(seq_len(nrow(terms)), function(i) {
        list(
          name = terms$Description[i],
          ontology = terms$ONTOLOGY[i]
        )
      })
      names(go_terms) <- terms$ID
      tb$go_terms <- go_terms
    }
  }

  tb
}

# ============================================================
# GO-ORA Tool UI
# ============================================================
tools_goora_ui <- function() {
  defs <- tools_goora_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    div(
      class = "top",
      actionButton("tools_goora_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("GO-ORA", style = "margin: 0;")
    ),
    tags$p("Paste a gene list to run GO over-representation analysis against a TerpBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("TerpBase"),
        uiOutput("tools_goora_terpbase_status"),
        selectInput(
          "tools_goora_terpbase_default_path",
          "Default TerpBase",
          choices = tools_default_terpbase_choices(),
          selected = ""
        ),
        actionButton(
          "tools_goora_terpbase_default_load",
          "Load default",
          class = "btn btn-default btn-sm btn-tool-action"
        ),
        fileInput(
          "tools_goora_terpbase_file",
          "Load TerpBase (.terpbase)",
          accept = c(".terpbase", ".rds"),
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Gene Input"),
        textAreaInput(
          "tools_goora_genes",
          "Gene list (one per line)",
          rows = 10,
          placeholder = "TP53\nEGFR\nBRCA1"
        ),
        actionButton("tools_goora_run", "Run GO-ORA", class = "btn-primary btn-tool-action"),
        hr(),
        tags$h4("View"),
        selectInput(
          "tools_goora_ontology_view",
          "Ontology",
          choices = c("Biological Process" = "BP",
                      "Molecular Function" = "MF", "Cellular Component" = "CC"),
          selected = "BP"
        ),
        tools_collapse_section_ui(
          "tools_goora_params_section",
          "Parameters",
          open = FALSE,
          numericInput(
            "tools_goora_fdr_cutoff",
            "FDR cutoff",
            value = params_defaults$fdr_cutoff %||% 0.03,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "tools_goora_min_term_size",
            "Min term size",
            value = params_defaults$min_term_size %||% 5,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_goora_min_overlap",
            "Min overlap",
            value = params_defaults$min_overlap %||% 1,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_goora_max_terms",
            "Terms to show (per ontology)",
            value = params_defaults$max_terms %||% 20,
            min = 1,
            max = 200,
            step = 1
          )
        ),
        tools_collapse_section_ui(
          "tools_goora_plot_section",
          "Plot Options",
          open = FALSE,
          selectInput(
            "tools_goora_plot_type",
            "Plot type",
            choices = c("bar", "dot"),
            selected = style_defaults$plot_type %||% "bar"
          ),
          selectInput(
            "tools_goora_color_mode",
            "Coloring",
            choices = c("fdr", "flat"),
            selected = style_defaults$color_mode %||% "fdr"
          ),
          conditionalPanel(
            condition = "input.tools_goora_color_mode == 'fdr'",
            selectInput(
              "tools_goora_fdr_palette",
              "FDR color palette",
              choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
              selected = style_defaults$fdr_palette %||% "yellow_cap"
            )
          ),
          conditionalPanel(
            condition = "input.tools_goora_color_mode == 'flat'",
            textInput(
              "tools_goora_flat_color",
              "Flat color (hex)",
              value = style_defaults$flat_color %||% "#B0B0B0"
            )
          ),
          sliderInput(
            "tools_goora_alpha",
            "Opacity",
            min = 0,
            max = 1,
            value = style_defaults$alpha %||% 0.8,
            step = 0.05
          ),
          checkboxInput(
            "tools_goora_show_go_id",
            "Show GO ID in labels",
            value = isTRUE(style_defaults$show_go_id %||% FALSE)
          ),
          checkboxInput(
            "tools_goora_flip_axis",
            "Flip horizontal axis",
            value = FALSE
          ),
          numericInput(
            "tools_goora_font_size",
            "Font size",
            value = style_defaults$font_size %||% 14,
            min = 6,
            max = 30,
            step = 1
          ),
          numericInput(
            "tools_goora_axis_text_size",
            "Axis text size",
            value = style_defaults$axis_text_size %||% 20,
            min = 6,
            max = 40,
            step = 1
          ),
          numericInput(
            "tools_goora_width",
            "Plot width (in)",
            value = style_defaults$width %||% 12,
            min = 2,
            max = 24,
            step = 0.5
          ),
          numericInput(
            "tools_goora_height",
            "Plot height (in)",
            value = style_defaults$height %||% 6,
            min = 2,
            max = 24,
            step = 0.5
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_goora_summary"),
        uiOutput("tools_goora_tabs")
      )
    )
  )
}

# ============================================================
# GO-ORA Server Logic
# ============================================================
tools_goora_server <- function(input, output, session, app_state, rv, defs_goora) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

  parse_gene_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(character(0))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    parts <- unlist(strsplit(lines, "[,;]"))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    unique(parts)
  }

  plot_dims_px <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_goora_width, defs_goora$style$width %||% 12)
    h_in <- safe_num(input$tools_goora_height, defs_goora$style$height %||% 6)

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(
      w = as.integer(round(w_px * scale)),
      h = as.integer(round(h_px * scale))
    )
  }

  output$tools_goora_terpbase_status <- renderUI({
    tb <- app_state$terpbase
    if (is.null(tb)) {
      return(tags$div(
        class = "text-danger",
        "No TerpBase loaded. Load one above or build one in the TerpBase page."
      ))
    }

    library_name <- tb$library_name %||% "(unnamed)"
    organism <- tb$organism %||% "(unknown)"
    n_ids <- NA_integer_
    if (!is.null(tb$annot_long) && "gene" %in% names(tb$annot_long)) {
      n_ids <- length(unique(tb$annot_long$gene))
    }

    tags$div(
      class = "text-muted",
      tags$strong("Active TerpBase:"),
      " ",
      library_name,
      tags$br(),
      sprintf("Organism: %s", organism),
      if (is.finite(n_ids)) {
        tags$div(sprintf("Unique identifiers: %s", n_ids))
      }
    )
  })

  # Helper to write progress.json
  write_progress <- function(path, status, message, pct = NULL) {
    obj <- list(status = status, message = message, pct = pct, timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    tryCatch(jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = FALSE), error = function(e) NULL)
  }

  # Reactive for polling progress.json
  progress_goora_rx <- reactive({
    if (identical(rv$run_status, "running")) {
      invalidateLater(500, session)
    }
    path <- rv$progress_path
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  })

  # Polling observer for background process completion
  observe({
    if (is.null(rv$bg_process) || !identical(rv$run_status, "running")) return()

    if (!rv$bg_process$is_alive()) {
      result <- tryCatch(rv$bg_process$get_result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))

      if (isTRUE(result$ok)) {
        rv$results <- result$result
        rv$rendered <- tb_render_goora(result$result, rv$pending_style, meta = NULL)
        rv$run_status <- "done"
        rv$status_msg <- NULL
      } else {
        rv$run_status <- "error"
        rv$status_msg <- result$error %||% "Analysis failed"
        rv$status_level <- "error"
      }
      rv$bg_process <- NULL
      if (!is.null(rv$progress_path) && file.exists(rv$progress_path)) {
        tryCatch(unlink(rv$progress_path), error = function(e) NULL)
      }
    } else {
      invalidateLater(500, session)
    }
  })

  observeEvent(input$tools_goora_run, {
    rv$status_msg <- NULL
    rv$status_level <- NULL
    rv$results <- NULL
    rv$rendered <- NULL
    rv$input_count <- NULL

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv$status_level <- "error"
      rv$run_status <- "idle"
      return()
    }

    genes <- parse_gene_input(input$tools_goora_genes)
    rv$input_count <- length(genes)

    if (length(genes) == 0) {
      rv$status_msg <- "Enter at least one gene symbol to run GO-ORA."
      rv$status_level <- "warn"
      rv$run_status <- "idle"
      return()
    }

    # Collect params and style from inputs (lightweight)
    params <- list(
      fdr_cutoff = safe_num(input$tools_goora_fdr_cutoff, defs_goora$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_goora_min_term_size, defs_goora$params$min_term_size %||% 5),
      min_overlap = safe_int(input$tools_goora_min_overlap, defs_goora$params$min_overlap %||% 1),
      max_terms = safe_int(input$tools_goora_max_terms, defs_goora$params$max_terms %||% 20)
    )

    style <- list(
      plot_type = input$tools_goora_plot_type %||% defs_goora$style$plot_type %||% "bar",
      color_mode = input$tools_goora_color_mode %||% defs_goora$style$color_mode %||% "fdr",
      fdr_palette = input$tools_goora_fdr_palette %||% defs_goora$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_goora_flat_color %||% "")) {
        input$tools_goora_flat_color
      } else {
        defs_goora$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_goora_alpha, defs_goora$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_goora_show_go_id),
      font_size = safe_int(input$tools_goora_font_size, defs_goora$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_goora_axis_text_size, defs_goora$style$axis_text_size %||% 20),
      width = safe_num(input$tools_goora_width, defs_goora$style$width %||% 12),
      height = safe_num(input$tools_goora_height, defs_goora$style$height %||% 6),
      flip_axis = isTRUE(input$tools_goora_flip_axis),
      ontology_filter = "all"  # Always compute all ontologies
    )

    rv$pending_style <- style

    # Show progress immediately - before any heavy work
    rv$progress_path <- tempfile(fileext = ".json")
    rv$run_start_time <- Sys.time()
    rv$run_status <- "running"
    write_progress(rv$progress_path, "running", "Starting GO-ORA analysis...", 5)

    has_callr <- requireNamespace("callr", quietly = TRUE)

    if (has_callr) {
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

      rv$bg_process <- callr::r_bg(
        func = function(app_root, tb, genes, params, progress_path) {
          # Set working directory (critical for file paths to resolve correctly)
          setwd(app_root)

          write_prog <- function(msg, pct) {
            obj <- list(status = "running", message = msg, pct = pct)
            tryCatch(jsonlite::write_json(obj, progress_path, auto_unbox = TRUE), error = function(e) NULL)
          }

          write_prog("Loading engine code...", 5)

          # Source all required files (same pattern as new_run page)
          source(file.path(app_root, "R", "00_init.R"), local = FALSE)
          engine_files <- list.files(file.path(app_root, "R", "engines"), pattern = "\\.R$", full.names = TRUE)
          for (f in engine_files) source(f, local = FALSE)
          stats_files <- list.files(file.path(app_root, "R", "engines", "stats"), pattern = "\\.R$", full.names = TRUE)
          for (f in stats_files) source(f, local = FALSE)
          utils_files <- list.files(file.path(app_root, "R", "utils"), pattern = "\\.R$", full.names = TRUE)
          for (f in utils_files) source(f, local = FALSE)

          write_prog("Preparing GO mappings...", 10)

          # Ensure terpbase has protein_to_go and go_terms mappings (moved to background)
          if (is.null(tb$protein_to_go) && !is.null(tb$annot_long)) {
            annot <- tb$annot_long
            if ("gene" %in% names(annot) && "ID" %in% names(annot)) {
              genes_list <- unique(annot$gene)
              protein_to_go <- lapply(genes_list, function(g) {
                unique(annot$ID[annot$gene == g])
              })
              names(protein_to_go) <- genes_list
              tb$protein_to_go <- protein_to_go
            }
          }
          if (is.null(tb$go_terms) && !is.null(tb$terms_by_id)) {
            terms <- tb$terms_by_id
            if (all(c("ID", "Description", "ONTOLOGY") %in% names(terms))) {
              go_terms <- lapply(seq_len(nrow(terms)), function(i) {
                list(name = terms$Description[i], ontology = terms$ONTOLOGY[i])
              })
              names(go_terms) <- terms$ID
              tb$go_terms <- go_terms
            }
          }

          payload <- list(
            ok = TRUE,
            params = params,
            query_proteins = genes,
            terpbase = tb
          )

          write_prog("Running over-representation analysis...", 30)

          result <- tryCatch({
            res <- stats_goora_run(payload, params = params, context = list(terpbase = tb))
            write_prog("Analysis complete!", 100)
            list(ok = TRUE, result = res)
          }, error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          })

          result
        },
        args = list(
          app_root = app_root,
          tb = tb,
          genes = genes,
          params = params,
          progress_path = rv$progress_path
        ),
        package = TRUE,
        supervise = TRUE
      )
    } else {
      # Synchronous fallback - must do prep work here
      tb <- tools_ensure_terpbase_go_mappings(tb)
      payload <- list(
        ok = TRUE,
        params = params,
        query_proteins = genes,
        terpbase = tb
      )

      res <- tryCatch({
        stats_goora_run(payload, params = params, context = list(terpbase = tb))
      }, error = function(e) {
        rv$status_msg <- conditionMessage(e)
        rv$status_level <- "error"
        rv$run_status <- "error"
        NULL
      })

      if (!is.null(res)) {
        rv$results <- res
        rv$rendered <- tb_render_goora(res, style, meta = NULL)
        rv$run_status <- "done"
      }
    }
  }, ignoreInit = TRUE)

  # Stop button handler
  observeEvent(input$tools_goora_stop, {
    if (!is.null(rv$bg_process) && rv$bg_process$is_alive()) {
      tryCatch(rv$bg_process$kill(), error = function(e) NULL)
      rv$bg_process <- NULL
      rv$run_status <- "idle"
      rv$status_msg <- "Analysis stopped by user"
      rv$status_level <- "warn"
    }
  }, ignoreInit = TRUE)

  output$tools_goora_summary <- renderUI({
    if (identical(rv$run_status, "running")) {
      p <- progress_goora_rx()
      pct <- suppressWarnings(as.numeric(p$pct %||% 0))
      if (is.na(pct)) pct <- 0
      pct <- max(0, min(100, pct))
      msg <- p$message %||% "Processing..."

      return(tagList(
        div(class = "tool-status-row",
          div(class = "tool-status-pill running", "Running"),
          div(class = "tool-status-msg", msg),
          actionButton("tools_goora_stop", "Stop", class = "btn btn-danger btn-sm", style = "margin-left: auto;")
        ),
        div(class = "tool-progress-wrap",
          div(class = "tool-progress-bar",
            div(class = "tool-progress-fill active", style = sprintf("width:%s%%;", pct))
          )
        )
      ))
    }

    if (!is.null(rv$status_msg)) {
      cls <- if (identical(rv$status_level, "error")) "text-danger" else "text-warning"
      return(tags$div(class = cls, rv$status_msg))
    }

    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "Run GO-ORA to see results."))
    }

    qinfo <- res$data$query_info %||% list()
    n_query <- qinfo$n_query %||% NA_integer_
    n_background <- qinfo$n_background %||% NA_integer_
    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv$input_count %||% NA_integer_
    n_unmatched <- if (is.finite(n_input) && is.finite(n_query)) n_input - n_query else NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("GO-ORA summary"),
      tags$p(sprintf("Input genes: %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Matched to TerpBase: %s", ifelse(is.finite(n_query), n_query, "NA"))),
      if (is.finite(n_unmatched) && n_unmatched > 0) {
        tags$p(sprintf("Unmatched input genes: %s", n_unmatched))
      },
      tags$p(sprintf("Background size: %s", ifelse(is.finite(n_background), n_background, "NA"))),
      tags$p(sprintf("Enriched terms: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  output$tools_goora_tabs <- renderUI({
    res <- rv$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "No GO-ORA results yet."))
    }

    # Calculate aspect ratio from current inputs
    w_in <- safe_num(input$tools_goora_width, defs_goora$style$width %||% 12)
    h_in <- safe_num(input$tools_goora_height, defs_goora$style$height %||% 6)
    ar <- w_in / h_in

    tagList(
      div(
        class = "tool-plot-box",
        style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
        plotOutput("tools_goora_plot", height = "100%")
      ),
      DT::DTOutput("tools_goora_table")
    )
  })

  # Reactive to re-render plot when plot options or ontology view changes
  output$tools_goora_plot <- renderPlot({
    res <- rv$results
    if (is.null(res)) {
      plot.new()
      text(0.5, 0.5, "No results yet.")
      return(invisible(NULL))
    }

    # Build current style from inputs (reactive to plot option changes)
    style <- list(
      plot_type = input$tools_goora_plot_type %||% defs_goora$style$plot_type %||% "bar",
      color_mode = input$tools_goora_color_mode %||% defs_goora$style$color_mode %||% "fdr",
      fdr_palette = input$tools_goora_fdr_palette %||% defs_goora$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_goora_flat_color %||% "")) {
        input$tools_goora_flat_color
      } else {
        defs_goora$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_goora_alpha, defs_goora$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_goora_show_go_id),
      font_size = safe_int(input$tools_goora_font_size, defs_goora$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_goora_axis_text_size, defs_goora$style$axis_text_size %||% 20),
      width = safe_num(input$tools_goora_width, defs_goora$style$width %||% 12),
      height = safe_num(input$tools_goora_height, defs_goora$style$height %||% 6),
      flip_axis = isTRUE(input$tools_goora_flip_axis),
      ontology_filter = input$tools_goora_ontology_view %||% "BP"  # Use view selector for filtering display
    )

    # Re-render with current style
    rend <- tb_render_goora(res, style, meta = NULL)
    rv$rendered <- rend

    # Get the plot for the selected ontology
    ont <- tolower(input$tools_goora_ontology_view %||% "BP")
    plot_key <- paste0(ont, "_plot")
    p <- rend$plots[[plot_key]]

    # Fallback to single plot if no tabs
    if (is.null(p) && length(rend$plots) > 0) {
      p <- rend$plots[[1]]
    }

    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No enriched terms for this ontology.")
      return(invisible(NULL))
    }
    suppressMessages(print(p))
  },
  width = function() plot_dims_px()$w,
  height = function() plot_dims_px()$h,
  res = 150
  )

  output$tools_goora_table <- DT::renderDT({
    rend <- rv$rendered
    if (is.null(rend)) {
      return(DT::datatable(data.frame()))
    }

    # Get the table for the selected ontology
    ont <- tolower(input$tools_goora_ontology_view %||% "BP")
    table_key <- paste0(ont, "_table")
    df <- rend$tables[[table_key]]

    # Fallback to single table if no tabs
    if (is.null(df) && length(rend$tables) > 0) {
      df <- rend$tables[[1]]
    }

    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame()))
    }

    n_rows <- nrow(df)
    DT::datatable(
      df,
      options = list(
        pageLength = min(15, n_rows),
        lengthMenu = list(c(10, 15, 25, 50), c("10", "15", "25", "50")),
        scrollX = TRUE,
        scrollY = "350px",
        scrollCollapse = TRUE,
        deferRender = TRUE,
        searchDelay = 350,
        autoWidth = FALSE
      ),
      rownames = FALSE
    )
  })
}

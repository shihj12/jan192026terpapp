# R/pages/tools/tool_1dgofcs.R
# 1D GO-FCS Tool - 1D Functional Class Scoring

tools_1dgofcs_defaults <- function() {
  eng <- msterp_engine_get("1dgofcs")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# Build protein_to_go and go_terms from annot_long if not already present
# This mirrors the logic in run_utils.R for pipeline execution
tools_1dgofcs_ensure_terpbase_go_mappings <- function(tb) {
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
# 1D GO-FCS Tool UI
# ============================================================
tools_1dgofcs_ui <- function() {
  defs <- tools_1dgofcs_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    div(
      class = "top",
      actionButton("tools_1dgofcs_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("1D GO-FCS", style = "margin: 0;")
    ),
    tags$p("Paste a gene list with scores to run 1D functional class scoring against a TerpBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("TerpBase"),
        uiOutput("tools_1dgofcs_terpbase_status"),
        selectInput(
          "tools_1dgofcs_terpbase_default_path",
          "Default TerpBase",
          choices = tools_default_terpbase_choices(),
          selected = ""
        ),
        actionButton(
          "tools_1dgofcs_terpbase_default_load",
          "Load default",
          class = "btn btn-default btn-sm btn-tool-action"
        ),
        fileInput(
          "tools_1dgofcs_terpbase_file",
          "Load TerpBase (.terpbase)",
          accept = c(".terpbase", ".rds"),
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Gene Input with Scores"),
        tags$p(class = "text-muted", "Enter gene/protein ID and score (tab or comma separated), one per line."),
        textAreaInput(
          "tools_1dgofcs_genes",
          "Gene list (ID<tab>Score per line)",
          rows = 10,
          placeholder = "TP53\t2.5\nEGFR\t-1.2\nBRCA1\t0.8"
        ),
        actionButton("tools_1dgofcs_run", "Run 1D GO-FCS", class = "btn-primary btn-tool-action"),
        hr(),
        tags$h4("Filter"),
        selectInput(
          "tools_1dgofcs_ontology_filter",
          "Ontology",
          choices = c("All" = "all", "Biological Process" = "BP",
                      "Molecular Function" = "MF", "Cellular Component" = "CC"),
          selected = "all"
        ),
        tools_collapse_section_ui(
          "tools_1dgofcs_params_section",
          "Parameters",
          open = FALSE,
          numericInput(
            "tools_1dgofcs_fdr_cutoff",
            "FDR cutoff",
            value = params_defaults$fdr_cutoff %||% 0.03,
            min = 0,
            max = 1,
            step = 0.001
          ),
          numericInput(
            "tools_1dgofcs_min_term_size",
            "Min term size",
            value = params_defaults$min_term_size %||% 5,
            min = 1,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_max_terms",
            "Terms to show (per ontology)",
            value = params_defaults$max_terms %||% 20,
            min = 1,
            max = 200,
            step = 1
          )
        ),
        tools_collapse_section_ui(
          "tools_1dgofcs_plot_section",
          "Plot Options",
          open = FALSE,
          selectInput(
            "tools_1dgofcs_plot_type",
            "Plot type",
            choices = c("bar", "dot"),
            selected = style_defaults$plot_type %||% "bar"
          ),
          selectInput(
            "tools_1dgofcs_color_mode",
            "Coloring",
            choices = c("fdr", "flat"),
            selected = style_defaults$color_mode %||% "fdr"
          ),
          conditionalPanel(
            condition = "input.tools_1dgofcs_color_mode == 'fdr'",
            selectInput(
              "tools_1dgofcs_fdr_palette",
              "FDR color palette",
              choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
              selected = style_defaults$fdr_palette %||% "yellow_cap"
            )
          ),
          conditionalPanel(
            condition = "input.tools_1dgofcs_color_mode == 'flat'",
            textInput(
              "tools_1dgofcs_flat_color",
              "Flat color (hex)",
              value = style_defaults$flat_color %||% "#B0B0B0"
            )
          ),
          sliderInput(
            "tools_1dgofcs_alpha",
            "Opacity",
            min = 0,
            max = 1,
            value = style_defaults$alpha %||% 0.8,
            step = 0.05
          ),
          checkboxInput(
            "tools_1dgofcs_show_go_id",
            "Show GO ID in labels",
            value = isTRUE(style_defaults$show_go_id %||% FALSE)
          ),
          checkboxInput(
            "tools_1dgofcs_flip_axis",
            "Flip horizontal axis",
            value = FALSE
          ),
          numericInput(
            "tools_1dgofcs_font_size",
            "Font size",
            value = style_defaults$font_size %||% 14,
            min = 6,
            max = 30,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_axis_text_size",
            "Axis text size",
            value = style_defaults$axis_text_size %||% 20,
            min = 6,
            max = 40,
            step = 1
          ),
          numericInput(
            "tools_1dgofcs_width",
            "Plot width (in)",
            value = style_defaults$width %||% 14,
            min = 2,
            max = 24,
            step = 0.5
          ),
          numericInput(
            "tools_1dgofcs_height",
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
        uiOutput("tools_1dgofcs_summary"),
        uiOutput("tools_1dgofcs_tabs")
      )
    )
  )
}

# ============================================================
# 1D GO-FCS Server Logic
# ============================================================
tools_1dgofcs_server <- function(input, output, session, app_state, rv_1dgofcs, defs_1dgofcs) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

  # Parse gene input with scores for 1D GO-FCS
  parse_gene_score_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(list(genes = character(0), scores = numeric(0)))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]

    genes <- character(0)
    scores <- numeric(0)

    for (line in lines) {
      # Split by tab or comma
      parts <- unlist(strsplit(line, "[\t,]"))
      parts <- trimws(parts)
      if (length(parts) >= 2) {
        gene <- parts[1]
        score <- suppressWarnings(as.numeric(parts[2]))
        if (nzchar(gene) && !is.na(score)) {
          genes <- c(genes, gene)
          scores <- c(scores, score)
        }
      }
    }

    list(genes = genes, scores = scores)
  }

  plot_dims_px_1dgofcs <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14)
    h_in <- safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6)

    max_px <- 2600
    w_px <- as.integer(round(w_in * dpi))
    h_px <- as.integer(round(h_in * dpi))

    scale <- min(1, max_px / max(w_px, h_px))
    list(
      w = as.integer(round(w_px * scale)),
      h = as.integer(round(h_px * scale))
    )
  }

  # TerpBase status for 1D GO-FCS
  output$tools_1dgofcs_terpbase_status <- renderUI({
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

  progress_1dgofcs_rx <- reactive({
    if (identical(rv_1dgofcs$run_status, "running")) {
      invalidateLater(500, session)
    }
    path <- rv_1dgofcs$progress_path
    if (is.null(path) || !file.exists(path)) return(NULL)
    tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
  })

  # Polling observer for background process completion

  observe({
    if (is.null(rv_1dgofcs$bg_process) || !identical(rv_1dgofcs$run_status, "running")) return()

    if (!rv_1dgofcs$bg_process$is_alive()) {
      result <- tryCatch(rv_1dgofcs$bg_process$get_result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))

      if (isTRUE(result$ok)) {
        rv_1dgofcs$results <- result$result
        rv_1dgofcs$rendered <- tb_render_1dgofcs(result$result, rv_1dgofcs$pending_style, meta = NULL)
        rv_1dgofcs$run_status <- "done"
        rv_1dgofcs$status_msg <- NULL
      } else {
        rv_1dgofcs$run_status <- "error"
        rv_1dgofcs$status_msg <- result$error %||% "Analysis failed"
        rv_1dgofcs$status_level <- "error"
      }
      rv_1dgofcs$bg_process <- NULL
      # Clean up progress file
      if (!is.null(rv_1dgofcs$progress_path) && file.exists(rv_1dgofcs$progress_path)) {
        tryCatch(unlink(rv_1dgofcs$progress_path), error = function(e) NULL)
      }
    } else {
      invalidateLater(500, session)
    }
  })

  # Run 1D GO-FCS
  observeEvent(input$tools_1dgofcs_run, {
    rv_1dgofcs$status_msg <- NULL
    rv_1dgofcs$status_level <- NULL
    rv_1dgofcs$results <- NULL
    rv_1dgofcs$rendered <- NULL
    rv_1dgofcs$input_count <- NULL
    rv_1dgofcs$run_status <- "idle"

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv_1dgofcs$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv_1dgofcs$status_level <- "error"
      return()
    }

    # Ensure terpbase has protein_to_go and go_terms mappings
    tb <- tools_1dgofcs_ensure_terpbase_go_mappings(tb)

    parsed <- parse_gene_score_input(input$tools_1dgofcs_genes)
    rv_1dgofcs$input_count <- length(parsed$genes)

    if (length(parsed$genes) == 0) {
      rv_1dgofcs$status_msg <- "Enter at least one gene with a score to run 1D GO-FCS."
      rv_1dgofcs$status_level <- "warn"
      return()
    }

    # Create named score vector
    scores <- parsed$scores
    names(scores) <- parsed$genes

    params <- list(
      fdr_cutoff = safe_num(input$tools_1dgofcs_fdr_cutoff, defs_1dgofcs$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_1dgofcs_min_term_size, defs_1dgofcs$params$min_term_size %||% 5),
      max_terms = safe_int(input$tools_1dgofcs_max_terms, defs_1dgofcs$params$max_terms %||% 20)
    )

    payload <- list(
      ok = TRUE,
      params = params,
      scores = scores,
      terpbase = tb
    )

    style <- list(
      plot_type = input$tools_1dgofcs_plot_type %||% defs_1dgofcs$style$plot_type %||% "bar",
      color_mode = input$tools_1dgofcs_color_mode %||% defs_1dgofcs$style$color_mode %||% "fdr",
      fdr_palette = input$tools_1dgofcs_fdr_palette %||% defs_1dgofcs$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_1dgofcs_flat_color %||% "")) {
        input$tools_1dgofcs_flat_color
      } else {
        defs_1dgofcs$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_1dgofcs_alpha, defs_1dgofcs$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_1dgofcs_show_go_id),
      font_size = safe_int(input$tools_1dgofcs_font_size, defs_1dgofcs$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_1dgofcs_axis_text_size, defs_1dgofcs$style$axis_text_size %||% 20),
      width = safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14),
      height = safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6),
      flip_axis = isTRUE(input$tools_1dgofcs_flip_axis),
      ontology_filter = input$tools_1dgofcs_ontology_filter %||% "all"
    )

    # Store style for later use when rendering
    rv_1dgofcs$pending_style <- style

    # Check if callr is available for async execution
    has_callr <- requireNamespace("callr", quietly = TRUE)

    if (has_callr) {
      # Async execution with callr
      rv_1dgofcs$progress_path <- tempfile(fileext = ".json")
      rv_1dgofcs$run_start_time <- Sys.time()
      rv_1dgofcs$run_status <- "running"

      # Write initial progress
      write_progress(rv_1dgofcs$progress_path, "running", "Starting 1D GO-FCS analysis...", 5)

      # Get app root for sourcing files
      app_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

      rv_1dgofcs$bg_process <- callr::r_bg(
        func = function(app_root, payload, params, progress_path) {
          # Source required files
          source(file.path(app_root, "R/engines/stats_go.R"), local = TRUE)
          source(file.path(app_root, "R/utils/utils.R"), local = TRUE)

          write_prog <- function(msg, pct) {
            obj <- list(status = "running", message = msg, pct = pct)
            tryCatch(jsonlite::write_json(obj, progress_path, auto_unbox = TRUE), error = function(e) NULL)
          }

          write_prog("Running functional class scoring...", 30)

          result <- tryCatch({
            res <- stats_1dgofcs_run(payload, params = params, context = list(terpbase = payload$terpbase))
            write_prog("Analysis complete!", 100)
            list(ok = TRUE, result = res)
          }, error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          })

          result
        },
        args = list(
          app_root = app_root,
          payload = payload,
          params = params,
          progress_path = rv_1dgofcs$progress_path
        ),
        package = TRUE,
        supervise = TRUE
      )
    } else {
      # Synchronous fallback
      rv_1dgofcs$run_status <- "running"

      res <- tryCatch({
        stats_1dgofcs_run(payload, params = params, context = list(terpbase = tb))
      }, error = function(e) {
        rv_1dgofcs$status_msg <- conditionMessage(e)
        rv_1dgofcs$status_level <- "error"
        rv_1dgofcs$run_status <- "error"
        NULL
      })

      if (!is.null(res)) {
        rv_1dgofcs$results <- res
        rv_1dgofcs$rendered <- tb_render_1dgofcs(res, style, meta = NULL)
        rv_1dgofcs$run_status <- "done"
      }
    }
  }, ignoreInit = TRUE)

  # Stop button handler
  observeEvent(input$tools_1dgofcs_stop, {
    if (!is.null(rv_1dgofcs$bg_process) && rv_1dgofcs$bg_process$is_alive()) {
      tryCatch(rv_1dgofcs$bg_process$kill(), error = function(e) NULL)
      rv_1dgofcs$bg_process <- NULL
      rv_1dgofcs$run_status <- "idle"
      rv_1dgofcs$status_msg <- "Analysis stopped by user"
      rv_1dgofcs$status_level <- "warn"
    }
  }, ignoreInit = TRUE)

  # 1D GO-FCS summary
  output$tools_1dgofcs_summary <- renderUI({
    # Show progress bar when running
    if (identical(rv_1dgofcs$run_status, "running")) {
      p <- progress_1dgofcs_rx()
      pct <- suppressWarnings(as.numeric(p$pct %||% 0))
      if (is.na(pct)) pct <- 0
      pct <- max(0, min(100, pct))
      msg <- p$message %||% "Processing..."

      return(tagList(
        div(class = "tool-status-row",
          div(class = "tool-status-pill running", "Running"),
          div(class = "tool-status-msg", msg),
          actionButton("tools_1dgofcs_stop", "Stop", class = "btn btn-danger btn-sm", style = "margin-left: auto;")
        ),
        div(class = "tool-progress-wrap",
          div(class = "tool-progress-bar",
            div(class = "tool-progress-fill active", style = sprintf("width:%s%%;", pct))
          )
        )
      ))
    }

    if (!is.null(rv_1dgofcs$status_msg)) {
      cls <- if (identical(rv_1dgofcs$status_level, "error")) "text-danger" else "text-warning"
      return(tags$div(class = cls, rv_1dgofcs$status_msg))
    }

    res <- rv_1dgofcs$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "Run 1D GO-FCS to see results."))
    }

    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv_1dgofcs$input_count %||% NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("1D GO-FCS summary"),
      tags$p(sprintf("Input genes with scores: %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Enriched terms: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  # 1D GO-FCS tabs
  output$tools_1dgofcs_tabs <- renderUI({
    rend <- rv_1dgofcs$rendered
    if (is.null(rend)) {
      return(tags$div(class = "text-muted", "No 1D GO-FCS results yet."))
    }

    # Calculate aspect ratio from current inputs
    w_in <- safe_num(input$tools_1dgofcs_width, defs_1dgofcs$style$width %||% 14)
    h_in <- safe_num(input$tools_1dgofcs_height, defs_1dgofcs$style$height %||% 6)
    ar <- w_in / h_in

    tabs <- rend$tabs %||% character(0)
    if (length(tabs) > 0) {
      tab_panels <- lapply(tabs, function(tab) {
        plot_id <- paste0("tools_1dgofcs_plot_", tolower(tab))
        table_id <- paste0("tools_1dgofcs_table_", tolower(tab))
        tabPanel(
          tab,
          div(
            class = "tool-plot-box",
            style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
            plotOutput(plot_id, height = "100%")
          ),
          DT::DTOutput(table_id)
        )
      })
      do.call(tabsetPanel, tab_panels)
    } else {
      tagList(
        div(
          class = "tool-plot-box",
          style = sprintf("--tool-plot-ar:%s;", format(ar, scientific = FALSE, trim = TRUE)),
          plotOutput("tools_1dgofcs_plot", height = "100%")
        ),
        DT::DTOutput("tools_1dgofcs_table")
      )
    }
  })

  # 1D GO-FCS render observer
  observeEvent(rv_1dgofcs$rendered, {
    rend <- rv_1dgofcs$rendered
    if (is.null(rend)) return(invisible(NULL))

    render_plot_1d <- function(plot_key) {
      p <- rend$plots[[plot_key]]
      if (is.null(p)) {
        plot.new()
        text(0.5, 0.5, "No plot available.")
        return(invisible(NULL))
      }
      suppressMessages(print(p))
    }

    render_table_1d <- function(table_key) {
      df <- rend$tables[[table_key]]
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
    }

    tabs <- rend$tabs %||% character(0)
    if (length(tabs) > 0) {
      for (tab in tabs) {
        plot_key <- paste0(tolower(tab), "_plot")
        table_key <- paste0(tolower(tab), "_table")
        plot_id <- paste0("tools_1dgofcs_plot_", tolower(tab))
        table_id <- paste0("tools_1dgofcs_table_", tolower(tab))

        local({
          pk <- plot_key
          tk <- table_key
          pid <- plot_id
          tid <- table_id
          output[[pid]] <- renderPlot({
            render_plot_1d(pk)
          },
          width = function() plot_dims_px_1dgofcs()$w,
          height = function() plot_dims_px_1dgofcs()$h,
          res = 150
          )
          output[[tid]] <- DT::renderDT({
            render_table_1d(tk)
          })
        })
      }
    } else {
      output$tools_1dgofcs_plot <- renderPlot({
        render_plot_1d("1dgofcs_plot")
      },
      width = function() plot_dims_px_1dgofcs()$w,
      height = function() plot_dims_px_1dgofcs()$h,
      res = 150
      )
      output$tools_1dgofcs_table <- DT::renderDT({
        render_table_1d("1dgofcs_table")
      })
    }
  }, ignoreInit = TRUE)
}

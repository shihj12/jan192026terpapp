library(shiny)

tools_goora_defaults <- function() {
  eng <- msterp_engine_get("goora")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

# ============================================================
# Tools Landing Page UI
# ============================================================
page_tools_ui <- function() {
  msterp_page(
    title = "Tools",
    tags$p("Standalone ad hoc utilities to complement the core MS Terp workflow."),
    uiOutput("tools_content")
  )
}

# ============================================================
# Tools Landing Page (card grid)
# ============================================================
tools_landing_ui <- function() {
  div(
    class = "grid",
    # GO-ORA card
    div(
      class = "card",
      tags$h3("GO-ORA"),
      tags$p("Gene Ontology over-representation analysis on gene lists."),
      actionButton("tools_open_goora", "Open GO-ORA", class = "btn btn-primary")
    ),
    # Labeling helper card
    div(
      class = "card",
      tags$h3("GO-FCS"),
      tags$p("Gene Ontology functional class scoring on gene lists."),
      tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Coming soon")
    ),
    # QC snapshots card
    div(
      class = "card",
      tags$h3("QC Report"),
      tags$p("QC reports for raw intensity distributions and missingness."),
      tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Coming soon")
    ),
    # Placeholder for future tool
    div(
      class = "card",
      tags$h3("More coming soon"),
      tags$p("Additional analysis tools and utilities are in development."),
      tags$button(type = "button", class = "btn btn-default", disabled = "disabled", "Stay tuned")
    )
  )
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
        fileInput(
          "tools_goora_terpbase_file",
          "Load TerpBase (.rds)",
          accept = ".rds",
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
        actionButton("tools_goora_run", "Run GO-ORA", class = "btn-primary"),
        hr(),
        tags$h4("Parameters"),
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
        ),
        hr(),
        tags$h4("Plot options"),
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
      ),
      right_ui = tagList(
        uiOutput("tools_goora_summary"),
        uiOutput("tools_goora_tabs")
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
page_tools_server <- function(input, output, session, app_state) {
  defs <- tools_goora_defaults()

  # Track which tool view is active: "landing" or "goora"
  current_tool <- reactiveVal("landing")

  rv <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL
  )

  # Render the appropriate content based on current_tool

output$tools_content <- renderUI({
    if (current_tool() == "goora") {
      tools_goora_ui()
    } else {
      tools_landing_ui()
    }
  })

  # Navigation between landing and GO-ORA
  observeEvent(input$tools_open_goora, {
    current_tool("goora")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_back, {
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for standalone GO-ORA
  observeEvent(input$tools_goora_terpbase_file, {
    req(input$tools_goora_terpbase_file)
    file_path <- input$tools_goora_terpbase_file$datapath
    tryCatch({
      tb <- readRDS(file_path)
      if (!is.null(tb) && is.list(tb)) {
        app_state$terpbase <- tb
        rv$status_msg <- "TerpBase loaded successfully."
        rv$status_level <- "success"
      } else {
        rv$status_msg <- "Invalid TerpBase file format."
        rv$status_level <- "error"
      }
    }, error = function(e) {
      rv$status_msg <- paste("Error loading TerpBase:", e$message)
      rv$status_level <- "error"
    })
  }, ignoreInit = TRUE)

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
    w_in <- safe_num(input$tools_goora_width, defs$style$width %||% 12)
    h_in <- safe_num(input$tools_goora_height, defs$style$height %||% 6)

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
      return()
    }

    genes <- parse_gene_input(input$tools_goora_genes)
    rv$input_count <- length(genes)

    if (length(genes) == 0) {
      rv$status_msg <- "Enter at least one gene symbol to run GO-ORA."
      rv$status_level <- "warn"
      return()
    }

    params <- list(
      fdr_cutoff = safe_num(input$tools_goora_fdr_cutoff, defs$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_goora_min_term_size, defs$params$min_term_size %||% 5),
      min_overlap = safe_int(input$tools_goora_min_overlap, defs$params$min_overlap %||% 1),
      max_terms = safe_int(input$tools_goora_max_terms, defs$params$max_terms %||% 20)
    )

    payload <- list(
      ok = TRUE,
      params = params,
      query_proteins = genes,
      terpbase = tb
    )

    res <- stats_goora_run(payload, params = params, context = list(terpbase = tb))

    style <- list(
      plot_type = input$tools_goora_plot_type %||% defs$style$plot_type %||% "bar",
      color_mode = input$tools_goora_color_mode %||% defs$style$color_mode %||% "fdr",
      fdr_palette = input$tools_goora_fdr_palette %||% defs$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_goora_flat_color %||% "")) {
        input$tools_goora_flat_color
      } else {
        defs$style$flat_color %||% "#B0B0B0"
      },
      alpha = safe_num(input$tools_goora_alpha, defs$style$alpha %||% 0.8),
      show_go_id = isTRUE(input$tools_goora_show_go_id),
      font_size = safe_int(input$tools_goora_font_size, defs$style$font_size %||% 14),
      axis_text_size = safe_int(input$tools_goora_axis_text_size, defs$style$axis_text_size %||% 20),
      width = safe_num(input$tools_goora_width, defs$style$width %||% 12),
      height = safe_num(input$tools_goora_height, defs$style$height %||% 6),
      flip_axis = isTRUE(input$tools_goora_flip_axis),
      ontology_filter = "all"
    )

    rv$results <- res
    rv$rendered <- tb_render_goora(res, style, meta = NULL)
  }, ignoreInit = TRUE)

  output$tools_goora_summary <- renderUI({
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
    rend <- rv$rendered
    if (is.null(rend)) {
      return(tags$div(class = "text-muted", "No GO-ORA results yet."))
    }

    tabs <- rend$tabs %||% character(0)
    if (length(tabs) > 0) {
      tab_panels <- lapply(tabs, function(tab) {
        plot_id <- paste0("tools_goora_plot_", tolower(tab))
        table_id <- paste0("tools_goora_table_", tolower(tab))
        tabPanel(
          tab,
          plotOutput(plot_id),
          DT::DTOutput(table_id)
        )
      })
      do.call(tabsetPanel, tab_panels)
    } else {
      tagList(
        plotOutput("tools_goora_plot"),
        DT::DTOutput("tools_goora_table")
      )
    }
  })

  observeEvent(rv$rendered, {
    rend <- rv$rendered
    if (is.null(rend)) return(invisible(NULL))

    render_plot <- function(plot_key) {
      p <- rend$plots[[plot_key]]
      if (is.null(p)) {
        plot.new()
        text(0.5, 0.5, "No plot available.")
        return(invisible(NULL))
      }
      suppressMessages(print(p))
    }

    render_table <- function(table_key) {
      df <- rend$tables[[table_key]]
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame()))
      }
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }

    tabs <- rend$tabs %||% character(0)
    if (length(tabs) > 0) {
      for (tab in tabs) {
        plot_key <- paste0(tolower(tab), "_plot")
        table_key <- paste0(tolower(tab), "_table")
        plot_id <- paste0("tools_goora_plot_", tolower(tab))
        table_id <- paste0("tools_goora_table_", tolower(tab))

        local({
          pk <- plot_key
          tk <- table_key
          pid <- plot_id
          tid <- table_id
          output[[pid]] <- renderPlot({
            render_plot(pk)
          },
          width = function() plot_dims_px()$w,
          height = function() plot_dims_px()$h,
          res = 150
          )
          output[[tid]] <- DT::renderDT({
            render_table(tk)
          })
        })
      }
    } else {
      output$tools_goora_plot <- renderPlot({
        render_plot("goora_plot")
      },
      width = function() plot_dims_px()$w,
      height = function() plot_dims_px()$h,
      res = 150
      )
      output$tools_goora_table <- DT::renderDT({
        render_table("goora_table")
      })
    }
  }, ignoreInit = TRUE)
}

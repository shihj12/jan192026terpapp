library(shiny)

tools_goora_defaults <- function() {
  eng <- msterp_engine_get("goora")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

tools_1dgofcs_defaults <- function() {
  eng <- msterp_engine_get("1dgofcs")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
  )
}

tools_2dgofcs_defaults <- function() {
  eng <- msterp_engine_get("2dgofcs")
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
    # 1D GO-FCS card
    div(
      class = "card",
      tags$h3("1D GO-FCS"),
      tags$p("1D functional class scoring on a ranked gene list with scores."),
      actionButton("tools_open_1dgofcs", "Open 1D GO-FCS", class = "btn btn-primary")
    ),
    # 2D GO-FCS card
    div(
      class = "card",
      tags$h3("2D GO-FCS"),
      tags$p("2D functional class scoring on two ranked gene lists (scatter plot)."),
      actionButton("tools_open_2dgofcs", "Open 2D GO-FCS", class = "btn btn-primary")
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
        fileInput(
          "tools_1dgofcs_terpbase_file",
          "Load TerpBase (.rds)",
          accept = ".rds",
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
        actionButton("tools_1dgofcs_run", "Run 1D GO-FCS", class = "btn-primary"),
        hr(),
        tags$h4("Parameters"),
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
        ),
        hr(),
        tags$h4("Plot options"),
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
      ),
      right_ui = tagList(
        uiOutput("tools_1dgofcs_summary"),
        uiOutput("tools_1dgofcs_tabs")
      )
    )
  )
}

# ============================================================
# 2D GO-FCS Tool UI
# ============================================================
tools_2dgofcs_ui <- function() {
  defs <- tools_2dgofcs_defaults()
  params_defaults <- defs$params %||% list()
  style_defaults <- defs$style %||% list()

  tagList(
    div(
      class = "top",
      actionButton("tools_2dgofcs_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("2D GO-FCS", style = "margin: 0;")
    ),
    tags$p("Paste a gene list with two score columns to run 2D functional class scoring against a TerpBase."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("TerpBase"),
        uiOutput("tools_2dgofcs_terpbase_status"),
        fileInput(
          "tools_2dgofcs_terpbase_file",
          "Load TerpBase (.rds)",
          accept = ".rds",
          placeholder = "No file selected"
        ),
        hr(),
        tags$h4("Gene Input with Two Scores"),
        tags$p(class = "text-muted", "Enter gene/protein ID and two scores (tab or comma separated), one per line."),
        textAreaInput(
          "tools_2dgofcs_genes",
          "Gene list (ID<tab>Score_X<tab>Score_Y per line)",
          rows = 10,
          placeholder = "TP53\t2.5\t1.3\nEGFR\t-1.2\t0.5\nBRCA1\t0.8\t-0.9"
        ),
        textInput(
          "tools_2dgofcs_x_label",
          "X-axis label",
          value = "Score X"
        ),
        textInput(
          "tools_2dgofcs_y_label",
          "Y-axis label",
          value = "Score Y"
        ),
        actionButton("tools_2dgofcs_run", "Run 2D GO-FCS", class = "btn-primary"),
        hr(),
        tags$h4("Parameters"),
        numericInput(
          "tools_2dgofcs_fdr_cutoff",
          "FDR cutoff",
          value = params_defaults$fdr_cutoff %||% 0.03,
          min = 0,
          max = 1,
          step = 0.001
        ),
        numericInput(
          "tools_2dgofcs_min_term_size",
          "Min term size",
          value = params_defaults$min_term_size %||% 5,
          min = 1,
          step = 1
        ),
        numericInput(
          "tools_2dgofcs_max_terms",
          "Terms to show (per ontology)",
          value = params_defaults$max_terms %||% 20,
          min = 1,
          max = 200,
          step = 1
        ),
        hr(),
        tags$h4("Plot options"),
        selectInput(
          "tools_2dgofcs_color_mode",
          "Coloring",
          choices = c("fdr", "flat"),
          selected = style_defaults$color_mode %||% "fdr"
        ),
        conditionalPanel(
          condition = "input.tools_2dgofcs_color_mode == 'fdr'",
          selectInput(
            "tools_2dgofcs_fdr_palette",
            "FDR color palette",
            choices = c("yellow_cap" = "Yellow (significant)", "blue_red" = "Blue-Red"),
            selected = style_defaults$fdr_palette %||% "yellow_cap"
          )
        ),
        conditionalPanel(
          condition = "input.tools_2dgofcs_color_mode == 'flat'",
          textInput(
            "tools_2dgofcs_flat_color",
            "Flat color (hex)",
            value = style_defaults$flat_color %||% "#B0B0B0"
          )
        ),
        sliderInput(
          "tools_2dgofcs_dot_alpha",
          "Dot opacity",
          min = 0,
          max = 1,
          value = style_defaults$dot_alpha %||% 1,
          step = 0.05
        ),
        checkboxInput(
          "tools_2dgofcs_show_ref_lines",
          "Show x=0 and y=0 reference lines",
          value = isTRUE(style_defaults$show_ref_lines %||% TRUE)
        ),
        checkboxInput(
          "tools_2dgofcs_show_diagonal_guides",
          "Show y=x and y=-x guidelines",
          value = isTRUE(style_defaults$show_diagonal_guides %||% TRUE)
        ),
        numericInput(
          "tools_2dgofcs_label_font_size",
          "Label font size",
          value = style_defaults$label_font_size %||% 12,
          min = 6,
          max = 30,
          step = 1
        ),
        numericInput(
          "tools_2dgofcs_axis_text_size",
          "Axis text size",
          value = style_defaults$axis_text_size %||% 20,
          min = 6,
          max = 40,
          step = 1
        ),
        numericInput(
          "tools_2dgofcs_width",
          "Plot width (in)",
          value = style_defaults$width %||% 8,
          min = 2,
          max = 24,
          step = 0.5
        ),
        numericInput(
          "tools_2dgofcs_height",
          "Plot height (in)",
          value = style_defaults$height %||% 6,
          min = 2,
          max = 24,
          step = 0.5
        )
      ),
      right_ui = tagList(
        uiOutput("tools_2dgofcs_summary"),
        uiOutput("tools_2dgofcs_tabs")
      )
    )
  )
}

# ============================================================
# Server
# ============================================================
page_tools_server <- function(input, output, session, app_state) {
  defs_goora <- tools_goora_defaults()
  defs_1dgofcs <- tools_1dgofcs_defaults()
  defs_2dgofcs <- tools_2dgofcs_defaults()

  # Track which tool view is active: "landing", "goora", "1dgofcs", or "2dgofcs"
  current_tool <- reactiveVal("landing")

  # Reactive values for GO-ORA
  rv <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL
  )

  # Reactive values for 1D GO-FCS
  rv_1dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL
  )

  # Reactive values for 2D GO-FCS
  rv_2dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL
  )

  # Render the appropriate content based on current_tool
  output$tools_content <- renderUI({
    tool <- current_tool()
    if (tool == "goora") {
      tools_goora_ui()
    } else if (tool == "1dgofcs") {
      tools_1dgofcs_ui()
    } else if (tool == "2dgofcs") {
      tools_2dgofcs_ui()
    } else {
      tools_landing_ui()
    }
  })

  # Navigation between landing and tools
  observeEvent(input$tools_open_goora, {
    current_tool("goora")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_back, {
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_1dgofcs, {
    current_tool("1dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_back, {
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_2dgofcs, {
    current_tool("2dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_2dgofcs_back, {
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

  # Handle TerpBase file upload for 1D GO-FCS
  observeEvent(input$tools_1dgofcs_terpbase_file, {
    req(input$tools_1dgofcs_terpbase_file)
    file_path <- input$tools_1dgofcs_terpbase_file$datapath
    tryCatch({
      tb <- readRDS(file_path)
      if (!is.null(tb) && is.list(tb)) {
        app_state$terpbase <- tb
        rv_1dgofcs$status_msg <- "TerpBase loaded successfully."
        rv_1dgofcs$status_level <- "success"
      } else {
        rv_1dgofcs$status_msg <- "Invalid TerpBase file format."
        rv_1dgofcs$status_level <- "error"
      }
    }, error = function(e) {
      rv_1dgofcs$status_msg <- paste("Error loading TerpBase:", e$message)
      rv_1dgofcs$status_level <- "error"
    })
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for 2D GO-FCS
  observeEvent(input$tools_2dgofcs_terpbase_file, {
    req(input$tools_2dgofcs_terpbase_file)
    file_path <- input$tools_2dgofcs_terpbase_file$datapath
    tryCatch({
      tb <- readRDS(file_path)
      if (!is.null(tb) && is.list(tb)) {
        app_state$terpbase <- tb
        rv_2dgofcs$status_msg <- "TerpBase loaded successfully."
        rv_2dgofcs$status_level <- "success"
      } else {
        rv_2dgofcs$status_msg <- "Invalid TerpBase file format."
        rv_2dgofcs$status_level <- "error"
      }
    }, error = function(e) {
      rv_2dgofcs$status_msg <- paste("Error loading TerpBase:", e$message)
      rv_2dgofcs$status_level <- "error"
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

  plot_dims_px_2dgofcs <- function() {
    dpi <- 150
    w_in <- safe_num(input$tools_2dgofcs_width, defs_2dgofcs$style$width %||% 8)
    h_in <- safe_num(input$tools_2dgofcs_height, defs_2dgofcs$style$height %||% 6)

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
      fdr_cutoff = safe_num(input$tools_goora_fdr_cutoff, defs_goora$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_goora_min_term_size, defs_goora$params$min_term_size %||% 5),
      min_overlap = safe_int(input$tools_goora_min_overlap, defs_goora$params$min_overlap %||% 1),
      max_terms = safe_int(input$tools_goora_max_terms, defs_goora$params$max_terms %||% 20)
    )

    payload <- list(
      ok = TRUE,
      params = params,
      query_proteins = genes,
      terpbase = tb
    )

    res <- stats_goora_run(payload, params = params, context = list(terpbase = tb))

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

  # ============================================================
  # 1D GO-FCS Server Logic
  # ============================================================

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

  # Run 1D GO-FCS
  observeEvent(input$tools_1dgofcs_run, {
    rv_1dgofcs$status_msg <- NULL
    rv_1dgofcs$status_level <- NULL
    rv_1dgofcs$results <- NULL
    rv_1dgofcs$rendered <- NULL
    rv_1dgofcs$input_count <- NULL

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv_1dgofcs$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv_1dgofcs$status_level <- "error"
      return()
    }

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

    res <- stats_1dgofcs_run(payload, params = params, context = list(terpbase = tb))

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
      ontology_filter = "all"
    )

    rv_1dgofcs$results <- res
    rv_1dgofcs$rendered <- tb_render_1dgofcs(res, style, meta = NULL)
  }, ignoreInit = TRUE)

  # 1D GO-FCS summary
  output$tools_1dgofcs_summary <- renderUI({
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

    tabs <- rend$tabs %||% character(0)
    if (length(tabs) > 0) {
      tab_panels <- lapply(tabs, function(tab) {
        plot_id <- paste0("tools_1dgofcs_plot_", tolower(tab))
        table_id <- paste0("tools_1dgofcs_table_", tolower(tab))
        tabPanel(
          tab,
          plotOutput(plot_id),
          DT::DTOutput(table_id)
        )
      })
      do.call(tabsetPanel, tab_panels)
    } else {
      tagList(
        plotOutput("tools_1dgofcs_plot"),
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

  # ============================================================
  # 2D GO-FCS Server Logic
  # ============================================================

  # TerpBase status for 2D GO-FCS
  output$tools_2dgofcs_terpbase_status <- renderUI({
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

  # Parse gene input with two scores for 2D GO-FCS
  parse_gene_2score_input <- function(text) {
    if (is.null(text) || !nzchar(text)) return(list(genes = character(0), scores_x = numeric(0), scores_y = numeric(0)))
    lines <- unlist(strsplit(text, "\\r?\\n"))
    lines <- trimws(lines)
    lines <- lines[nzchar(lines)]

    genes <- character(0)
    scores_x <- numeric(0)
    scores_y <- numeric(0)

    for (line in lines) {
      # Split by tab or comma
      parts <- unlist(strsplit(line, "[\t,]"))
      parts <- trimws(parts)
      if (length(parts) >= 3) {
        gene <- parts[1]
        score_x <- suppressWarnings(as.numeric(parts[2]))
        score_y <- suppressWarnings(as.numeric(parts[3]))
        if (nzchar(gene) && !is.na(score_x) && !is.na(score_y)) {
          genes <- c(genes, gene)
          scores_x <- c(scores_x, score_x)
          scores_y <- c(scores_y, score_y)
        }
      }
    }

    list(genes = genes, scores_x = scores_x, scores_y = scores_y)
  }

  # Run 2D GO-FCS
  observeEvent(input$tools_2dgofcs_run, {
    rv_2dgofcs$status_msg <- NULL
    rv_2dgofcs$status_level <- NULL
    rv_2dgofcs$results <- NULL
    rv_2dgofcs$rendered <- NULL
    rv_2dgofcs$input_count <- NULL

    tb <- app_state$terpbase
    if (is.null(tb)) {
      rv_2dgofcs$status_msg <- "No TerpBase loaded. Load one above or build one in the TerpBase page."
      rv_2dgofcs$status_level <- "error"
      return()
    }

    parsed <- parse_gene_2score_input(input$tools_2dgofcs_genes)
    rv_2dgofcs$input_count <- length(parsed$genes)

    if (length(parsed$genes) == 0) {
      rv_2dgofcs$status_msg <- "Enter at least one gene with two scores to run 2D GO-FCS."
      rv_2dgofcs$status_level <- "warn"
      return()
    }

    # Create named score vectors
    scores_x <- parsed$scores_x
    names(scores_x) <- parsed$genes
    scores_y <- parsed$scores_y
    names(scores_y) <- parsed$genes

    params <- list(
      fdr_cutoff = safe_num(input$tools_2dgofcs_fdr_cutoff, defs_2dgofcs$params$fdr_cutoff %||% 0.03),
      min_term_size = safe_int(input$tools_2dgofcs_min_term_size, defs_2dgofcs$params$min_term_size %||% 5),
      max_terms = safe_int(input$tools_2dgofcs_max_terms, defs_2dgofcs$params$max_terms %||% 20)
    )

    # Get axis labels from input
    x_label <- input$tools_2dgofcs_x_label %||% "Score X"
    y_label <- input$tools_2dgofcs_y_label %||% "Score Y"

    payload <- list(
      ok = TRUE,
      params = params,
      scores_x = scores_x,
      scores_y = scores_y,
      x_score_label = x_label,
      y_score_label = y_label,
      terpbase = tb
    )

    res <- stats_2dgofcs_run(payload, params = params, context = list(terpbase = tb))

    style <- list(
      color_mode = input$tools_2dgofcs_color_mode %||% defs_2dgofcs$style$color_mode %||% "fdr",
      fdr_palette = input$tools_2dgofcs_fdr_palette %||% defs_2dgofcs$style$fdr_palette %||% "yellow_cap",
      flat_color = if (nzchar(input$tools_2dgofcs_flat_color %||% "")) {
        input$tools_2dgofcs_flat_color
      } else {
        defs_2dgofcs$style$flat_color %||% "#B0B0B0"
      },
      dot_alpha = safe_num(input$tools_2dgofcs_dot_alpha, defs_2dgofcs$style$dot_alpha %||% 1),
      show_ref_lines = isTRUE(input$tools_2dgofcs_show_ref_lines),
      show_diagonal_guides = isTRUE(input$tools_2dgofcs_show_diagonal_guides),
      label_font_size = safe_int(input$tools_2dgofcs_label_font_size, defs_2dgofcs$style$label_font_size %||% 12),
      axis_text_size = safe_int(input$tools_2dgofcs_axis_text_size, defs_2dgofcs$style$axis_text_size %||% 20),
      width = safe_num(input$tools_2dgofcs_width, defs_2dgofcs$style$width %||% 8),
      height = safe_num(input$tools_2dgofcs_height, defs_2dgofcs$style$height %||% 6),
      ontology_filter = "all"
    )

    rv_2dgofcs$results <- res
    rv_2dgofcs$rendered <- tb_render_2dgofcs(res, style, meta = NULL)
  }, ignoreInit = TRUE)

  # 2D GO-FCS summary
  output$tools_2dgofcs_summary <- renderUI({
    if (!is.null(rv_2dgofcs$status_msg)) {
      cls <- if (identical(rv_2dgofcs$status_level, "error")) "text-danger" else "text-warning"
      return(tags$div(class = cls, rv_2dgofcs$status_msg))
    }

    res <- rv_2dgofcs$results
    if (is.null(res)) {
      return(tags$div(class = "text-muted", "Run 2D GO-FCS to see results."))
    }

    n_terms <- if (is.data.frame(res$data$terms %||% NULL)) nrow(res$data$terms) else 0L
    n_input <- rv_2dgofcs$input_count %||% NA_integer_

    log_df <- res$data$log %||% NULL
    log_msg <- NULL
    if (!is.null(log_df) && nrow(log_df) > 0) {
      last <- log_df[nrow(log_df), , drop = FALSE]
      log_msg <- paste0("[", last$level, "] ", last$message)
    }

    tags$div(
      class = "card",
      tags$h4("2D GO-FCS summary"),
      tags$p(sprintf("Input genes with scores: %s", ifelse(is.finite(n_input), n_input, "NA"))),
      tags$p(sprintf("Enriched terms: %s", n_terms)),
      if (!is.null(log_msg)) tags$p(log_msg)
    )
  })

  # 2D GO-FCS tabs (scatter plot)
  output$tools_2dgofcs_tabs <- renderUI({
    rend <- rv_2dgofcs$rendered
    if (is.null(rend)) {
      return(tags$div(class = "text-muted", "No 2D GO-FCS results yet."))
    }

    # 2D GO-FCS typically shows a single scatter plot
    tagList(
      plotOutput("tools_2dgofcs_plot"),
      DT::DTOutput("tools_2dgofcs_table")
    )
  })

  # 2D GO-FCS render observer
  observeEvent(rv_2dgofcs$rendered, {
    rend <- rv_2dgofcs$rendered
    if (is.null(rend)) return(invisible(NULL))

    # Get the first available plot
    plot_key <- names(rend$plots)[1] %||% "2dgofcs_plot"
    table_key <- names(rend$tables)[1] %||% "2dgofcs_table"

    output$tools_2dgofcs_plot <- renderPlot({
      p <- rend$plots[[plot_key]]
      if (is.null(p)) {
        plot.new()
        text(0.5, 0.5, "No plot available.")
        return(invisible(NULL))
      }
      suppressMessages(print(p))
    },
    width = function() plot_dims_px_2dgofcs()$w,
    height = function() plot_dims_px_2dgofcs()$h,
    res = 150
    )

    output$tools_2dgofcs_table <- DT::renderDT({
      df <- rend$tables[[table_key]]
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(DT::datatable(data.frame()))
      }
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  }, ignoreInit = TRUE)
}

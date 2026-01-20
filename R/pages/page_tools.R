# R/pages/page_tools.R
# Tools Landing Page - Orchestrates individual tool modules

library(shiny)

# Source individual tool modules
source(file.path("R", "pages", "tools", "tool_goora.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_1dgofcs.R"), local = FALSE)
source(file.path("R", "pages", "tools", "tool_2dgofcs.R"), local = FALSE)

# ============================================================
# Shared Utilities
# ============================================================
tools_default_terpbase_choices <- function() {
  base_dir <- "terpbase"
  if (!dir.exists(base_dir)) {
    return(c("No default terpbase found" = ""))
  }

  files <- list.files(base_dir, pattern = "\\.(terpbase|rds)$", ignore.case = TRUE)
  if (length(files) == 0) {
    return(c("No default terpbase found" = ""))
  }

  files <- sort(files)
  stats::setNames(file.path(base_dir, files), files)
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
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL
  )

  # Reactive values for 1D GO-FCS
  rv_1dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL
  )

  # Reactive values for 2D GO-FCS
  rv_2dgofcs <- reactiveValues(
    results = NULL,
    rendered = NULL,
    status_msg = NULL,
    status_level = NULL,
    input_count = NULL,
    # Stored parameters to persist across navigation
    stored_params = NULL,
    stored_genes = NULL,
    stored_x_label = NULL,
    stored_y_label = NULL
  )

  # Shared TerpBase loading function
  tools_load_terpbase <- function(path, rv_target) {
    path <- as.character(path %||% "")
    if (!nzchar(path)) {
      rv_target$status_msg <- "No default TerpBase available."
      rv_target$status_level <- "error"
      return(invisible(FALSE))
    }

    if (!file.exists(path)) {
      rv_target$status_msg <- paste("TerpBase file not found:", path)
      rv_target$status_level <- "error"
      return(invisible(FALSE))
    }

    tryCatch({
      tb <- terpbase_load(path)
      if (!is.null(tb) && is.list(tb)) {
        app_state$terpbase <- tb
        rv_target$status_msg <- "TerpBase loaded successfully."
        rv_target$status_level <- "success"
      } else {
        rv_target$status_msg <- "Invalid TerpBase file format."
        rv_target$status_level <- "error"
      }
    }, error = function(e) {
      rv_target$status_msg <- paste("Error loading TerpBase:", e$message)
      rv_target$status_level <- "error"
    })
  }

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
    # Save current parameters before leaving
    rv$stored_params <- list(
      fdr_cutoff = input$tools_goora_fdr_cutoff,
      min_term_size = input$tools_goora_min_term_size,
      min_overlap = input$tools_goora_min_overlap,
      max_terms = input$tools_goora_max_terms,
      ontology_view = input$tools_goora_ontology_view,
      plot_type = input$tools_goora_plot_type,
      color_mode = input$tools_goora_color_mode,
      fdr_palette = input$tools_goora_fdr_palette,
      flat_color = input$tools_goora_flat_color,
      alpha = input$tools_goora_alpha,
      show_go_id = input$tools_goora_show_go_id,
      flip_axis = input$tools_goora_flip_axis,
      font_size = input$tools_goora_font_size,
      axis_text_size = input$tools_goora_axis_text_size,
      width = input$tools_goora_width,
      height = input$tools_goora_height
    )
    rv$stored_genes <- input$tools_goora_genes
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_1dgofcs, {
    current_tool("1dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_back, {
    # Save current parameters before leaving
    rv_1dgofcs$stored_params <- list(
      fdr_cutoff = input$tools_1dgofcs_fdr_cutoff,
      min_term_size = input$tools_1dgofcs_min_term_size,
      max_terms = input$tools_1dgofcs_max_terms,
      ontology_view = input$tools_1dgofcs_ontology_view,
      plot_type = input$tools_1dgofcs_plot_type,
      color_mode = input$tools_1dgofcs_color_mode,
      fdr_palette = input$tools_1dgofcs_fdr_palette,
      flat_color = input$tools_1dgofcs_flat_color,
      alpha = input$tools_1dgofcs_alpha,
      show_go_id = input$tools_1dgofcs_show_go_id,
      flip_axis = input$tools_1dgofcs_flip_axis,
      font_size = input$tools_1dgofcs_font_size,
      axis_text_size = input$tools_1dgofcs_axis_text_size,
      width = input$tools_1dgofcs_width,
      height = input$tools_1dgofcs_height
    )
    rv_1dgofcs$stored_genes <- input$tools_1dgofcs_genes
    current_tool("landing")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_open_2dgofcs, {
    current_tool("2dgofcs")
  }, ignoreInit = TRUE)

  observeEvent(input$tools_2dgofcs_back, {
    # Save current parameters before leaving
    rv_2dgofcs$stored_params <- list(
      fdr_cutoff = input$tools_2dgofcs_fdr_cutoff,
      min_term_size = input$tools_2dgofcs_min_term_size,
      max_terms = input$tools_2dgofcs_max_terms,
      ontology_view = input$tools_2dgofcs_ontology_view,
      color_mode = input$tools_2dgofcs_color_mode,
      fdr_palette = input$tools_2dgofcs_fdr_palette,
      flat_color = input$tools_2dgofcs_flat_color,
      dot_alpha = input$tools_2dgofcs_dot_alpha,
      show_ref_lines = input$tools_2dgofcs_show_ref_lines,
      show_diagonal_guides = input$tools_2dgofcs_show_diagonal_guides,
      label_font_size = input$tools_2dgofcs_label_font_size,
      axis_text_size = input$tools_2dgofcs_axis_text_size,
      width = input$tools_2dgofcs_width,
      height = input$tools_2dgofcs_height
    )
    rv_2dgofcs$stored_genes <- input$tools_2dgofcs_genes
    rv_2dgofcs$stored_x_label <- input$tools_2dgofcs_x_label
    rv_2dgofcs$stored_y_label <- input$tools_2dgofcs_y_label
    current_tool("landing")
  }, ignoreInit = TRUE)

  # Helper to restore parameters after UI is rendered
  # Uses observe + invalidateLater for a delayed update without shinyjs
  tools_restore_after_delay <- function(restore_fn, delay_ms = 150) {
    restore_done <- FALSE
    observe({
      if (!restore_done) {
        invalidateLater(delay_ms, session)
        restore_done <<- TRUE
      } else {
        restore_fn()
      }
    })
  }

  # Restore stored parameters when tools are opened
  observeEvent(current_tool(), {
    tool <- current_tool()

    # Restore GO-ORA parameters
    if (tool == "goora" && !is.null(rv$stored_params)) {
      p <- rv$stored_params
      genes <- rv$stored_genes
      # Schedule restore after UI renders
      observe({
        invalidateLater(150, session)
      }, once = TRUE)
      observe({
        # Check if input exists (UI has rendered)
        req(input$tools_goora_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_goora_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_goora_min_term_size", value = p$min_term_size)
          if (!is.null(p$min_overlap)) updateNumericInput(session, "tools_goora_min_overlap", value = p$min_overlap)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_goora_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_goora_ontology_view", selected = p$ontology_view)
          if (!is.null(p$plot_type)) updateSelectInput(session, "tools_goora_plot_type", selected = p$plot_type)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_goora_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_goora_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_goora_flat_color", value = p$flat_color)
          if (!is.null(p$alpha)) updateSliderInput(session, "tools_goora_alpha", value = p$alpha)
          if (!is.null(p$show_go_id)) updateCheckboxInput(session, "tools_goora_show_go_id", value = p$show_go_id)
          if (!is.null(p$flip_axis)) updateCheckboxInput(session, "tools_goora_flip_axis", value = p$flip_axis)
          if (!is.null(p$font_size)) updateNumericInput(session, "tools_goora_font_size", value = p$font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_goora_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_goora_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_goora_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_goora_genes", value = genes)
        })
      }, once = TRUE)
    }

    # Restore 1D GO-FCS parameters
    if (tool == "1dgofcs" && !is.null(rv_1dgofcs$stored_params)) {
      p <- rv_1dgofcs$stored_params
      genes <- rv_1dgofcs$stored_genes
      observe({
        req(input$tools_1dgofcs_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_1dgofcs_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_1dgofcs_min_term_size", value = p$min_term_size)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_1dgofcs_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_1dgofcs_ontology_view", selected = p$ontology_view)
          if (!is.null(p$plot_type)) updateSelectInput(session, "tools_1dgofcs_plot_type", selected = p$plot_type)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_1dgofcs_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_1dgofcs_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_1dgofcs_flat_color", value = p$flat_color)
          if (!is.null(p$alpha)) updateSliderInput(session, "tools_1dgofcs_alpha", value = p$alpha)
          if (!is.null(p$show_go_id)) updateCheckboxInput(session, "tools_1dgofcs_show_go_id", value = p$show_go_id)
          if (!is.null(p$flip_axis)) updateCheckboxInput(session, "tools_1dgofcs_flip_axis", value = p$flip_axis)
          if (!is.null(p$font_size)) updateNumericInput(session, "tools_1dgofcs_font_size", value = p$font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_1dgofcs_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_1dgofcs_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_1dgofcs_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_1dgofcs_genes", value = genes)
        })
      }, once = TRUE)
    }

    # Restore 2D GO-FCS parameters
    if (tool == "2dgofcs" && !is.null(rv_2dgofcs$stored_params)) {
      p <- rv_2dgofcs$stored_params
      genes <- rv_2dgofcs$stored_genes
      x_label <- rv_2dgofcs$stored_x_label
      y_label <- rv_2dgofcs$stored_y_label
      observe({
        req(input$tools_2dgofcs_fdr_cutoff)
        isolate({
          if (!is.null(p$fdr_cutoff)) updateNumericInput(session, "tools_2dgofcs_fdr_cutoff", value = p$fdr_cutoff)
          if (!is.null(p$min_term_size)) updateNumericInput(session, "tools_2dgofcs_min_term_size", value = p$min_term_size)
          if (!is.null(p$max_terms)) updateNumericInput(session, "tools_2dgofcs_max_terms", value = p$max_terms)
          if (!is.null(p$ontology_view)) updateSelectInput(session, "tools_2dgofcs_ontology_view", selected = p$ontology_view)
          if (!is.null(p$color_mode)) updateSelectInput(session, "tools_2dgofcs_color_mode", selected = p$color_mode)
          if (!is.null(p$fdr_palette)) updateSelectInput(session, "tools_2dgofcs_fdr_palette", selected = p$fdr_palette)
          if (!is.null(p$flat_color)) updateTextInput(session, "tools_2dgofcs_flat_color", value = p$flat_color)
          if (!is.null(p$dot_alpha)) updateSliderInput(session, "tools_2dgofcs_dot_alpha", value = p$dot_alpha)
          if (!is.null(p$show_ref_lines)) updateCheckboxInput(session, "tools_2dgofcs_show_ref_lines", value = p$show_ref_lines)
          if (!is.null(p$show_diagonal_guides)) updateCheckboxInput(session, "tools_2dgofcs_show_diagonal_guides", value = p$show_diagonal_guides)
          if (!is.null(p$label_font_size)) updateNumericInput(session, "tools_2dgofcs_label_font_size", value = p$label_font_size)
          if (!is.null(p$axis_text_size)) updateNumericInput(session, "tools_2dgofcs_axis_text_size", value = p$axis_text_size)
          if (!is.null(p$width)) updateNumericInput(session, "tools_2dgofcs_width", value = p$width)
          if (!is.null(p$height)) updateNumericInput(session, "tools_2dgofcs_height", value = p$height)
          if (!is.null(genes)) updateTextAreaInput(session, "tools_2dgofcs_genes", value = genes)
          if (!is.null(x_label)) updateTextInput(session, "tools_2dgofcs_x_label", value = x_label)
          if (!is.null(y_label)) updateTextInput(session, "tools_2dgofcs_y_label", value = y_label)
        })
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for standalone GO-ORA
  observeEvent(input$tools_goora_terpbase_default_load, {
    tools_load_terpbase(input$tools_goora_terpbase_default_path, rv)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_goora_terpbase_file, {
    req(input$tools_goora_terpbase_file)
    tools_load_terpbase(input$tools_goora_terpbase_file$datapath, rv)
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for 1D GO-FCS
  observeEvent(input$tools_1dgofcs_terpbase_default_load, {
    tools_load_terpbase(input$tools_1dgofcs_terpbase_default_path, rv_1dgofcs)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_1dgofcs_terpbase_file, {
    req(input$tools_1dgofcs_terpbase_file)
    tools_load_terpbase(input$tools_1dgofcs_terpbase_file$datapath, rv_1dgofcs)
  }, ignoreInit = TRUE)

  # Handle TerpBase file upload for 2D GO-FCS
  observeEvent(input$tools_2dgofcs_terpbase_default_load, {
    tools_load_terpbase(input$tools_2dgofcs_terpbase_default_path, rv_2dgofcs)
  }, ignoreInit = TRUE)

  observeEvent(input$tools_2dgofcs_terpbase_file, {
    req(input$tools_2dgofcs_terpbase_file)
    tools_load_terpbase(input$tools_2dgofcs_terpbase_file$datapath, rv_2dgofcs)
  }, ignoreInit = TRUE)

  # Initialize tool-specific server logic
  tools_goora_server(input, output, session, app_state, rv, defs_goora)
  tools_1dgofcs_server(input, output, session, app_state, rv_1dgofcs, defs_1dgofcs)
  tools_2dgofcs_server(input, output, session, app_state, rv_2dgofcs, defs_2dgofcs)
}

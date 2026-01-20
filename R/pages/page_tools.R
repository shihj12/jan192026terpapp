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

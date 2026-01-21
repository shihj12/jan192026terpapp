# R/pages/tools/tool_metabobase.R
# MetaboBase Builder Tool - Build and validate MetaboBase files for metabolite analyses

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# ============================================================
# MetaboBase Tool Defaults
# ============================================================
tools_metabobase_defaults <- function() {
  list(
    params = list(),
    style = list()
  )
}

# Helper to list available metabobase files
tools_default_metabobase_choices <- function() {
  base_dir <- "metabobase"
  if (!dir.exists(base_dir)) {
    return(c("No default metabobase found" = ""))
  }

  files <- list.files(base_dir, pattern = "\\.(metabobase|rds)$", ignore.case = TRUE)
  if (length(files) == 0) {
    return(c("No default metabobase found" = ""))
  }

  files <- sort(files)
  stats::setNames(file.path(base_dir, files), files)
}

# ============================================================
# MetaboBase Tool UI
# ============================================================
tools_metabobase_ui <- function() {
  tagList(
    div(
      class = "top",
      actionButton("tools_metabobase_back", "Back to Tools", class = "btn btn-default"),
      tags$h3("MetaboBase Builder", style = "margin: 0;")
    ),
    tags$p("Build a MetaboBase annotation database for metabolite pathway and class enrichment analyses."),
    two_panel_ui(
      left_ui = tagList(
        tags$h4("Mode"),
        radioButtons(
          "tools_metabobase_mode",
          NULL,
          choices = c(
            "Build from file (CSV/Excel)" = "build_csv",
            "Build from KEGG (online)" = "build_kegg",
            "Validate existing .metabobase" = "validate"
          ),
          selected = "build_csv"
        ),
        hr(),

        # Build from file mode (CSV or Excel)
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'build_csv'",
          tags$h4("File Input"),
          fileInput(
            "tools_metabobase_csv_file",
            "Metabolite file (CSV or Excel)",
            accept = c(".csv", ".txt", ".xlsx", ".xls")
          ),
          uiOutput("tools_metabobase_sheet_ui"),
          uiOutput("tools_metabobase_column_mapping_ui"),
          textInput("tools_metabobase_csv_library_name", "Library name", value = ""),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_build_csv", "Build MetaboBase", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        # Build from KEGG mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'build_kegg'",
          tags$h4("KEGG Library Builder"),
          selectInput(
            "tools_metabobase_kegg_organism",
            "Organism",
            choices = c(
              "Human (hsa)" = "hsa",
              "Mouse (mmu)" = "mmu",
              "Rat (rno)" = "rno",
              "Fruit fly (dme)" = "dme",
              "Yeast (sce)" = "sce",
              "E. coli (eco)" = "eco",
              "Arabidopsis (ath)" = "ath"
            ),
            selected = "hsa"
          ),
          textInput("tools_metabobase_kegg_library_name", "Library name (optional)", value = ""),
          tags$p(class = "text-muted", style = "font-size: 12px;",
            "This will fetch pathway data from the KEGG API in the background. You can continue using the app."
          ),
          uiOutput("tools_metabobase_kegg_buttons_ui"),
          uiOutput("tools_metabobase_kegg_progress_ui")
        ),

        # Validate mode
        conditionalPanel(
          condition = "input.tools_metabobase_mode == 'validate'",
          tags$h4("MetaboBase File"),
          fileInput(
            "tools_metabobase_file",
            "Select .metabobase file",
            accept = c(".metabobase", ".rds")
          ),
          div(
            style = "display: flex; gap: 8px; margin-top: 10px;",
            actionButton("tools_metabobase_validate", "Validate", class = "btn-primary btn-tool-action"),
            actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
          )
        ),

        hr(),
        uiOutput("tools_metabobase_download_ui"),

        hr(),
        tools_collapse_section_ui(
          "tools_metabobase_csv_info_section",
          "CSV Format Requirements",
          open = FALSE,
          tags$p(tags$strong("Required columns:")),
          tags$ul(
            tags$li(tags$code("metabolite_id"), " - Primary identifier"),
            tags$li(tags$code("name"), " - Common metabolite name")
          ),
          tags$p(tags$strong("Optional columns:")),
          tags$ul(
            tags$li(tags$code("hmdb_id"), " - HMDB ID"),
            tags$li(tags$code("kegg_id"), " - KEGG compound ID"),
            tags$li(tags$code("chebi_id"), " - ChEBI ID"),
            tags$li(tags$code("formula"), " - Molecular formula"),
            tags$li(tags$code("class"), " - Chemical class"),
            tags$li(tags$code("subclass"), " - Chemical subclass"),
            tags$li(tags$code("superclass"), " - Chemical superclass"),
            tags$li(tags$code("pathway_kegg"), " - KEGG pathway IDs (semicolon-separated)"),
            tags$li(tags$code("pathway_reactome"), " - Reactome pathway IDs (semicolon-separated)")
          )
        )
      ),
      right_ui = div(
        class = "tool-results",
        uiOutput("tools_metabobase_status"),
        hr(),
        uiOutput("tools_metabobase_summary"),
        hr(),
        tags$h4("Messages"),
        verbatimTextOutput("tools_metabobase_messages")
      )
    )
  )
}

# ============================================================
# MetaboBase Tool Server
# ============================================================
tools_metabobase_server <- function(input, output, session, app_state, rv_metabobase) {

  # Message helper
  msg_push <- function(x) {
    rv_metabobase$messages <- c(rv_metabobase$messages, paste0(format(Sys.time(), "%H:%M:%S"), "  ", x))
  }

  # Format time helper
  format_hms <- function(seconds) {
    s <- max(0L, as.integer(round(seconds)))
    h <- s %/% 3600L
    m <- (s %% 3600L) %/% 60L
    sec <- s %% 60L
    sprintf("%02dh %02dm %02ds", h, m, sec)
  }

  # Busy indicator helper
  set_busy <- function(active, message = "", percent = NULL) {
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active = active, message = message, percent = percent)
    }
  }

  # Status icons
  icon_ok  <- function() tags$span(style = "color:#1a7f37;font-weight:700;margin-right:8px;", HTML("&#10003;"))
  icon_bad <- function() tags$span(style = "color:#d1242f;font-weight:700;margin-right:8px;", HTML("&#10007;"))

  # KEGG build state tracking
  rv_kegg <- reactiveValues(
    running = FALSE,
    start_time = NULL,
    organism = NULL,
    process = NULL,      # callr process object
    result_file = NULL   # temp file for result
  )

  # File type and Excel sheet tracking
  rv_file <- reactiveValues(
    is_excel = FALSE,
    sheets = NULL,
    columns = NULL
  )

  # Detect file type and get sheets for Excel
  observe({
    f <- input$tools_metabobase_csv_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
      rv_file$is_excel <- FALSE
      rv_file$sheets <- NULL
      rv_file$columns <- NULL
      return()
    }

    ext <- tolower(tools::file_ext(f$name))
    rv_file$is_excel <- ext %in% c("xlsx", "xls")

    if (rv_file$is_excel) {
      tryCatch({
        rv_file$sheets <- readxl::excel_sheets(f$datapath)
      }, error = function(e) {
        rv_file$sheets <- NULL
        msg_push(paste0("Error reading Excel sheets: ", conditionMessage(e)))
      })
    } else {
      rv_file$sheets <- NULL
    }
  })

  # Get columns from selected sheet or CSV
  observe({
    f <- input$tools_metabobase_csv_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) {
      rv_file$columns <- NULL
      return()
    }

    tryCatch({
      if (rv_file$is_excel) {
        sheet <- input$tools_metabobase_sheet %||% 1
        # Read just the header row
        hdr <- readxl::read_excel(f$datapath, sheet = sheet, n_max = 0)
        rv_file$columns <- names(hdr)
      } else {
        hdr <- read.csv(f$datapath, nrows = 0, check.names = FALSE)
        rv_file$columns <- names(hdr)
      }
    }, error = function(e) {
      rv_file$columns <- NULL
    })
  })

  # Sheet selection UI (only for Excel files)
  output$tools_metabobase_sheet_ui <- renderUI({
    if (!rv_file$is_excel || is.null(rv_file$sheets)) return(NULL)

    selectInput(
      "tools_metabobase_sheet",
      "Select sheet",
      choices = stats::setNames(seq_along(rv_file$sheets), rv_file$sheets),
      selected = 1
    )
  })

  # Column mapping UI
  output$tools_metabobase_column_mapping_ui <- renderUI({
    cols <- rv_file$columns
    if (is.null(cols) || length(cols) == 0) return(NULL)

    # Try to auto-detect columns
    cols_lower <- tolower(cols)
    default_id <- which(cols_lower %in% c("metabolite_id", "id", "compound_id", "hmdb_id", "kegg_id"))[1]
    default_name <- which(cols_lower %in% c("name", "metabolite_name", "compound_name", "metabolite"))[1]

    if (is.na(default_id)) default_id <- 1
    if (is.na(default_name)) default_name <- if (length(cols) > 1) 2 else 1

    tagList(
      tags$h5("Column Mapping", style = "margin-top: 15px;"),
      tags$p(class = "text-muted", style = "font-size: 12px;",
        "Select which columns contain the metabolite ID and name."
      ),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
        selectInput(
          "tools_metabobase_col_id",
          "Metabolite ID column",
          choices = stats::setNames(cols, cols),
          selected = cols[default_id]
        ),
        selectInput(
          "tools_metabobase_col_name",
          "Name column",
          choices = stats::setNames(cols, cols),
          selected = cols[default_name]
        )
      ),
      tags$details(
        style = "margin-top: 10px;",
        tags$summary(style = "cursor: pointer; font-size: 12px; color: #666;", "Optional column mappings"),
        div(
          style = "margin-top: 10px; display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
          selectInput(
            "tools_metabobase_col_class",
            "Class column (optional)",
            choices = c("(none)" = "", stats::setNames(cols, cols)),
            selected = cols[which(cols_lower == "class")[1]] %||% ""
          ),
          selectInput(
            "tools_metabobase_col_pathway",
            "Pathway column (optional)",
            choices = c("(none)" = "", stats::setNames(cols, cols)),
            selected = cols[which(cols_lower %in% c("pathway", "pathway_kegg", "kegg_pathway"))[1]] %||% ""
          )
        )
      )
    )
  })

  # Validation check (now uses column mapping)
  file_validation_check <- reactive({
    f <- input$tools_metabobase_csv_file
    if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)

    col_id <- input$tools_metabobase_col_id
    col_name <- input$tools_metabobase_col_name

    # Check if columns are selected
    if (is.null(col_id) || is.null(col_name) || !nzchar(col_id) || !nzchar(col_name)) {
      return(list(ok = FALSE, message = "Please select ID and Name columns"))
    }

    list(ok = TRUE, message = "Ready to build")
  })

  # Clear state when new file selected
  observeEvent(input$tools_metabobase_csv_file, {
    rv_metabobase$metabo <- NULL
    f <- input$tools_metabobase_csv_file
    if (!is.null(f)) {
      ext <- tolower(tools::file_ext(f$name))
      if (ext %in% c("xlsx", "xls")) {
        msg_push(paste0("Selected Excel file: ", f$name))
      } else {
        msg_push(paste0("Selected file: ", f$name))
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$tools_metabobase_file, {
    rv_metabobase$metabo <- NULL
    msg_push("Selected a .metabobase file.")
  }, ignoreInit = TRUE)

  # Reset button
  observeEvent(input$tools_metabobase_reset, {
    rv_metabobase$metabo <- NULL
    rv_metabobase$messages <- character(0)
    rv_metabobase$status_msg <- NULL
    rv_metabobase$status_level <- NULL
  }, ignoreInit = TRUE)

  # Build from file (CSV or Excel)
  observeEvent(input$tools_metabobase_build_csv, {
    req(input$tools_metabobase_csv_file)

    # Validate column selection
    chk <- file_validation_check()
    if (is.null(chk) || !isTRUE(chk$ok)) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("Build blocked: ", chk$message %||% "Invalid configuration"))
      showNotification(chk$message %||% "Please select ID and Name columns", type = "error")
      return()
    }

    rv_metabobase$metabo <- NULL
    msg_push("Starting build...")

    t0 <- Sys.time()

    # Progress tracking
    prog <- 0
    progress_set <- function(value, detail = NULL) {
      prog <<- max(0, min(1, as.numeric(value)))
      set_busy(TRUE, message = detail %||% "Working...", percent = prog * 100)
    }
    progress_inc <- function(amount, detail = NULL) {
      prog <<- max(0, min(1, prog + as.numeric(amount)))
      set_busy(TRUE, message = detail %||% "Working...", percent = prog * 100)
    }

    set_busy(TRUE, "Starting...", 0)
    on.exit(set_busy(FALSE, "", NULL), add = TRUE)

    # Get column mapping
    col_mapping <- list(
      metabolite_id = input$tools_metabobase_col_id,
      name = input$tools_metabobase_col_name,
      class = if (nzchar(input$tools_metabobase_col_class %||% "")) input$tools_metabobase_col_class else NULL,
      pathway = if (nzchar(input$tools_metabobase_col_pathway %||% "")) input$tools_metabobase_col_pathway else NULL
    )

    # Get sheet for Excel files
    sheet <- if (rv_file$is_excel) (input$tools_metabobase_sheet %||% 1) else NULL

    tryCatch({
      metabo <- metabobase_build_from_file(
        path = input$tools_metabobase_csv_file$datapath,
        library_name = input$tools_metabobase_csv_library_name,
        sheet = sheet,
        col_mapping = col_mapping,
        progress_inc = progress_inc,
        progress_set = progress_set
      )

      v <- metabobase_validate(metabo)
      if (!v$ok) stop(paste(v$errors, collapse = "\n"))

      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      dt_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      msg_push(paste0("Build complete. Time: ", format_hms(dt_sec)))
      showNotification("MetaboBase built successfully.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("Build failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # KEGG buttons UI (dynamic based on running state)
  output$tools_metabobase_kegg_buttons_ui <- renderUI({
    running <- rv_kegg$running
    div(
      style = "display: flex; gap: 8px; margin-top: 10px;",
      if (!running) {
        actionButton("tools_metabobase_build_kegg", "Build from KEGG", class = "btn-primary btn-tool-action")
      } else {
        actionButton("tools_metabobase_cancel_kegg", "Cancel", class = "btn btn-warning btn-tool-action")
      },
      actionButton("tools_metabobase_reset", "Reset", class = "btn btn-default btn-tool-action")
    )
  })

  # Build from KEGG (background task using callr)
  observeEvent(input$tools_metabobase_build_kegg, {
    organism <- input$tools_metabobase_kegg_organism %||% "hsa"
    library_name <- input$tools_metabobase_kegg_library_name

    rv_metabobase$metabo <- NULL
    rv_kegg$running <- TRUE
    rv_kegg$start_time <- Sys.time()
    rv_kegg$organism <- organism
    rv_kegg$result_file <- tempfile(fileext = ".rds")

    msg_push(paste0("Starting background KEGG build for organism: ", organism))
    msg_push("You can continue using the app while this runs.")

    # Start background R process
    rv_kegg$process <- callr::r_bg(
      func = function(org, lib_name, out_file) {
        # Source the metabobase engine in the subprocess
        source("R/engines/metabobase.R", local = TRUE)

        result <- tryCatch({
          metabo <- metabobase_build_kegg_library(
            organism = org,
            library_name = if (nzchar(lib_name %||% "")) lib_name else NULL,
            output_file = NULL,
            progress_callback = function(msg) message(msg)
          )
          list(ok = TRUE, data = metabo, error = NULL)
        }, error = function(e) {
          list(ok = FALSE, data = NULL, error = conditionMessage(e))
        })

        saveRDS(result, out_file)
        result
      },
      args = list(
        org = organism,
        lib_name = library_name,
        out_file = rv_kegg$result_file
      ),
      supervise = TRUE
    )
  }, ignoreInit = TRUE)

  # Poll for KEGG task completion
  observe({
    req(rv_kegg$running, rv_kegg$process)
    invalidateLater(500, session)

    proc <- rv_kegg$process
    if (!proc$is_alive()) {
      # Process finished
      rv_kegg$running <- FALSE

      tryCatch({
        # Read result from temp file
        if (file.exists(rv_kegg$result_file)) {
          result <- readRDS(rv_kegg$result_file)
          unlink(rv_kegg$result_file)

          if (isTRUE(result$ok)) {
            metabo <- result$data

            v <- metabobase_validate(metabo)
            if (!v$ok) stop(paste(v$errors, collapse = "\n"))

            rv_metabobase$metabo <- metabo
            app_state$metabobase <- metabo

            dt_sec <- as.numeric(difftime(Sys.time(), rv_kegg$start_time, units = "secs"))
            msg_push(paste0("KEGG build complete. Time: ", format_hms(dt_sec)))
            showNotification("MetaboBase built from KEGG.", type = "message")
          } else {
            stop(result$error %||% "Unknown error")
          }
        } else {
          # Check if process had an error
          err <- tryCatch(proc$get_result(), error = function(e) conditionMessage(e))
          stop(if (is.character(err)) err else "Process failed without result")
        }
      }, error = function(e) {
        rv_metabobase$metabo <- NULL
        msg_push(paste0("KEGG build failed: ", conditionMessage(e)))
        showNotification(conditionMessage(e), type = "error")
      })

      rv_kegg$process <- NULL
    }
  })

  # Cancel KEGG build
  observeEvent(input$tools_metabobase_cancel_kegg, {
    if (!is.null(rv_kegg$process) && rv_kegg$process$is_alive()) {
      rv_kegg$process$kill()
    }
    rv_kegg$running <- FALSE
    rv_kegg$process <- NULL
    if (!is.null(rv_kegg$result_file) && file.exists(rv_kegg$result_file)) {
      unlink(rv_kegg$result_file)
    }
    msg_push("KEGG build cancelled.")
    showNotification("KEGG build cancelled.", type = "warning")
  }, ignoreInit = TRUE)

  # KEGG progress UI
  output$tools_metabobase_kegg_progress_ui <- renderUI({
    if (!rv_kegg$running) return(NULL)

    elapsed <- as.numeric(difftime(Sys.time(), rv_kegg$start_time, units = "secs"))
    invalidateLater(1000, session)

    tags$div(
      class = "kegg-progress",
      style = "margin-top: 10px; padding: 10px; background: #f0f7ff; border-radius: 4px; border: 1px solid #b3d7ff;",
      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$div(class = "spinner-border spinner-border-sm text-primary", role = "status"),
        tags$span(paste0("Building KEGG library for ", rv_kegg$organism, "..."))
      ),
      tags$div(
        style = "margin-top: 5px; font-size: 12px; color: #666;",
        paste0("Elapsed: ", format_hms(elapsed), " - App remains responsive")
      )
    )
  })

  # Validate existing MetaboBase
  observeEvent(input$tools_metabobase_validate, {
    req(input$tools_metabobase_file)

    rv_metabobase$metabo <- NULL
    msg_push("Validating .metabobase file.")

    tryCatch({
      metabo <- metabobase_load(input$tools_metabobase_file$datapath)
      rv_metabobase$metabo <- metabo
      app_state$metabobase <- metabo

      msg_push("Validation complete.")
      showNotification("MetaboBase validated and loaded.", type = "message")

    }, error = function(e) {
      rv_metabobase$metabo <- NULL
      msg_push(paste0("Validation failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error")
    })
  }, ignoreInit = TRUE)

  # Status UI
  output$tools_metabobase_status <- renderUI({
    mode <- input$tools_metabobase_mode %||% "build_csv"

    if (identical(mode, "build_csv")) {
      chk <- csv_header_check()
      required <- msterp_metabobase_required_csv_cols()

      if (is.null(chk)) {
        return(tagList(
          tags$h4("Status"),
          tags$p(tags$strong("Upload a metabolite CSV file."))
        ))
      }

      ok_set <- chk$present
      # Case-insensitive matching
      ok_set_lower <- tolower(ok_set)

      items <- lapply(required, function(col) {
        has <- tolower(col) %in% ok_set_lower
        tags$div(
          if (has) icon_ok() else icon_bad(),
          tags$span(col),
          style = "margin-bottom:6px;"
        )
      })

      return(tagList(
        tags$h4("Column Validation"),
        tags$div(items),
        tags$hr(),
        if (isTRUE(chk$ok)) {
          tags$div(icon_ok(), tags$strong("CSV columns: OK"))
        } else {
          tagList(
            tags$div(icon_bad(), tags$strong("CSV columns: MISSING REQUIRED")),
            tags$p(class = "text-muted", "Required: metabolite_id, name")
          )
        }
      ))
    }

    if (identical(mode, "build_kegg")) {
      organism <- input$tools_metabobase_kegg_organism %||% "hsa"
      return(tagList(
        tags$h4("Status"),
        tags$p("Ready to build KEGG library for: ", tags$strong(organism)),
        tags$p(class = "text-muted", "Click 'Build from KEGG' to start.")
      ))
    }

    # Validate mode
    f <- input$tools_metabobase_file
    if (is.null(f)) {
      return(tagList(
        tags$h4("Status"),
        tags$p(tags$strong("Select a .metabobase file and click Validate."))
      ))
    }

    if (is.null(rv_metabobase$metabo)) {
      return(tagList(
        tags$h4("Status"),
        icon_bad(), tags$strong("Not validated yet.")
      ))
    }

    v <- metabobase_validate(rv_metabobase$metabo)
    tagList(
      tags$h4("Status"),
      if (v$ok) {
        tags$div(icon_ok(), tags$strong("MetaboBase: Valid"))
      } else {
        tagList(
          tags$div(icon_bad(), tags$strong("MetaboBase: INVALID")),
          tags$ul(lapply(v$errors, tags$li))
        )
      }
    )
  })

  # Summary UI
  output$tools_metabobase_summary <- renderUI({
    metabo <- rv_metabobase$metabo
    if (is.null(metabo)) {
      return(tags$div(class = "text-muted", "No MetaboBase loaded."))
    }

    lines <- metabobase_summary_lines(metabo)
    tagList(
      tags$h4("Library Summary"),
      tags$pre(paste(lines, collapse = "\n"))
    )
  })

  # Messages
  output$tools_metabobase_messages <- renderText({
    if (!length(rv_metabobase$messages)) return(" ")
    paste(rv_metabobase$messages, collapse = "\n")
  })

  # Download button UI
  output$tools_metabobase_download_ui <- renderUI({
    if (is.null(rv_metabobase$metabo)) return(NULL)
    downloadButton("tools_metabobase_download", "Download .metabobase", class = "btn-success btn-tool-action")
  })

  # Download handler
  output$tools_metabobase_download <- downloadHandler(
    filename = function() {
      metabo <- rv_metabobase$metabo
      mode <- input$tools_metabobase_mode %||% "build_csv"

      base <- if (mode == "build_csv") {
        input$tools_metabobase_csv_library_name
      } else if (mode == "build_kegg") {
        input$tools_metabobase_kegg_library_name
      } else {
        ""
      }

      if (is.null(base) || !nzchar(base)) {
        base <- if (!is.null(metabo) && nzchar(metabo$library_name %||% "")) metabo$library_name else "metabobase"
      }
      paste0(base, ".metabobase")
    },
    content = function(file) {
      metabo <- isolate(rv_metabobase$metabo)
      if (is.null(metabo)) stop("No library built/validated yet.")
      metabobase_save(metabo, file = file)
    },
    contentType = "application/octet-stream"
  )
}

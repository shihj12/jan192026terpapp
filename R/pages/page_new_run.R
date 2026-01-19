# =========================================================
# R/pages/page_new_run.R — New Run (terpbook runner UI)
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

nr_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)


nr_sanitize <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  if (!nzchar(x)) return("run")
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

nr_fileinput_path <- function(f) {
  if (is.null(f) || is.null(f$datapath) || !nzchar(f$datapath)) return(NULL)
  f$datapath
}

nr_ext <- function(path) tolower(tools::file_ext(path %||% ""))

nr_ui_validate_terpbase <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("terpbase", "rds")) return(list(ok = FALSE, msg = "Expected .terpbase/.rds"))
  obj <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(obj, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  list(ok = TRUE, msg = "OK")
}


nr_ui_validate_formatted <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("xlsx", "xls")) return(list(ok = FALSE, msg = "Expected .xlsx (data+design sheets)"))
  
  if (!requireNamespace("readxl", quietly = TRUE)) return(list(ok = FALSE, msg = "readxl not installed"))
  
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
  if (is.null(sh)) return(list(ok = FALSE, msg = "Cannot read sheets"))
  sh_l <- tolower(sh)
  
  if (!all(c("data", "design") %in% sh_l)) {
    return(list(ok = FALSE, msg = "Missing required sheets: data, design"))
  }
  
  # Read design (small)
  design <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = sh[match("design", sh_l)], n_max = 5000)),
    error = function(e) e
  )
  if (inherits(design, "error") || !is.data.frame(design)) return(list(ok = FALSE, msg = "Cannot read design sheet"))
  
  nms <- tolower(names(design))
  if (!("record_type" %in% nms)) return(list(ok = FALSE, msg = "design missing record_type"))
  
  # Required for parsing meta/groups/cols
  need_any <- c("record_type")
  if (!all(need_any %in% nms)) return(list(ok = FALSE, msg = "design missing required columns"))
  
  # Meta checks
  meta_ok <- all(c("key", "value") %in% nms)
  if (!meta_ok) return(list(ok = FALSE, msg = "design meta requires key,value"))
  
  meta <- design[tolower(design$record_type) == "meta", , drop = FALSE]
  keys <- tolower(as.character(meta$key %||% character()))
  if (!any(keys == "analysis_level")) return(list(ok = FALSE, msg = "meta missing analysis_level"))
  if (!any(keys == "id_primary_col")) return(list(ok = FALSE, msg = "meta missing id_primary_col"))
  
  # Column mapping checks
  col_need <- c("include", "data_col", "group_name", "color", "replicate")
  if (!all(col_need %in% nms)) return(list(ok = FALSE, msg = paste0("design column records require: ", paste(col_need, collapse = ", "))))
  
  cols <- design[tolower(design$record_type) == "column", , drop = FALSE]
  if (nrow(cols) == 0) return(list(ok = FALSE, msg = "No column records in design"))
  
  include <- cols$include
  include <- if (is.logical(include)) include else tolower(as.character(include)) %in% c("true","t","1","yes")
  cols_in <- cols[include %in% TRUE, , drop = FALSE]
  if (nrow(cols_in) == 0) return(list(ok = FALSE, msg = "No included measurement columns"))
  
  # Read just header of data sheet
  data_head <- tryCatch(
    as.data.frame(readxl::read_excel(path, sheet = sh[match("data", sh_l)], n_max = 1)),
    error = function(e) e
  )
  if (inherits(data_head, "error") || !is.data.frame(data_head)) return(list(ok = FALSE, msg = "Cannot read data sheet"))
  
  data_cols <- names(data_head)
  meas <- as.character(cols_in$data_col %||% character())
  missing_meas <- setdiff(meas, data_cols)
  if (length(missing_meas) > 0) return(list(ok = FALSE, msg = paste0("Missing measurement columns in data: ", missing_meas[[1]])))
  
  list(ok = TRUE, msg = paste0("OK (", length(meas), " measurement cols)"))
}


nr_ui_validate_terpflow <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(list(ok = FALSE, msg = "Missing"))
  ex <- nr_ext(path)
  if (!ex %in% c("terpflow", "rds")) return(list(ok = FALSE, msg = "Expected .terpflow"))
  flow <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(flow, "error")) return(list(ok = FALSE, msg = "Cannot readRDS"))
  if (!is.list(flow)) return(list(ok = FALSE, msg = "Invalid object"))
  steps <- flow$steps %||% NULL
  if (!is.list(steps) || length(steps) == 0) return(list(ok = FALSE, msg = "No steps"))
  ok_engine <- all(vapply(steps, function(s) is.list(s) && nzchar(s$engine_id %||% ""), logical(1)))
  if (!isTRUE(ok_engine)) return(list(ok = FALSE, msg = "Step missing engine_id"))
  list(ok = TRUE, msg = "OK")
}

page_new_run_ui <- function() {
  msterp_page(
    title = NULL,
    actions = NULL,
    div(
      class = "nr-wrap",
      tags$head(
        tags$style(HTML("
        .nr-wrap { height: 100%; min-height: 0; display: flex; flex-direction: column; }
        .nr-main { flex: 1; min-height: 0; display: flex; padding: 20px 0; justify-content: center; }
        .nr-main .msterp-gate { flex: 1 1 auto; }

        /* Running panel: full width */
        .nr-running-panel {
          flex: 1; max-width: 800px; width: 100%;
          background: var(--md-card-bg); border: 1px solid var(--md-card-border); border-radius: 12px;
          padding: 16px; overflow: auto; min-height: 0;
          box-shadow: var(--md-card-shadow);
        }

        .nr-title { font-weight: 800; font-size: 18px; margin-bottom: 12px; text-align: center; }

        .nr-row { display: flex; align-items: center; justify-content: space-between; gap: 10px; }
        .nr-light { display: inline-flex; align-items: center; gap: 8px; font-weight: 700; }
        .nr-dot { width: 10px; height: 10px; border-radius: 999px; display: inline-block; }
        .nr-dot.red { background: #d11; }
        .nr-dot.green { background: #1a8f3a; }
        .nr-dot.gray { background: #999; }
        .nr-msg { color: #333; font-size: 12px; font-weight: 700; }

        .nr-section { border-top: 1px solid var(--md-border); margin-top: 10px; padding-top: 10px; }

        .nr-progress-wrap { margin-top: 10px; }
        .nr-progress-bar {
          height: 10px; background: #eee; border-radius: 999px; overflow: hidden; border: 1px solid #ddd;
        }
        .nr-progress-fill {
          height: 10px; background: #111; width: 0%;
          transition: width 0.3s ease;
        }
        .nr-progress-fill.active {
          background: repeating-linear-gradient(
            -45deg,
            #333,
            #333 10px,
            #111 10px,
            #111 20px
          );
          background-size: 28px 28px;
          animation: progress-stripe 0.6s linear infinite;
        }
        @keyframes progress-stripe {
          0% { background-position: 0 0; }
          100% { background-position: 28px 0; }
        }

        .nr-elapsed {
          font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
          font-size: 13px;
          font-weight: 700;
          color: #555;
        }

        .nr-status-pill {
          display: inline-block; padding: 6px 10px; border-radius: 999px;
          border: 1px solid var(--md-border); background: #fff; font-weight: 700; font-size: 12px;
        }

        /* Flickering effect for running state */
        .nr-status-pill.running {
          background-color: #fef3cd;
          border-color: #ffc107;
          animation: pulse-running 1.5s infinite;
        }
        @keyframes pulse-running {
          0%, 100% { opacity: 1; box-shadow: 0 0 4px rgba(255, 193, 7, 0.4); }
          50% { opacity: 0.7; box-shadow: 0 0 8px rgba(255, 193, 7, 0.8); }
        }

        .nr-log {
          white-space: pre-wrap;
          overflow-wrap: anywhere;
          font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
          font-size: 12.5px;
          line-height: 1.5;
          tab-size: 2;
          padding: 12px 14px;
          border: 1px solid #333;
          border-radius: 12px;
          background: #1a1a1a;
          color: #e8e8e8;
          min-height: 240px;
          max-height: 520px;
          overflow: auto;
          display: flex;
          flex-direction: column;
          justify-content: flex-start;
        }

        .nr-actions { margin-top: 16px; display: flex; gap: 10px; flex-wrap: wrap; }
      "))
      ,
        tags$script(HTML("
        (function() {
          function initNrLogAutoScroll() {
            var el = document.querySelector('.nr-log');
            if (!el || el.dataset.nrAutoScrollInit === '1') return;
            el.dataset.nrAutoScrollInit = '1';

            var paused = false;
            function isAtBottom() {
              return (el.scrollHeight - el.scrollTop - el.clientHeight) < 8;
            }
            function updatePaused() {
              paused = !isAtBottom();
            }

            el.addEventListener('scroll', function() {
              updatePaused();
            }, { passive: true });

            updatePaused();

            var obs = new MutationObserver(function() {
              if (!paused) {
                el.scrollTop = el.scrollHeight;
              }
            });

            obs.observe(el, { childList: true, subtree: true, characterData: true });
          }

          document.addEventListener('DOMContentLoaded', initNrLogAutoScroll);
          document.addEventListener('shiny:connected', initNrLogAutoScroll);
          setInterval(initNrLogAutoScroll, 1000);
        })();
      "))
      ),

      div(
        class = "nr-main",
        uiOutput("nr_main_panel")
      )
    )
  )
}

page_new_run_server <- function(input, output, session, app_state = NULL, state = NULL) {

  rv <- reactiveValues(
    run_root = NULL,
    manifest_path = NULL,
    progress_path = NULL,
    log_path = NULL,
    last_progress = NULL,
    last_status = "idle",
    last_msg = "",
    last_err = NULL,
    ui_log = character(0),
    bg_process = NULL,  # Track background process
    run_start_time = NULL,  # Track when run started for elapsed time
    # Store original file names for display in log
    formatted_filename = NULL,
    terpflow_filename = NULL,
    terpbase_filename = NULL
  )

  set_busy <- function(active, message = "", percent = NULL) {
    if (exists("msterp_set_busy", mode = "function", inherits = TRUE)) {
      msterp_set_busy(session, active = active, message = message, percent = percent)
      try(session$flushReact(), silent = TRUE)
      try(shiny::flushReact(), silent = TRUE)
    }
  }

  reset_file_input <- function(id) {
    session$sendCustomMessage("msterp_reset_file_input", list(id = id))
  }

  # Helper to append log messages and update UI
  ui_log <- function(msg, level = "INFO") {
    # First line gets full date, subsequent lines get time only
    is_first <- length(rv$ui_log) == 0
    if (is_first) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    } else {
      timestamp <- format(Sys.time(), "%H:%M:%S")
    }
    line <- sprintf("[%s] %s", timestamp, msg)
    rv$ui_log <- c(rv$ui_log, line)
  }

  # Helper to write progress.json (used by background process)
  write_progress <- function(progress_path, status, message, pct = NULL) {
    obj <- list(
      status = status,
      message = message,
      pct = pct,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    jsonlite::write_json(obj, progress_path, auto_unbox = TRUE, pretty = TRUE)
  }

  terpbase_path_rx <- reactive({
    if (identical(input$nr_terpbase_mode, "upload")) {
      nr_fileinput_path(input$nr_terpbase_upload)
    } else {
      input$nr_terpbase_default_path %||% ""
    }
  })

  formatted_path_rx <- reactive(nr_fileinput_path(input$nr_formatted_upload))
  terpflow_path_rx  <- reactive({
    nr_fileinput_path(input$nr_terpflow_upload)
  })

  # Capture original file names when files are uploaded
  observeEvent(input$nr_formatted_upload, {
    if (!is.null(input$nr_formatted_upload$name)) {
      rv$formatted_filename <- input$nr_formatted_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_terpflow_upload, {
    if (!is.null(input$nr_terpflow_upload$name)) {
      rv$terpflow_filename <- input$nr_terpflow_upload$name
    }
  }, ignoreInit = TRUE)

  observeEvent(input$nr_terpbase_upload, {
    if (!is.null(input$nr_terpbase_upload$name)) {
      rv$terpbase_filename <- input$nr_terpbase_upload$name
    }
  }, ignoreInit = TRUE)

  v_terpbase <- reactive(nr_ui_validate_terpbase(terpbase_path_rx()))
  v_formatted <- reactive(nr_ui_validate_formatted(formatted_path_rx()))
  v_terpflow <- reactive(nr_ui_validate_terpflow(terpflow_path_rx()))

  all_ok <- reactive({
    isTRUE(v_terpbase()$ok) &&
    isTRUE(v_formatted()$ok) &&
    isTRUE(v_terpflow()$ok)
  })

  nr_missing_items <- reactive({
    missing <- character(0)
    if (!isTRUE(v_terpbase()$ok)) missing <- c(missing, "Terpbase")
    if (!isTRUE(v_formatted()$ok)) missing <- c(missing, "Formatted data")
    if (!isTRUE(v_terpflow()$ok)) missing <- c(missing, "Pipeline")
    missing
  })

  light_ui <- function(v, label) {
    dot <- if (!isTRUE(v$ok)) "red" else "green"
    div(
      class = "nr-row",
      div(class = "nr-light", span(class = paste("nr-dot", dot)), label),
      div(class = "nr-msg", v$msg %||% "")
    )
  }

  output$nr_light_terpbase <- renderUI(light_ui(v_terpbase(), "Terpbase"))
  output$nr_light_formatted <- renderUI(light_ui(v_formatted(), "Formatted"))
  output$nr_light_terpflow <- renderUI(light_ui(v_terpflow(), "Pipeline"))
  nr_gate_panel_ui <- function() {
    div(
      class = "msterp-gate",
      div(
        class = "msterp-gate-card",
        div(
          class = "msterp-gate-body",
          div(class = "msterp-gate-title", "New Run"),
          div(class = "nr-section",
              strong("1) Terpbase"),
              radioButtons(
                "nr_terpbase_mode",
                label = NULL,
                choices = c("Use default" = "default", "Upload" = "upload"),
                selected = "default",
                inline = TRUE
              ),
              conditionalPanel(
                "input.nr_terpbase_mode == 'default'",
                textInput(
                  "nr_terpbase_default_path",
                  label = NULL,
                  # Use app-relative path that works on Shiny Server (not Windows-specific Desktop path)
                  value = "terpbase/Jan2026UniprotHuman.terpbase"
                )
              ),
              conditionalPanel(
                "input.nr_terpbase_mode == 'upload'",
                fileInput("nr_terpbase_upload", label = NULL, accept = c(".terpbase", ".rds"))
              ),
              uiOutput("nr_light_terpbase")
          ),

          div(class = "nr-section",
              strong("2) Formatted data (.xlsx)"),
              fileInput("nr_formatted_upload", label = NULL, accept = c(".xlsx", ".xls")),
              uiOutput("nr_light_formatted")
          ),

          div(class = "nr-section",
              strong("3) Pipeline (.terpflow)"),
              fileInput("nr_terpflow_upload", label = NULL, accept = c(".terpflow", ".rds")),
              uiOutput("nr_light_terpflow")
          ),

          div(class = "nr-section",
              strong("4) Run Name (optional)"),
              textInput("nr_run_name", label = NULL, placeholder = "e.g., my_analysis", width = "70%")
          ),

          div(class = "nr-section", style = "text-align: center;",
              uiOutput("nr_run_btn"),
              uiOutput("nr_run_status_ui")
          )
        )
      )
    )
  }

  output$nr_run_btn <- renderUI({
    if (isTRUE(all_ok())) {
      actionButton("nr_run", "Run Pipeline", class = "btn btn-primary btn-lg")
    } else {
      tags$button(
        type = "button",
        class = "btn btn-primary btn-lg",
        disabled = "disabled",
        "Run Pipeline"
      )
    }
  })

  nr_running_panel_ui <- function() {
    pill_class <- if (rv$last_status == "running") "nr-status-pill running" else "nr-status-pill"

    div(
      class = "nr-running-panel",
      div(class = "nr-row",
          div(class = pill_class, textOutput("nr_status_text")),
          div(class = "nr-elapsed", uiOutput("nr_elapsed_time")),
          div(class = "nr-msg", textOutput("nr_status_msg"))
      ),
      div(class = "nr-progress-wrap",
          div(class = "nr-progress-bar", uiOutput("nr_progress_fill"))
      ),
      # Stop button - only show while running
      if (rv$last_status == "running") {
        div(class = "nr-actions", style = "margin-top: 10px;",
            actionButton("nr_stop_run", "Stop Run", class = "btn btn-danger btn-sm")
        )
      },
      tags$hr(),
      div(strong(""), tags$br(), div(class = "nr-log", uiOutput("nr_log_tail"))),

      # Actions section - show after run completes
      if (rv$last_status %in% c("done", "error")) {
        div(class = "nr-actions",
            uiOutput("nr_download_ui"),
            actionButton("nr_new_run", "Start New Run", class = "btn btn-default")
        )
      }
    )
  }

  output$nr_main_panel <- renderUI({
    if (rv$last_status %in% c("running", "done", "error")) {
      nr_running_panel_ui()
    } else {
      nr_gate_panel_ui()
    }
  })

  output$nr_run_status_ui <- renderUI({
    if (!isTRUE(all_ok())) {
      return(div(class = "nr-msg", style = "color: #d11; margin-top: 6px;",
                 "Needs valid inputs above"))
    }
    # Show if a run is in progress
    if (identical(rv$last_status, "running")) {
      return(div(class = "nr-msg", style = "color: #1a8f3a; margin-top: 6px;",
                 "Pipeline running in background..."))
    }
    NULL
  })

  observeEvent(input$nr_run, {
    # Clear previous log
    rv$ui_log <- character(0)

    ui_log("Initiating run...")

    # Prevent multiple concurrent runs
    if (identical(rv$last_status, "running")) {
      ui_log("A run is already in progress", "WARN")
      return()
    }

    ui_log("Validating inputs...")

    if (!isTRUE(all_ok())) {
      ui_log("Validation failed", "ERROR")
      if (!isTRUE(v_terpbase()$ok)) ui_log("  - Terpbase: invalid or missing", "ERROR")
      if (!isTRUE(v_formatted()$ok)) ui_log("  - Formatted data: invalid or missing", "ERROR")
      if (!isTRUE(v_terpflow()$ok)) ui_log("  - Pipeline: invalid or missing", "ERROR")
      return()
    }

    ui_log("Validation passed")

    # Check if callr is available for async execution
    has_callr <- requireNamespace("callr", quietly = TRUE)
    if (!has_callr) {
      ui_log("Note: callr not available, running synchronously", "WARN")
    }

    rv$last_err <- NULL
    rv$last_status <- "running"
    rv$last_msg <- "Initiating run..."
    rv$run_start_time <- Sys.time()

    # Use temp directory for output (works on both local and server deployments)
    out_parent <- tempdir()
    dir.create(out_parent, recursive = TRUE, showWarnings = FALSE)

    # Get input paths (normalize to absolute paths for subprocess)
    tb <- terpbase_path_rx()
    ff <- formatted_path_rx()
    tf <- terpflow_path_rx()

    tb_norm <- if (!is.null(tb) && nzchar(tb)) nr_norm(tb) else NULL
    ff_norm <- nr_norm(ff)
    tf_norm <- nr_norm(tf)
    out_norm <- nr_norm(out_parent)

    # Create a unique run directory with progress.json upfront
    # Use user-provided run name if given, otherwise auto-generate
    user_run_name <- trimws(input$nr_run_name %||% "")
    if (nzchar(user_run_name)) {
      run_name <- paste0(nr_sanitize(user_run_name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    } else {
      run_name <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    run_root <- file.path(out_norm, paste0(run_name, ".terpbook"))
    dir.create(run_root, recursive = TRUE, showWarnings = FALSE)

    # Set up paths for polling
    rv$run_root <- run_root
    rv$progress_path <- file.path(run_root, "progress.json")
    rv$log_path <- file.path(run_root, "log.txt")
    rv$manifest_path <- file.path(run_root, "manifest.json")

    # Write initial progress
    write_progress(rv$progress_path, "running", "Initializing...", 5)

    ui_log("Loading input files...")
    # Use original file names if available, otherwise fall back to temp path basename
    formatted_display <- rv$formatted_filename %||% basename(ff_norm)
    terpflow_display <- rv$terpflow_filename %||% basename(tf_norm)
    terpbase_display <- rv$terpbase_filename %||% (if (!is.null(tb_norm)) basename(tb_norm) else NULL)

    ui_log(sprintf("  Formatted: %s", formatted_display))
    ui_log(sprintf("  Pipeline:  %s", terpflow_display))
    if (!is.null(tb_norm)) {
      # For default terpbase path, show the actual path
      if (identical(input$nr_terpbase_mode, "default")) {
        ui_log(sprintf("  Terpbase:  %s", basename(tb_norm)))
      } else {
        ui_log(sprintf("  Terpbase:  %s", terpbase_display %||% basename(tb_norm)))
      }
    }
    ui_log(sprintf("  Output:    %s", out_norm))

    # Get app root directory for sourcing in subprocess
    app_root <- getwd()

    if (has_callr) {
      # =========================================================
      # ASYNC EXECUTION via callr::r_bg()
      # =========================================================
      ui_log("Launching background process...")

      # The runner function executes in a separate R process
      # It sources the app files and runs nr_execute_run with a file-based progress callback
      runner_fn <- function(app_root, formatted_path, terpflow_path, terpbase_path,
                            out_dir, run_name, progress_path, log_path) {
        # Set working directory
        setwd(app_root)

        # Helper to write progress
        write_prog <- function(status, message, pct = NULL) {
          obj <- list(
            status = status,
            message = message,
            pct = pct,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
          jsonlite::write_json(obj, progress_path, auto_unbox = TRUE, pretty = TRUE)
        }

        # Helper to append to log
        write_log <- function(msg, level = "INFO") {
          # First line gets full date, subsequent lines get time only
          is_first <- !file.exists(log_path) || file.info(log_path)$size == 0
          if (is_first) {
            time_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          } else {
            time_str <- format(Sys.time(), "%H:%M:%S")
          }
          line <- sprintf("[%s] %s\n", time_str, msg)
          cat(line, file = log_path, append = TRUE)
        }

        write_prog("running", "Loading engine code...", 8)
        write_log("Loading engine code...")

        # Source all required files
        tryCatch({
          source(file.path(app_root, "R", "00_init.R"), local = FALSE)

          # Source engines directory
          engine_files <- list.files(file.path(app_root, "R", "engines"),
                                      pattern = "\\.R$", full.names = TRUE)
          for (f in engine_files) source(f, local = FALSE)

          # Source stats engines
          stats_files <- list.files(file.path(app_root, "R", "engines", "stats"),
                                     pattern = "\\.R$", full.names = TRUE)
          for (f in stats_files) source(f, local = FALSE)

          # Source utils
          utils_files <- list.files(file.path(app_root, "R", "utils"),
                                     pattern = "\\.R$", full.names = TRUE)
          for (f in utils_files) source(f, local = FALSE)

          write_log("Engine code loaded")
        }, error = function(e) {
          write_prog("error", paste("Failed to load engine code:", conditionMessage(e)), 0)
          write_log(paste("ERROR loading code:", conditionMessage(e)), "ERROR")
          stop(e)
        })

        # Create progress callback that writes to file
        # This callback receives messages from nr_execute_run
        progress_callback <- function(status, msg, pct) {
          write_prog("running", msg, pct)
          write_log(msg)
        }

        # Run the pipeline
        result <- tryCatch({
          nr_execute_run(
            formatted_path = formatted_path,
            terpflow_path = terpflow_path,
            terpbase_path = terpbase_path,
            out_dir = out_dir,
            run_name = run_name,
            progress_callback = progress_callback
          )
        }, error = function(e) {
          write_prog("error", paste("Pipeline failed:", conditionMessage(e)), 0)
          write_log(paste("ERROR:", conditionMessage(e)), "ERROR")
          list(ok = FALSE, error = conditionMessage(e))
        })

        if (isTRUE(result$ok)) {
          write_prog("done", "Pipeline complete!", 100)
          write_log("Pipeline completed successfully")
        } else {
          write_prog("error", result$error %||% "Unknown error", 0)
          write_log(paste("Pipeline failed:", result$error %||% "Unknown error"), "ERROR")
        }

        result
      }

      # Launch background process
      rv$bg_process <- callr::r_bg(
        func = runner_fn,
        args = list(
          app_root = app_root,
          formatted_path = ff_norm,
          terpflow_path = tf_norm,
          terpbase_path = tb_norm,
          out_dir = out_norm,
          run_name = run_name,
          progress_path = rv$progress_path,
          log_path = rv$log_path
        ),
        package = TRUE,
        supervise = TRUE
      )

      ui_log("Background process launched")
      # Return immediately - polling will handle updates

    } else {
      # =========================================================
      # SYNCHRONOUS FALLBACK (no callr)
      # =========================================================
      ui_log("Running synchronously...")
      set_busy(TRUE, "Running pipeline...", 5)
      on.exit(set_busy(FALSE, "", NULL), add = TRUE)

      # Progress callback writes to file for consistency
      progress_callback <- function(status, msg, pct) {
        write_progress(rv$progress_path, status, msg, pct)
        set_busy(TRUE, msg, pct)
        ui_log(sprintf("[%s] %s", status, msg))
      }

      res <- tryCatch({
        nr_execute_run(
          formatted_path = ff_norm,
          terpflow_path = tf_norm,
          terpbase_path = tb_norm,
          out_dir = out_norm,
          run_name = run_name,
          progress_callback = progress_callback
        )
      }, error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      })

      if (inherits(res, "error") || !isTRUE(res$ok)) {
        err_msg <- if (inherits(res, "error")) conditionMessage(res) else res$error %||% "Unknown error"
        ui_log(sprintf("Run failed: %s", err_msg), "ERROR")
        rv$last_status <- "error"
        rv$last_err <- err_msg
        rv$last_msg <- "Run failed."
        write_progress(rv$progress_path, "error", err_msg, 0)
        return()
      }

      rv$run_root <- nr_norm(res$run_root %||% run_root)
      rv$manifest_path <- nr_norm(res$manifest_path %||% file.path(rv$run_root, "manifest.json"))
      rv$log_path <- nr_norm(res$log_path %||% file.path(rv$run_root, "log.txt"))

      rv$last_status <- "done"
      rv$last_msg <- "Run complete!"
      ui_log(sprintf("Run complete! Output: %s", rv$run_root))
    }
  }, ignoreInit = TRUE)

  # =========================================================
  # Polling observer for background process completion
  # =========================================================
  observe({
    # Only poll if we have a running background process
    if (is.null(rv$bg_process) || !identical(rv$last_status, "running")) {
      return()
    }

    # Check process status
    if (!rv$bg_process$is_alive()) {
      # Process finished - get result
      result <- tryCatch({
        rv$bg_process$get_result()
      }, error = function(e) {
        list(ok = FALSE, error = conditionMessage(e))
      })

      if (isTRUE(result$ok)) {
        rv$last_status <- "done"
        rv$last_msg <- "Run complete!"
        ui_log(sprintf("Background run complete! Output: %s", rv$run_root))

        # Update paths from result if available
        if (!is.null(result$run_root)) {
          rv$run_root <- nr_norm(result$run_root)
          rv$manifest_path <- file.path(rv$run_root, "manifest.json")
          rv$log_path <- file.path(rv$run_root, "log.txt")
        }
      } else {
        rv$last_status <- "error"
        rv$last_err <- result$error %||% "Unknown error"
        rv$last_msg <- "Run failed."
        ui_log(sprintf("Background run failed: %s", rv$last_err), "ERROR")
      }

      rv$bg_process <- NULL
    } else {
      # Still running - poll again
      invalidateLater(500, session)
    }
  })
  
  # Poll progress + status
  output$nr_progress_json <- renderText({
    if (is.null(rv$run_root)) return("")
    if (!file.exists(rv$progress_path %||% "")) {
      invalidateLater(400, session)
      return("{ \"status\": \"starting\" }")
    }
    
    invalidateLater(400, session)
    txt <- paste(readLines(rv$progress_path, warn = FALSE), collapse = "\n")
    if (nzchar(txt)) txt else "{ }"
  })
  
  progress_obj_rx <- reactive({
    # Poll while running
    if (identical(rv$last_status, "running")) {
      invalidateLater(500, session)
    }

    if (is.null(rv$run_root) || is.null(rv$progress_path) || !file.exists(rv$progress_path)) return(NULL)
    if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
    tryCatch(jsonlite::read_json(rv$progress_path, simplifyVector = TRUE), error = function(e) NULL)
  })
  
  output$nr_status_text <- renderText({
    # First check rv$last_status for immediate feedback
    status <- rv$last_status %||% "idle"

    # Override with progress.json status if available
    p <- progress_obj_rx()
    if (!is.null(p) && !is.null(p$status)) {
      status <- as.character(p$status)
    }

    # Capitalize for display
    tools::toTitleCase(status)
  })
  
  output$nr_status_msg <- renderText({
    p <- progress_obj_rx()
    if (is.null(rv$run_root)) return("")
    if (is.null(p)) return(rv$last_msg %||% "")
    as.character(p$message %||% "")
  })
  
  output$nr_progress_fill <- renderUI({
    p <- progress_obj_rx()
    pct <- suppressWarnings(as.numeric(p$pct %||% 0))
    if (is.na(pct)) pct <- 0
    pct <- max(0, min(100, pct))
    # Add "active" class for animated stripes when running
    fill_class <- if (identical(rv$last_status, "running")) "nr-progress-fill active" else "nr-progress-fill"
    div(class = fill_class, style = paste0("width:", pct, "%;"))
  })

  # Elapsed time display
  output$nr_elapsed_time <- renderUI({
    # Poll while running to update timer
    if (identical(rv$last_status, "running")) {
      invalidateLater(1000, session)
    }

    if (is.null(rv$run_start_time)) return(NULL)

    elapsed <- as.numeric(difftime(Sys.time(), rv$run_start_time, units = "secs"))
    mins <- floor(elapsed / 60)
    secs <- floor(elapsed %% 60)
    sprintf("%d:%02d", mins, secs)
  })
  
  output$nr_log_tail <- renderUI({
    # Poll while running
    if (identical(rv$last_status, "running")) {
      invalidateLater(500, session)
    }

    # Show UI log first, then append file log if available
    ui_lines <- rv$ui_log
    file_lines <- character(0)

    if (!is.null(rv$run_root) && !is.null(rv$log_path) && file.exists(rv$log_path)) {
      file_lines <- readLines(rv$log_path, warn = FALSE, encoding = "UTF-8")
    }

    all_lines <- c(ui_lines, file_lines)
    if (length(all_lines) == 0) return(tags$span(style = "color: #5b5b5b;", "(waiting for run...)"))

    # Take last 200 lines
    all_lines <- utils::tail(all_lines, 200)

    # Format each line with styled spans
    # Pattern: [timestamp] [engine_id] message  OR  [timestamp] message
    format_log_line <- function(line) {
      # Escape HTML entities
      line <- gsub("&", "&amp;", line, fixed = TRUE)
      line <- gsub("<", "&lt;", line, fixed = TRUE)
      line <- gsub(">", "&gt;", line, fixed = TRUE)

      # Match patterns like [timestamp] or [engine_id]
      # Step labels like "=== Step N: engine ===" get bold treatment
      if (grepl("^\\[", line)) {
        # Extract timestamp (first bracketed item) - lighter gray for dark bg
        line <- gsub("^(\\[[^\\]]+\\])", '<span style="color:#888;">\\1</span>', line)
        # Style additional bracketed items (engine_id) - slightly brighter
        line <- gsub('(<\\/span> )(\\[[^\\]]+\\])', '\\1<span style="color:#aaa;">\\2</span>', line)
      }

      # Bold step/phase labels (=== Step N: xxx ===) - bright for emphasis
      if (grepl("===", line)) {
        line <- gsub("(=== .+ ===)", '<span style="font-weight:700;color:#f0f0f0;">\\1</span>', line)
      }

      line
    }

    formatted_lines <- vapply(all_lines, format_log_line, character(1), USE.NAMES = FALSE)
    HTML(paste(formatted_lines, collapse = "<br/>"))
  })
  
  output$nr_download_ui <- renderUI({
    # Check if run completed (either via progress.json or rv$last_status)
    p <- progress_obj_rx()
    done_progress <- !is.null(p) && identical(as.character(p$status %||% ""), "done")
    done_status <- identical(rv$last_status, "done")
    if ((!done_progress && !done_status) || is.null(rv$run_root)) return(NULL)
    downloadButton("nr_download_terpbook", "Download .terpbook", class = "btn btn-default")
  })
  
  output$nr_download_terpbook <- downloadHandler(
    filename = function() {
      nm <- "terpbook"
      if (!is.null(rv$manifest_path) && file.exists(rv$manifest_path) && requireNamespace("jsonlite", quietly = TRUE)) {
        man <- tryCatch(jsonlite::read_json(rv$manifest_path, simplifyVector = TRUE), error = function(e) NULL)
        nm <- nr_sanitize((man$pipeline$pipeline_name %||% man$pipeline_name %||% "terpbook"))
      }
      paste0(nm, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".terpbook")
    },
    content = function(file) {
      run_root <- rv$run_root
      if (is.null(run_root) || !dir.exists(run_root)) stop("run_root missing")
      
      # Zip entire run folder into a single file (works on Windows via utils::zip)
      old <- setwd(dirname(run_root))
      on.exit(setwd(old), add = TRUE)
      
      base <- basename(run_root)
      
      if (requireNamespace("zip", quietly = TRUE)) {
        zip::zipr(zipfile = file, files = base)
      } else {
        # utils::zip requires 'zip' on PATH on some systems; Windows usually OK
        utils::zip(zipfile = file, files = base, flags = "-r9X")
      }
    }
  )
  
  # Stop running process
  observeEvent(input$nr_stop_run, {
    if (!is.null(rv$bg_process) && rv$bg_process$is_alive()) {
      ui_log("Stopping run...")
      tryCatch({
        rv$bg_process$kill()
      }, error = function(e) {
        ui_log(sprintf("Error stopping process: %s", conditionMessage(e)), "WARN")
      })
      rv$bg_process <- NULL
      rv$last_status <- "error"
      rv$last_msg <- "Run stopped by user"
      rv$last_err <- "Stopped by user"
      ui_log("Run stopped by user")

      # Update progress.json to reflect stopped state
      if (!is.null(rv$progress_path) && file.exists(dirname(rv$progress_path))) {
        write_progress(rv$progress_path, "error", "Stopped by user", 0)
      }
    }
  }, ignoreInit = TRUE)

  # Reset to start a new run
  observeEvent(input$nr_new_run, {
    # Reset all reactive state
    rv$last_status <- "idle"
    rv$last_msg <- ""
    rv$run_root <- NULL
    rv$progress_path <- NULL
    rv$log_path <- NULL
    rv$manifest_path <- NULL
    rv$ui_log <- character(0)
    rv$bg_process <- NULL
    rv$run_start_time <- NULL
    rv$formatted_filename <- NULL
    rv$terpflow_filename <- NULL
    rv$terpbase_filename <- NULL

    # Reset run name input
    updateTextInput(session, "nr_run_name", value = "")

    # Reset file inputs after the gate panel is re-rendered
    session$onFlushed(function() {
      reset_file_input("nr_formatted_upload")
      reset_file_input("nr_terpflow_upload")
      reset_file_input("nr_terpbase_upload")
    }, once = TRUE)
  }, ignoreInit = TRUE)
}

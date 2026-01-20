# R/pages/tools/tool_2dgofcs.R
# 2D GO-FCS Tool - 2D Functional Class Scoring

tools_2dgofcs_defaults <- function() {
  eng <- msterp_engine_get("2dgofcs")
  list(
    params = msterp_schema_defaults(eng$params_schema %||% list()),
    style = msterp_schema_defaults(eng$style_schema %||% list())
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
        selectInput(
          "tools_2dgofcs_terpbase_default_path",
          "Default TerpBase",
          choices = tools_default_terpbase_choices(),
          selected = ""
        ),
        actionButton(
          "tools_2dgofcs_terpbase_default_load",
          "Load default",
          class = "btn btn-default btn-sm btn-tool-action"
        ),
        fileInput(
          "tools_2dgofcs_terpbase_file",
          "Load TerpBase (.terpbase)",
          accept = c(".terpbase", ".rds"),
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
        actionButton("tools_2dgofcs_run", "Run 2D GO-FCS", class = "btn-primary btn-tool-action"),
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
# 2D GO-FCS Server Logic
# ============================================================
tools_2dgofcs_server <- function(input, output, session, app_state, rv_2dgofcs, defs_2dgofcs) {
  safe_num <- function(x, default) {
    v <- suppressWarnings(as.numeric(x))
    if (!is.finite(v)) default else v
  }

  safe_int <- function(x, default) {
    v <- suppressWarnings(as.integer(x))
    if (!is.finite(v) || v <= 0) default else v
  }

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

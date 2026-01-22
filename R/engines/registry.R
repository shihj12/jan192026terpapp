# =========================================================
# engines/registry.R Engine Registry (single source of truth)
#
# Drives:
#  - pipeline editor UI (params + style controls)
#  - validation (requirements; sequential capability)
#  - results rendering expectations (outputs contract)
#
# Engine metadata flags (registry-level, not params/style):
#  - picker_hidden: hide engine from step/substep pickers (system/internal engines)
#  - locked_parent: for container engines, prevents clicking/navigating to the parent node in Results Viewer
#
# =========================================================
# THREE-SCHEMA CONTRACT
# =========================================================
#
# params_schema (COMPUTE-TIME - Pipeline only):
#   - Fields that affect computed output data
#   - Require re-running the pipeline to change
#   - Shown in Pipeline editor (basic section)
#   - NOT shown in Results Viewer
#   - Examples:
#     - Statistical mode (t-test vs ANOVA)
#     - Log transform choice
#     - Significance thresholds that define downstream gene lists
#     - FDR cutoffs that filter terms
#     - Compare mode (avg_groups vs within_groups)
#   - CRITICAL: Any threshold that determines child view content (e.g., GO gene lists)
#     MUST be in params_schema
#
# style_schema (DEFAULT STYLE - Both Pipeline and Results Viewer):
#   - Fields that affect aesthetics/presentation
#   - Can be changed in Results Viewer without re-running
#   - Shown in Pipeline editor under "Show more options" (as defaults)
#   - Shown in Results Viewer for live adjustment
#   - Stored in render_state.json overrides
#   - Examples:
#     - Colors (point, line, fill)
#     - Point/line sizes
#     - Opacity/alpha values
#     - Font sizes
#     - Plot dimensions (width/height)
#     - Show/hide toggles for annotations (labels, legends)
#
# viewer_schema (VIEWER-ONLY - Results Viewer only):
#   - Fields that only make sense in the Results Viewer context
#   - NOT shown in Pipeline editor
#   - Examples:
#     - Group selection dropdowns (populated from results data)
#     - Ontology filters (BP/MF/CC selection)
#     - Other runtime-only controls
#
# hidden = TRUE on any field: Not shown anywhere (internal use)
#
# =========================================================
# Alignment:
#  - Per-engine "Options" are style_schema keys
#  - mean_type for hor_dis + vert_dis moved to params_schema
#  - conditional UI relies on specific key names (color_mode, *_range_mode, show_ellipse, etc.)
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Session-level registry cache (avoids rebuilding 1800+ line registry on every call)
.registry_cache <- NULL

msterp_schema_field <- function(
    name,
    type = c("choice", "bool", "int", "num", "string", "range"),
    label = name,
    default = NULL,
    choices = NULL,
    choice_labels = NULL,
    min = NULL,
    max = NULL,
    advanced = FALSE,
    hidden = FALSE,
    help = NULL,
    viewer_only = FALSE  # DEPRECATED: use viewer_schema instead; kept for backward compatibility
) {
  type <- match.arg(type)
  list(
    name = name,
    type = type,
    label = label,
    default = default,
    choices = choices,
    choice_labels = choice_labels,
    min = min,
    max = max,
    advanced = isTRUE(advanced),
    hidden = isTRUE(hidden),
    help = help,
    viewer_only = isTRUE(viewer_only)  # DEPRECATED: kept for backward compatibility
  )
}

msterp_engine_registry <- function(force_rebuild = FALSE) {
  if (!is.null(.registry_cache) && !isTRUE(force_rebuild)) {
    return(.registry_cache)
  }

  registry_version <- 2L
  
  mk_style <- function(width = 7, height = 6, axis_text_size = 20) {
    list(
      msterp_schema_field(
        "axis_style", "choice", "Axis style",
        default = "clean", choices = c("clean", "bold"),
        advanced = TRUE  # Per-engine style control (no longer global toggle)
      ),
      msterp_schema_field(
        "axis_text_size", "int", "Axis text size",
        default = axis_text_size, min = 6, max = 40, advanced = TRUE
      ),
      msterp_schema_field(
        "width", "num", "Plot width (in)",
        default = width, min = 2, max = 24, advanced = TRUE
      ),
      msterp_schema_field(
        "height", "num", "Plot height (in)",
        default = height, min = 2, max = 24, advanced = TRUE
      )
    )
  }

  common_style <- mk_style(width = 7, height = 6, axis_text_size = 20)
  
  engines <- list(
    dataprocessor = list(
      engine_id = "dataprocessor",
      label = "Data Processor",
      category = "processing",
      description = "Preprocessing utilities (filters, aggregation, contaminants, imputation).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "operation", "choice", "Operation",
          default = "filter_non_numeric",
          choices = c(
            "filter_threshold", "filter_prefix", "filter_keyword",
            "filter_non_numeric", "aggregate_rows", "average_rows",
            "tag_remove_contaminants", "impute"
          ),
          advanced = TRUE
        ),
        msterp_schema_field("plan_json", "string", "Plan (JSON)", default = "", advanced = TRUE)
      ),
      style_schema = list(),
      outputs = list(figures = c(), tables = c("dataprocessor_log"), interactive = FALSE),
      render_spec = list(plots = character(0), tables = c("dataprocessor_log"), tabs = NULL)
    ),

    peptide_analysis = list(
      engine_id = "peptide_analysis",
      type = "container",
      label = "Peptide Analysis",
      category = "processing",
      description = "Container for peptide-level steps; auto-aggregates to protein-level before continuing.",
      supports_sequential = FALSE,
      accepted_input_levels = c("peptide"),
      locked_parent = TRUE,
      allowed_child_engines = c(
        "dataprocessor",
        "idquant_id_quant",
        "idquant_average_value",
        "idquant_cv_bar",
        "idquant_overlap",
        "idquant_overlap_detected",
        "idquant_overlap_quantified"
      ),
      # Auto-appended, non-removable final substep within this container:
      forced_final_substep_engine_id = "peptide_aggregate_to_protein",
      exit_level = "protein",
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("peptide")
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    peptide_aggregate_to_protein = list(
      engine_id = "peptide_aggregate_to_protein",
      label = "Aggregate Peptides → Proteins",
      category = "processing",
      description = "Aggregates peptide-level data into protein-level by combining peptide values per protein ID.",
      supports_sequential = FALSE,
      accepted_input_levels = c("peptide"),
      output_level = "protein",
      # Always used as a forced container substep; hide from independent Add Steps
      picker_hidden = TRUE,
      # Keep visible in Results Viewer when system-generated (shows before/after table)
      results_hidden_system_generated = FALSE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "method", "choice", "Aggregation method",
          default = "sum",
          choices = c("sum", "arithmetic_mean", "harmonic_mean"),
          choice_labels = c("Sum", "Arithmetic Mean", "Harmonic Mean")
        )
      ),
      style_schema = list(),
      outputs = list(figures = c(), tables = c("aggregation_log"), interactive = FALSE),
      render_spec = list(plots = character(0), tables = c("aggregation_log"), tabs = NULL)
    ),

    # ----------------------------
    # Metabolite Analysis Container
    # ----------------------------
    metabolite_analysis = list(
      engine_id = "metabolite_analysis",
      type = "container",
      label = "Metabolite Analysis",
      category = "processing",
      description = "Container for metabolite-level analysis steps. Unlike peptides, metabolites do not aggregate.",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      locked_parent = TRUE,
      allowed_child_engines = c(
        "dataprocessor",
        "idquant_id_quant",
        "idquant_average_value",
        "idquant_cv_scatter",
        "idquant_cv_bar",
        "idquant_overlap",
        "idquant_overlap_detected",
        "idquant_overlap_quantified"
      ),
      # NO forced_final_substep - metabolites don't aggregate like peptides
      exit_level = "metabolite",
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        requires_metabobase = FALSE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    # ----------------------------
    # QC
    # ----------------------------
    idquant = list(
      engine_id = "idquant",
      type = "container",
      label = "Quantitative QC",
      category = "qc",
      description = "Container for ID quantification summary views (protein, peptide, or metabolite).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      locked_parent = TRUE,
      allowed_child_engines = c(
        "idquant_id_quant",
        "idquant_average_value",
        "idquant_cv_scatter",
        "idquant_cv_bar",
        "idquant_overlap",
        "idquant_overlap_detected",
        "idquant_overlap_quantified"
      ),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = list(),
      outputs = list(figures = c(), tables = c(), interactive = FALSE),
      render_spec = list(plots = character(0), tables = character(0), tabs = NULL)
    ),

    # IDQuant container child engines (substeps)
    idquant_id_quant = list(
      engine_id = "idquant_id_quant",
      label = "ID & Quantification",
      category = "qc",
      description = "Identification/quantification summary view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "acquisition_mode", "choice", "Acquisition",
            default = "dia", choices = c("dda", "dia"),
            choice_labels = c("DDA", "DIA"),
            help = "Controls labeling for identification/quantification."
          ),
          msterp_schema_field(
            "y_limit_mode", "choice", "Y-limit mode",
            default = "auto", choices = c("auto", "manual"), advanced = TRUE
          ),
          msterp_schema_field("ymax_protein", "int", "Y max (protein)", default = 10000, min = 1, advanced = TRUE),
          msterp_schema_field(
            "show_values", "bool", "Show values",
            default = TRUE,
            help = "Display integer counts above bars."
          ),
          msterp_schema_field(
            "value_label_size", "num", "Value label size",
            default = 4, min = 1, max = 12,
            help = "Text size for value labels.",
            advanced = TRUE
          ),
          msterp_schema_field("color_quantified", "string", "Quantified color", default = "#1f77b4", advanced = TRUE),
          msterp_schema_field("color_identified", "string", "Identified color", default = "#ff7f0e", advanced = TRUE),
          msterp_schema_field("show_bar_outline", "bool", "Show bar outline", default = FALSE, advanced = TRUE),
          msterp_schema_field("bar_outline_color", "string", "Outline color", default = "#000000", advanced = TRUE),
          msterp_schema_field("bar_outline_width", "num", "Outline width", default = 0.5, min = 0, max = 5, advanced = TRUE)
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_group", "idquant_replicate"), tables = c("idquant_group", "idquant_replicate"), interactive = TRUE),
      render_spec = list(plots = c("idquant_group", "idquant_replicate"), tables = c("idquant_group", "idquant_replicate"), tabs = NULL)
    ),

    idquant_average_value = list(
      engine_id = "idquant_average_value",
      label = "Average Intensity",
      category = "qc",
      description = "Average value summary view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to values for plots/tables."
          ),
          msterp_schema_field(
            "bar_color_mode", "choice", "Bar color mode",
            default = "group", choices = c("group", "flat"),
            help = "Color bars by group, or use a single flat color.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_color", "string", "Bar color",
            default = "#1f77b4",
            help = "Flat bar color (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = TRUE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_width", "num", "Bar outline width",
            default = 0.5, min = 0, max = 5,
            help = "Outline width for bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_values", "bool", "Show values",
            default = TRUE,
            help = "Display values above bars."
          ),
          msterp_schema_field(
            "value_label_size", "num", "Value label size",
            default = 5, min = 1, max = 12,
            help = "Text size for value labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "y_axis_title", "string", "Y-axis title",
            default = "Intensity",
            help = "Custom label for Y-axis.",
            advanced = TRUE
          )
        ),
        mk_style(width = 4, height = 5, axis_text_size = 20)
      ),
      outputs = list(
        figures = c("idquant_average_value"),
        tables = c("idquant_average_value"),
        interactive = TRUE
      ),
      render_spec = list(
        plots = c("idquant_average_value"),
        tables = c("idquant_average_value"),
        tabs = NULL
      )
    ),

    # Legacy IDQuant child views (viewer-only; retained for backward compatibility)
    idquant_group = list(
      engine_id = "idquant_group",
      label = "IDQuant: Group",
      category = "qc",
      description = "IDQuant group-level summaries (legacy child view).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "show_count_labels", "bool", "Show count labels",
            default = FALSE,
            help = "Display count values above bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_label_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_group_plot"), tables = c("idquant_group_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_group_plot"), tables = c("idquant_group_table"), tabs = NULL)
    ),

    idquant_replicate = list(
      engine_id = "idquant_replicate",
      label = "IDQuant: Replicate",
      category = "qc",
      description = "IDQuant replicate-level summaries (legacy child view).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "show_count_labels", "bool", "Show count labels",
            default = FALSE,
            help = "Display count values above bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_label_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("idquant_replicate_plot"), tables = c("idquant_replicate_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_replicate_plot"), tables = c("idquant_replicate_table"), tabs = NULL)
    ),

    # IDQuant: CV scatter (split engine)
    idquant_cv_scatter = list(
      engine_id = "idquant_cv_scatter",
      label = "Coef. Of Variation (CV) (scatter)",
      category = "qc",
      description = "CV% scatter/table view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables."
          ),
          msterp_schema_field(
            "min_replicates", "int", "Minimum replicates",
            default = 2, min = 1, max = 10,
            help = "Minimum number of replicates required per group for CV calculation.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_axis_mode", "choice", "X-axis mode",
            default = "abundance", choices = c("abundance", "rank"),
            help = "X-axis shows (abundance) log-transformed abundance values, or (rank) abundance rank ordering.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_min", "num", "X min",
            default = 0,
            help = "Manual X-axis minimum (only used when set).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "x_max", "num", "X max",
            default = 12,
            help = "Manual X-axis maximum (only used when set).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_threshold", "num", "CV% threshold",
            default = 30, min = 0, max = 200,
            help = "Highlight/filter proteins with CV% above this threshold.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_show", "bool", "Show threshold line",
            default = FALSE,
            help = "Show the horizontal CV% threshold line on the scatter plot.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_labels", "bool", "Show labels",
            default = FALSE,
            help = "Show labels for highlighted proteins.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_color", "string", "Threshold line color",
            default = "gray60",
            help = "Color for the CV% threshold line.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "threshold_width", "num", "Threshold line width",
            default = 0.5, min = 0, max = 10,
            help = "Line width for the CV% threshold line.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "n_labels", "int", "# labels",
            default = 30, min = 0, max = 200,
            help = "Number of points to label (gene symbols when available).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_color_mode", "choice", "Point color mode",
            default = "group", choices = c("group", "flat"),
            help = "Color points by group, or use a single flat color.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "flat_color", "string", "Flat point color",
            default = "#808080",
            help = "Color for scatter points when point color mode is 'flat' (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_size", "num", "Point size",
            default = 1.5, min = 0.1, max = 10,
            help = "Point size.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "point_alpha", "num", "Point alpha",
            default = 0.7, min = 0, max = 1,
            help = "Point opacity (0=transparent, 1=opaque).",
            advanced = TRUE
          )
        ),
        mk_style(width = 6, height = 5, axis_text_size = 20),
        list(
          msterp_schema_field(
            "cv_plot_mode", "choice", "Plot mode",
            default = "scatter", choices = c("scatter"),
            help = "Scatter plot only (split engine).",
            hidden = TRUE
          )
        )
      ),
      outputs = list(figures = c("idquant_cv_plot"), tables = c("idquant_cv_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_cv_plot"), tables = character(0), tabs = NULL)
    ),

    # Legacy IDQuant CV (combined scatter+bar; retained for backward compatibility)
    # Peptide Analysis: IDQuant CV bar-only variant (system-generated substep)
    idquant_cv_bar = list(
      engine_id = "idquant_cv_bar",
      label = "Coef. Of Variation (CV) (bar)",
      category = "qc",
      description = "CV% bar chart view.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("peptide", "protein")
      ),
      params_schema = list(),
      style_schema = c(
        list(
          msterp_schema_field(
            "cv_threshold", "num", "CV% threshold",
            default = 30, min = 0, max = 200,
            help = "Highlight/filter proteins with CV% above this threshold.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "min_replicates", "int", "Minimum replicates",
            default = 2, min = 1, max = 10,
            help = "Minimum number of replicates required per group for CV calculation.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "num_bins", "int", "Number of bins",
            default = 4, min = 2, max = 6,
            help = "Number of CV% bins to display (2-6). Uses thresholds from the bin settings below.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_1", "num", "CV bin 1 threshold (%)",
            default = 10, min = 0, max = 500,
            help = "First CV% bin boundary (e.g., 0-10%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_1_color", "string", "CV bin 1 color",
            default = "#5C5C5C",
            help = "Color for the first CV% bin (0-X%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_2", "num", "CV bin 2 threshold (%)",
            default = 30, min = 0, max = 500,
            help = "Second CV% bin boundary (e.g., 10-20%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_2_color", "string", "CV bin 2 color",
            default = "#9C9C9C",
            help = "Color for the second CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_3", "num", "CV bin 3 threshold (%)",
            default = 100, min = 0, max = 500,
            help = "Third CV% bin boundary (e.g., 20-30%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_3_color", "string", "CV bin 3 color",
            default = "#C7C8C9",
            help = "Color for the third CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_4", "num", "CV bin 4 threshold (%)",
            default = 50, min = 0, max = 500,
            help = "Fourth CV% bin boundary (e.g., 30-50%).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_4_color", "string", "CV bin 4 color",
            default = "#FC0000",
            help = "Color for the fourth CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_5", "num", "CV bin 5 threshold (%)",
            default = 100, min = 0, max = 500,
            help = "Fifth CV% bin boundary (e.g., 50-100%). Values above this threshold go to 'X%+' bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_5_color", "string", "CV bin 5 color",
            default = "#EF8A62",
            help = "Color for the fifth CV% bin.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "cv_bin_6_color", "string", "CV bin 6 color",
            default = "#B2182B",
            help = "Color for the sixth CV% bin (highest CV%).",
            advanced = TRUE
          )
        ),
        mk_style(width = 6, height = 5, axis_text_size = 20),
        list(
          msterp_schema_field(
            "transform", "choice", "Transform",
            default = "log10", choices = c("none", "log2", "log10"),
            help = "Viewer-only transform applied to intensities for plots/tables.",
            hidden = TRUE
          )
        )
      ),
      outputs = list(figures = c("idquant_cv_plot"), tables = c("idquant_cv_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_cv_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap = list(
      engine_id = "idquant_overlap",
      label = "Overlaps",
      category = "qc",
      description = "Overlap view (UpSet).",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "detected", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = FALSE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 3.5, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#4245FF",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 0, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.4, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "upset", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap_detected = list(
      engine_id = "idquant_overlap_detected",
      label = "Overlaps (Identified)",
      category = "qc",
      description = "Overlap view using detected entries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "detected", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = TRUE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 4, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#9E9E9E",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 5, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.5, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 18)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "venn", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),

    idquant_overlap_quantified = list(
      engine_id = "idquant_overlap_quantified",
      label = "Overlaps (Quantified)",
      category = "qc",
      description = "Overlap view using reproducibly detected entries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "peptide", "metabolite"),
      picker_hidden = TRUE,
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein", "peptide")
      ),
      params_schema = list(
        msterp_schema_field(
          "overlap_metric", "choice", "Overlap metric",
          default = "quantified", choices = c("detected", "quantified"),
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "count_labels_show", "bool", "Show count labels",
            default = TRUE,
            help = "Show numeric count labels above intersection bars (UpSet).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_size", "num", "Count label size",
            default = 4, min = 1, max = 12,
            help = "Text size for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "count_labels_color", "string", "Count label color",
            default = "black",
            help = "Text color for count labels.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_bar_outline", "bool", "Show bar outline",
            default = FALSE,
            help = "Draw outline around bars.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_outline_color", "string", "Bar outline color",
            default = "#000000",
            help = "Color for bar outlines (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "bar_fill_color", "string", "Bar fill color",
            default = "#9E9E9E",
            help = "Fill color for intersection bars (hex).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_fill_alpha", "num", "Venn fill opacity",
            default = 0.4, min = 0, max = 1,
            help = "Opacity for Venn fills.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_show_percentage", "bool", "Show percentage",
            default = FALSE,
            help = "Show percentages inside the Venn diagram instead of counts.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_text_size", "num", "Venn value text size",
            default = 4, min = 1, max = 12,
            help = "Text size for values inside the Venn diagram.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_set_name_size", "num", "Venn set label size",
            default = 5, min = 0, max = 12,
            help = "Text size for set labels around the Venn diagram (0 hides labels).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_size", "num", "Venn outline size",
            default = 0.5, min = 0, max = 5,
            help = "Outline thickness for Venn circles.",
            advanced = TRUE
          ),
          msterp_schema_field(
            "venn_outline_color", "string", "Venn outline color",
            default = "#000000",
            help = "Outline color for Venn circles (hex).",
            advanced = TRUE
          )
        ),
        mk_style(width = 7, height = 5, axis_text_size = 18)
      ),
      viewer_schema = list(
        msterp_schema_field(
          "overlap_plot_type", "choice", "Plot type",
          default = "venn", choices = c("upset", "venn"),
          choice_labels = c("UpSet plot", "Venn diagram")
        )
      ),
      outputs = list(figures = c("idquant_overlap_plot"), tables = c("idquant_overlap_table"), interactive = TRUE),
      render_spec = list(plots = c("idquant_overlap_plot"), tables = character(0), tabs = NULL)
    ),
    spearman = list(
      engine_id = "spearman",
      label = "Spearman Correlation",
      category = "qc",
      description = "Pairwise scatter + Spearman rho comparisons.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      # params: things that actually change computed data
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups")),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (skip non-control pairwise comparisons)"
        ),
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none"))
      ),
      # style: all PDF "Options" live here
      style_schema = c(
        list(
          msterp_schema_field("point_color_mode", "choice", "Point color", default = "density",
                              choices = c("density", "flat"), advanced = TRUE),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("point_size", "num", "Point size", default = 2, min = 0.5, max = 10, advanced = TRUE),
          msterp_schema_field("point_alpha", "num", "Point opacity", default = 1, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("line_type", "choice", "Best-fit line", default = "y_equals_x",
                              choices = c("none", "y_equals_x", "best_fit", "best_fit_zero_intercept"),
                              choice_labels = c("None", "y = x", "Best fit (y = mx + b)", "Best fit (y = mx)"),
                              advanced = TRUE),
          msterp_schema_field("line_color", "string", "Line color (hex)", default = "#FF0000", advanced = TRUE),
          msterp_schema_field("line_size", "num", "Line width", default = 1, min = 0.1, max = 3, advanced = TRUE),
          msterp_schema_field("show_rho", "bool", "Show rho", default = TRUE, advanced = TRUE),
          msterp_schema_field("show_equation", "bool", "Show equation", default = FALSE,
                              help = "Display line equation below Rho (only when a best-fit line is selected)",
                              advanced = TRUE),
          msterp_schema_field("rho_text_size", "int", "Rho text size", default = 20, min = 6, max = 30, advanced = TRUE),
          msterp_schema_field("rho_color", "string", "Rho color (hex)", default = "#000000", advanced = TRUE),
          msterp_schema_field("rho_position_x", "num", "Rho position X (0-1)", default = 0.05, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("rho_position_y", "num", "Rho position Y (0-1)", default = 0.95, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("axis_title", "string", "Axis title", default = "Intensity", advanced = TRUE),
          msterp_schema_field("xy_range_mode", "choice", "XY range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("xy_min", "num", "XY min", default = 0, advanced = TRUE),
          msterp_schema_field("xy_max", "num", "XY max", default = 12, advanced = TRUE),
          msterp_schema_field("axis_style", "choice", "Axis style", default = "clean", choices = c("clean", "bold"), advanced = TRUE),
          msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20, min = 6, max = 40, advanced = TRUE),
          msterp_schema_field("width",  "num", "Plot width (in)",  default = 6, min = 2, max = 24, advanced = TRUE),
          msterp_schema_field("height", "num", "Plot height (in)", default = 6, min = 2, max = 24, advanced = TRUE)
        )
      ),
      outputs = list(figures = c("spearman_scatter"), tables = c("spearman_rho"), interactive = TRUE),
      render_spec = list(plots = c("spearman_scatter"), tables = character(0), tabs = NULL)
    ),
    
    hor_dis = list(
      engine_id = "hor_dis",
      label = "Histogram / Density",
      category = "qc",
      description = "Distribution plots across groups / replicates.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups", "all_reps"),
                            choice_labels = c("Average groups", "Within groups", "All replicates")),
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none"))
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type", default = "histogram",
                            choices = c("density", "histogram"),
                            choice_labels = c("Density", "Histogram"), advanced = TRUE),
        msterp_schema_field("layout", "choice", "Layout", default = "separate",
                            choices = c("overlay", "separate"),
                            choice_labels = c("Overlay", "Separated"), advanced = TRUE),
        msterp_schema_field("x_axis_title", "string", "X-axis title", default = "Intensity", advanced = TRUE),
        msterp_schema_field("show_group_names", "bool", "Show group names", default = TRUE, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity", default = 0.7, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("show_mean", "bool", "Show mean line", default = TRUE, advanced = TRUE),
        # Mean type: only visible when show_mean is true (conditional UI in page_results.R)
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic"),
                            choice_labels = c("Arithmetic", "Harmonic"), advanced = TRUE),
        msterp_schema_field("show_mean_value", "bool", "Show mean value", default = TRUE, advanced = TRUE),
        msterp_schema_field("mean_line_size", "num", "Mean line thickness", default = 1, min = 0.2, max = 5, advanced = TRUE),
        msterp_schema_field("mean_text_size", "int", "Mean text size", default = 17, min = 6, max = 24, advanced = TRUE),
        # Pooling controls (viewer-time override) - applies pool_value * 0.8 safeguard
        msterp_schema_field("pool_above", "bool", "Pool above threshold", default = TRUE, advanced = TRUE),
        msterp_schema_field("pool_value", "num", "Pool threshold", default = 12, min = 1, max = 50, advanced = TRUE),
        msterp_schema_field("x_range_mode", "choice", "X range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("x_min", "num", "X min", default = 0, advanced = TRUE),
        msterp_schema_field("x_max", "num", "X max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean", choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width",  "num", "Plot width (in)",  default = 7, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)", default = 14, min = 2, max = 24, advanced = TRUE)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Group selector for within_groups mode (dynamically populated from results)
        msterp_schema_field("selected_group", "string", "Group", default = "")
      ),
      outputs = list(figures = c("hor_dis_plot"), tables = c("hor_dis_means"), interactive = FALSE),
      render_spec = list(plots = c("hor_dis_plot"), tables = character(0), tabs = NULL)
    ),

    vert_dis = list(
      engine_id = "vert_dis",
      label = "Box / Violin",
      category = "qc",
      description = "Vertical distribution plot across groups / replicates.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("compare_mode", "choice", "Compare", default = "avg_groups",
                            choices = c("avg_groups", "within_groups", "all_reps"),
                            choice_labels = c("Average groups", "Within groups", "All replicates")),
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none"))
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type", default = "violin",
                            choices = c("box", "violin"),
                            choice_labels = c("Box plot", "Violin plot"), advanced = TRUE),
        msterp_schema_field("y_axis_title", "string", "Y-axis title", default = "Intensity", advanced = TRUE),
        msterp_schema_field("label_rotation", "choice", "Label rotation",
                            default = "0", choices = c("0", "45", "90"),
                            choice_labels = c("Horizontal", "45 degrees", "Vertical"), advanced = TRUE),
        msterp_schema_field("show_n", "bool", "Show n", default = TRUE, advanced = TRUE),
        msterp_schema_field("show_global_mean", "bool", "Show global mean",
                            default = TRUE, advanced = TRUE),
        # Mean type: only visible when show_global_mean is true (conditional UI in page_results.R)
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic"),
                            choice_labels = c("Arithmetic", "Harmonic"), advanced = TRUE),
        msterp_schema_field("mean_line_color", "string", "Mean line color (hex)",
                            default = "#FF0000", advanced = TRUE),
        msterp_schema_field("mean_line_size", "num", "Mean line thickness",
                            default = 1, min = 0.2, max = 5, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity", default = 0.8,
                            min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("y_range_mode", "choice", "Y range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("y_min", "num", "Y min", default = 0, advanced = TRUE),
        msterp_schema_field("y_max", "num", "Y max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean",
                            choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size", default = 20,
                            min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)", default = 5,
                            min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)", default = 6,
                            min = 2, max = 24, advanced = TRUE)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Group selector for within_groups mode (dynamically populated from results)
        msterp_schema_field("selected_group", "string", "Group", default = "")
      ),
      outputs = list(figures = c("vert_dis_plot"), tables = c("vert_dis_means"), interactive = FALSE),
      render_spec = list(plots = c("vert_dis_plot"), tables = character(0), tabs = NULL)
    ),

    # ----------------------------
    # Trends
    # ----------------------------
    pca = list(
      engine_id = "pca",
      label = "PCA",
      category = "trends",
      description = "PCA scores + scree; optional loadings correlation analysis.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "log2", "none")
        ),
        msterp_schema_field("n_pcs", "int", "Number of PCs", default = 3, min = 2, max = 20),
        msterp_schema_field("top_n", "int", "Top N IDs (pos/neg)", default = 50, min = 1, max = 500),
        msterp_schema_field("loadings_corr", "bool", "Loadings correlation analysis", default = FALSE)
      ),
      style_schema = c(
        list(
          msterp_schema_field("point_size", "num", "Point size",
                              default = 5, min = 0.5, max = 10, advanced = TRUE),
          msterp_schema_field("point_alpha", "num", "Point opacity",
                              default = 0.6, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_ellipse", "bool", "Display ellipse",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("ellipse_alpha", "num", "Ellipse opacity",
                              default = 0.25, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("scree_bar_color", "string", "Scree bar color (hex)",
                              default = "#4682B4", advanced = TRUE),
          msterp_schema_field("scree_line_color", "string", "Scree line color (hex)",
                              default = "#E74C3C", advanced = TRUE),
          msterp_schema_field("scree_text_size", "num", "Scree text size",
                              default = 3.5, min = 1, max = 10, advanced = TRUE)
        ),
        mk_style(width = 7, height = 5, axis_text_size = 20)
      ),
      outputs = list(figures = c("pca_scores", "pca_scree"),
                     tables = c("pca_loadings"), interactive = FALSE),
      render_spec = list(plots = c("pca_scores", "pca_scree"),
                         tables = character(0), tabs = NULL)
    ),

    heatmap = list(
      engine_id = "heatmap",
      label = "Targeted Heatmap",
      category = "comparison",
      description = "Standalone heatmap for a user-provided gene/metabolite list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "gene_list", "string", "Gene list (one per line)",
          default = "",
          help = "Paste gene symbols/IDs, one per line (max 2000). Unmatched genes are silently skipped."
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "none")
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are clustered; if FALSE, preserves input gene order.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "autoscale", "bool", "Autoscale (z-score)",
          default = TRUE,
          hidden = TRUE,
          help = "Always compute z-scores to enable zscore/abundance toggle in viewer."
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "color_mode", "choice", "Color mode",
            default = "zscore", choices = c("zscore", "abundance"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "color_palette", "choice", "Color palette",
            default = "PuOr",
            choices = c("viridis", "RdBu", "RdYlBu", "Blues", "Reds", "PuOr"),
            advanced = TRUE
          ),
          # show_row_labels removed per UI change request - hidden from both pipeline and viewer
          msterp_schema_field(
            "show_row_labels", "bool", "Show row labels",
            default = TRUE,
            hidden = TRUE
          ),
          msterp_schema_field(
            "row_font_size", "int", "Row font size",
            default = 8, min = 4, max = 20, advanced = TRUE
          ),
          msterp_schema_field(
            "exclude_na_rows", "bool", "Hide rows with NAs",
            default = TRUE,
            help = "If TRUE, hides rows with any NA before clustering/plotting (viewer-time).",
            advanced = TRUE
          ),
          msterp_schema_field(
            "na_color", "string", "NA color (hex or named)",
            default = "grey50", advanced = TRUE
          ),
          msterp_schema_field(
            "width", "num", "Plot width (in)",
            default = 10, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "height", "num", "Plot height (in)",
            default = 8, min = 2, max = 24, advanced = TRUE
          ),
          msterp_schema_field(
            "cluster_k", "int", "Cluster preview (k)",
            default = 2, min = 2, max = 20,
            help = "Number of clusters to show.",
            hidden = TRUE  # Controlled by cluster analysis panel
          ),
          msterp_schema_field(
            "show_cluster_colors", "bool", "Show cluster colors",
            default = FALSE,
            help = "Show cluster membership color bar on left side of heatmap.",
            hidden = TRUE  # Controlled by cluster analysis panel
          )
        )
      ),
      outputs = list(figures = c("heatmap"), tables = c("heatmap_data"), interactive = FALSE),
      render_spec = list(plots = c("heatmap"), tables = character(0), tabs = NULL)
    ),

    ftest_heatmap = list(
      engine_id = "ftest_heatmap",
      label = "F-test Heatmap",
      category = "comparison",
      description = "Heatmap of statistically significant features from multi-group F-test.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        required_ids = c(),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "stat_test", "choice", "Statistical test",
          default = "welch_anova",
          choices = c("welch_anova", "kruskal_wallis", "limma"),
          choice_labels = c("Welch's ANOVA", "Kruskal-Wallis", "limma (moderated F)")
        ),
        msterp_schema_field(
          "log_transform", "choice", "Log transform",
          default = "log10", choices = c("log10", "none")
        ),
        msterp_schema_field(
          "padj_threshold", "num", "FDR threshold",
          default = 0.05, min = 0, max = 1,
          help = "Benjamini-Hochberg adjusted p-value threshold for significance."
        ),
        msterp_schema_field(
          "top_n", "int", "Max genes",
          default = 100, min = 1, max = 300,
          help = "Maximum number of significant genes to display (sorted by significance)."
        ),
        msterp_schema_field(
          "max_missingness", "num", "Max % Missing Replicates",
          default = 60, min = 0, max = 100,
          help = "Maximum percentage of missing values allowed per row (e.g., 60% means row needs at least 40% valid values)."
        ),
        msterp_schema_field(
          "normalize", "bool", "Median centering",
          default = FALSE,
          help = "Center each sample to the global median before statistical testing.",
          advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_rows", "bool", "Cluster rows",
          default = TRUE,
          help = "If TRUE, rows are hierarchically clustered.",
          advanced = TRUE
        )
      ),
      style_schema = list(
        msterp_schema_field(
          "color_mode", "choice", "Color mode",
          default = "zscore", choices = c("zscore", "abundance"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "color_palette", "choice", "Color palette",
          default = "PuOr",
          choices = c("viridis", "RdBu", "RdYlBu", "Blues", "Reds", "PuOr"),
          advanced = TRUE
        ),
        msterp_schema_field(
          "show_sig_labels", "bool", "Show significance stars",
          default = FALSE,
          help = "Append *, **, *** to row labels based on adjusted p-value.",
          advanced = TRUE
        ),
        # show_row_labels removed per UI change request - hidden from both pipeline and viewer
        msterp_schema_field(
          "show_row_labels", "bool", "Show row labels",
          default = TRUE,
          hidden = TRUE
        ),
        msterp_schema_field(
          "row_font_size", "int", "Row font size",
          default = 7, min = 4, max = 20, advanced = TRUE
        ),
        msterp_schema_field(
          "exclude_na_rows", "bool", "Exclude rows with any NA",
          default = TRUE, advanced = TRUE
        ),
        msterp_schema_field(
          "na_color", "string", "NA color (hex or named)",
          default = "grey50", advanced = TRUE
        ),
        msterp_schema_field(
          "width", "num", "Plot width (in)",
          default = 7, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "height", "num", "Plot height (in)",
          default = 15, min = 2, max = 24, advanced = TRUE
        ),
        msterp_schema_field(
          "cluster_k", "int", "Cluster preview (k)",
          default = 2, min = 2, max = 20,
          help = "Number of clusters to show.",
          hidden = TRUE  # Controlled by cluster analysis panel
        ),
        msterp_schema_field(
          "show_cluster_colors", "bool", "Show cluster colors",
          default = FALSE,
          help = "Show cluster membership color bar on left side of heatmap.",
          hidden = TRUE  # Controlled by cluster analysis panel
        )
      ),
      outputs = list(figures = c("heatmap"), tables = c("heatmap_data", "stats_table"), interactive = FALSE),
      render_spec = list(plots = c("heatmap"), tables = c("stats_table"), tabs = NULL)
    ),

    # ----------------------------
    # Comparison
    # ----------------------------
    volcano = list(
      engine_id = "volcano",
      label = "Volcano",
      category = "comparison",
      description = "Pairwise volcano plots + summaries.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein", "metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = TRUE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      # COMPUTE-TIME: These params determine sig up/down gene lists for GO children
      params_schema = list(
        msterp_schema_field(
          "stat_mode", "choice", "Statistic",
          default = "ttest", choices = c("ttest", "limma"),
          help = "Default: ttest. limma runs an empirical Bayes moderated t-test."
        ),
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (e.g., KO1 vs Control, KO2 vs Control). If no control group is defined, this is ignored."
        ),
        msterp_schema_field(
          "fc_transform", "choice", "Fold-change transform",
          default = "log2", choices = c("none", "log2", "log10")
        ),
        # CRITICAL: Significance semantics for paired GO children - MUST be compute-time
        # apply_fdr: Whether to use FDR-adjusted p-values (TRUE) or raw p-values (FALSE)
        msterp_schema_field(
          "apply_fdr", "bool", "Apply FDR correction",
          default = TRUE,
          help = "Use FDR-adjusted p-values (TRUE) or raw p-values (FALSE). Frozen for child GO analyses."
        ),
        msterp_schema_field(
          "fc_threshold", "range", "Fold-change threshold (min, max)",
          default = c(-1, 1), min = -10, max = 10,
          help = "Defines significant up/down boundaries for child GO analyses"
        ),
        msterp_schema_field(
          "p_threshold", "choice", "p threshold",
          default = "0.05",
          choices = c("0.05", "0.03", "0.01", "0.005", "0.001"),
          help = "p-value cutoff for significance classification (applies to FDR-adjusted or raw p-values based on apply_fdr)"
        ),
        msterp_schema_field("show_summary_table", "bool", "Show summary table", default = TRUE, hidden = TRUE)
      ),
      # VIEWER-TIME: Pure aesthetics, changeable in Results Viewer
      style_schema = c(
        list(
          msterp_schema_field(
            "acquisition_mode", "choice", "Acquisition",
            default = "dia", choices = c("dda", "dia"),
            choice_labels = c("DDA", "DIA"),
            help = "Controls naming in volcano summary tables."
          ),
          msterp_schema_field(
            "label_mode", "choice", "Label mode",
            default = "color_sig",
            choices = c("color_sig", "hide_nonsig"),
            advanced = TRUE
          ),
          msterp_schema_field(
            "show_summary_cards", "bool", "Show summary info",
            default = TRUE,
            advanced = TRUE,
            help = "Display summary statistics as subtitle on plot"
          ),
          msterp_schema_field(
            "summary_text_size", "num", "Summary text size",
            default = 3.5,
            min = 1,
            max = 8,
            advanced = TRUE,
            help = "Text size for summary info cards"
          ),
          msterp_schema_field("show_cut_lines", "bool", "Show cutoff lines", default = FALSE, advanced = TRUE),
          msterp_schema_field("point_size", "num", "Point size", default = 3, min = 0.5, max = 10, advanced = TRUE),
          # FIX: Add label font size control for volcano gene labels
          msterp_schema_field("label_font_size", "int", "Label font size", default = 12, min = 6, max = 30, advanced = TRUE),

          msterp_schema_field("col_sig_up", "string", "Color: significant up (hex)", default = "#FF4242", advanced = TRUE),
          msterp_schema_field("col_sig_down", "string", "Color: significant down (hex)", default = "#4245FF", advanced = TRUE),
          msterp_schema_field("col_nonsig", "string", "Color: non-significant (hex)", default = "#B0B0B0", advanced = TRUE),

          # Axis range controls
          msterp_schema_field("x_range_mode", "choice", "X range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("x_min", "num", "X min", default = -7, advanced = TRUE),
          msterp_schema_field("x_max", "num", "X max", default = 7, advanced = TRUE),
          msterp_schema_field("y_range_mode", "choice", "Y range", default = "auto",
                              choices = c("auto", "manual"), advanced = TRUE),
          msterp_schema_field("y_max", "num", "Y max", default = 7, advanced = TRUE)
        ),
        common_style,
        list(
          msterp_schema_field("y_min", "num", "Y min", default = 0, min = 0, hidden = TRUE)
        )
      ),
      viewer_schema = list(
        msterp_schema_field(
          "view_mode", "choice", "Viewer mode",
          default = "export_preview", choices = c("export_preview", "interactive"),
          choice_labels = c("Export preview", "Interactive")
        ),
        # label_genes: per-plot map stored in label_genes_map (JSON); UI renders custom control
        # Moved from style_schema to viewer_schema - default is empty (no pre-filled genes)
        msterp_schema_field(
          "label_genes_map", "string", "Per-plot gene labels (JSON)",
          default = "{}"
        )
      ),
      outputs = list(
        figures = c("volcano_plot"),
        tables = c("volcano_summary"),
        interactive = TRUE,
        plotly_allowed = TRUE,
        default_plot_mode = "ggplot",
        click_target = "gene"
      ),
      render_spec = list(plots = c("volcano_plot"), tables = c("volcano_summary"), tabs = NULL)
    ),
    
    # ----------------------------
    # Enrichment
    # ----------------------------
    goora = list(
      engine_id = "goora",
      label = "GO-ORA",
      category = "enrichment",
      description = "GO over-representation analysis.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.05, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1,
                            help = "Minimum proteins in database annotated to a GO term (filters small terms)"),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 3, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "plot_type", "choice", "Plot type",
            default = "bar", choices = c("bar", "dot"), advanced = TRUE
          ),
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("alpha", "num", "Opacity", default = 0.8, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show GO ID in labels", default = FALSE,
                              help = "Display GO ID beside term name (e.g., 'Term Name (GO:0000000)')",
                              advanced = TRUE),
          msterp_schema_field("font_size", "int", "Font size", default = 14, min = 6, max = 30, advanced = TRUE)
        ),
        mk_style(width = 12, height = 6, axis_text_size = 20)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC only - no ALL option)
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC")),
        msterp_schema_field(
          "flip_axis", "bool", "Flip horizontal axis",
          default = FALSE
        )
      ),
      outputs = list(
        figures = c("goora_plot"),
        tables = c("goora_table"),
        interactive = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("goora_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("goora_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    `1dgofcs` = list(
      engine_id = "1dgofcs",
      label = "1D GO-FCS",
      category = "enrichment",
      description = "1D functional class scoring on a ranked gene list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons against control (e.g., log2(KO1/Control), log2(KO2/Control)). If no control group is defined, this is ignored."
        ),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 5, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "plot_type", "choice", "Plot type",
            default = "bar", choices = c("bar", "dot"), advanced = TRUE
          ),
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)", default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("alpha", "num", "Opacity", default = 0.8, min = 0, max = 1, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show GO ID in labels", default = FALSE,
                              help = "Display GO ID beside term name (e.g., 'Term Name (GO:0000000)')",
                              advanced = TRUE),
          msterp_schema_field("font_size", "int", "Font size", default = 14, min = 6, max = 30, advanced = TRUE)
        ),
        mk_style(width = 14, height = 6, axis_text_size = 20)
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC only - no ALL option)
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC")),
        msterp_schema_field(
          "flip_axis", "bool", "Flip horizontal axis",
          default = FALSE
        )
      ),
      outputs = list(
        figures = c("1dgofcs_plot"),
        tables = c("1dgofcs_table"),
        interactive = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("1dgofcs_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("1dgofcs_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    `2dgofcs` = list(
      engine_id = "2dgofcs",
      label = "2D GO-FCS",
      category = "enrichment",
      description = "2D functional class scoring on two ranked gene lists.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = TRUE,
        required_ids = c("gene"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field(
          "control_only", "bool", "Only compare against control",
          default = FALSE,
          help = "When a control group is defined, only generate comparisons involving control (e.g., log2(KO1/Control) vs log2(KO2/Control)). If no control group is defined, this is ignored."
        ),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff", default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_term_size", "int", "Min term size", default = 5, min = 1),
        msterp_schema_field("min_overlap", "int", "Min overlap", default = 5, min = 1,
                            help = "Minimum query proteins that must overlap with a GO term (filters weak hits)"),
        msterp_schema_field("max_terms", "int", "Terms to show", default = 20, min = 1, max = 200),
        msterp_schema_field(
          "ontology", "choice", "Ontology",
          default = "ALL", choices = c("ALL", "BP", "MF", "CC"),
          help = "Select which ontologies to compute: ALL (BP, MF, CC), or a single ontology.",
          hidden = TRUE
        )
      ),
      style_schema = c(
        list(
          msterp_schema_field(
            "color_mode", "choice", "Coloring",
            default = "fdr", choices = c("fdr", "flat"), advanced = TRUE
          ),
          msterp_schema_field(
            "fdr_palette", "choice", "FDR color palette",
            default = "yellow_cap", choices = c("yellow_cap", "blue_red"),
            choice_labels = c("Yellow (significant)", "Blue-Red"), advanced = TRUE
          ),
          msterp_schema_field("flat_color", "string", "Flat color (hex)",
                              default = "#B0B0B0", advanced = TRUE),
          msterp_schema_field("dot_alpha", "num", "Dot opacity",
                              default = 1, min = 0, max = 1, advanced = TRUE),
          # Reference lines (x=0, y=0) with independent controls for thickness, style, visibility
          msterp_schema_field("show_ref_lines", "bool", "Show x=0 and y=0 lines",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("ref_line_size", "num", "Reference line thickness",
                              default = 0.5, min = 0.2, max = 5, advanced = TRUE),
          msterp_schema_field("ref_line_dotted", "bool", "Dotted reference lines",
                              default = TRUE, advanced = TRUE),
          # Diagonal guideline controls (y=x and y=-x)
          msterp_schema_field("show_diagonal_guides", "bool", "Show y=x and y=-x guidelines",
                              default = TRUE, advanced = TRUE),
          msterp_schema_field("diagonal_guide_size", "num", "Diagonal guide thickness",
                              default = 1, min = 0.2, max = 5, advanced = TRUE),
          msterp_schema_field("label_font_size", "int", "Label font size",
                              default = 12, min = 6, max = 30, advanced = TRUE),
          msterp_schema_field("show_go_id", "bool", "Show GO ID in labels", default = FALSE,
                              help = "Display GO ID beside term name (e.g., 'Term Name (GO:0000000)')",
                              advanced = TRUE)
        ),
        mk_style(width = 8, height = 6, axis_text_size = 20),
        list(
          # Axis range controls - hidden (internal use only)
          msterp_schema_field("x_min", "num", "X min", default = -1, min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("x_max", "num", "X max", default = 1,  min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("y_min", "num", "Y min", default = -1, min = -100, max = 100, hidden = TRUE),
          msterp_schema_field("y_max", "num", "Y max", default = 1,  min = -100, max = 100, hidden = TRUE)
        )
      ),
      # VIEWER-ONLY: Fields only shown in Results Viewer
      viewer_schema = list(
        # Ontology filter (BP/MF/CC only - no ALL option)
        msterp_schema_field(
          "view_mode", "choice", "Viewer mode",
          default = "export_preview", choices = c("export_preview", "interactive"),
          choice_labels = c("Export preview", "Interactive")
        ),
        msterp_schema_field("ontology_filter", "choice", "Ontology",
                            default = "BP", choices = c("BP", "MF", "CC"))
      ),
      outputs = list(
        figures = c("2dgofcs_plot"),
        tables = c("2dgofcs_table"),
        interactive = TRUE,
        plotly_allowed = TRUE,
        default_plot_mode = "ggplot",
        click_target = "term"
      ),
      render_spec = list(
        plots = c("2dgofcs_plot", "bp_plot", "mf_plot", "cc_plot"),
        tables = c("2dgofcs_table", "bp_table", "mf_table", "cc_table"),
        tabs = c("BP", "MF", "CC")
      )
    ),

    subloc = list(
      engine_id = "subloc",
      label = "Subcellular Localization",
      category = "enrichment",
      description = "Subcellular localization group-wise distributions from terpbase.",
      supports_sequential = FALSE,
      accepted_input_levels = c("protein"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = TRUE,
        required_ids = c("protein"),
        analysis_levels = c("protein")
      ),
      params_schema = list(
        msterp_schema_field("log_transform", "choice", "Log transform", default = "log10",
                            choices = c("log10", "log2", "none")),
        msterp_schema_field("mean_type", "choice", "Mean type", default = "arithmetic",
                            choices = c("arithmetic", "harmonic")),
        msterp_schema_field("orientation", "choice", "Orientation", default = "vertical",
                            choices = c("vertical", "horizontal", "sideways"),
                            choice_labels = c("Vertical", "(horizontal)", "Sideways (labels as strips)"),
                            hidden = TRUE)
      ),
      style_schema = list(
        # FIX: Global mean is now a style option (viewer-time) not compute-time param
        msterp_schema_field("y_axis_title", "string", "Y-axis title", default = "Intensity"),
        msterp_schema_field("label_rotation", "choice", "Label rotation",
                            default = "0", choices = c("0", "45", "90"),
                            choice_labels = c("Horizontal", "45 degrees", "Vertical"),
                            advanced = TRUE),
        msterp_schema_field("show_global_mean", "bool", "Show global mean line", default = FALSE,
                            help = "Display the global mean across all groups as a reference line",
                            advanced = TRUE),
        msterp_schema_field("global_mean_color", "string", "Global mean color (hex)", default = "#FF0000", advanced = TRUE),
        msterp_schema_field("global_mean_size", "num", "Global mean line thickness", default = 1, min = 0.5, max = 5, advanced = TRUE),
        msterp_schema_field("color_mode", "choice", "Color", default = "group",
                            choices = c("group", "flat"), advanced = TRUE),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#B0B0B0", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Box opacity",
                            default = 0.3, min = 0, max = 1, advanced = TRUE),
        # Y-axis range controls
        msterp_schema_field("y_range_mode", "choice", "Y-axis range", default = "auto",
                            choices = c("auto", "manual"), advanced = TRUE),
        msterp_schema_field("y_min", "num", "Y-axis min", default = 0, advanced = TRUE),
        msterp_schema_field("y_max", "num", "Y-axis max", default = 12, advanced = TRUE),
        msterp_schema_field("axis_style", "choice", "Axis style", default = "clean",
                            choices = c("clean", "bold"), advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 20, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width",  "num", "Plot width (in)",
                            default = 7, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 6, min = 2, max = 24, advanced = TRUE)
      ),
      outputs = list(figures = c("subloc_plot"), tables = c("subloc_counts"),
                     interactive = FALSE),
      render_spec = list(plots = c("subloc_plot"), tables = c("subloc_counts"), tabs = NULL)
    ),

    # ----------------------------
    # Metabolite Enrichment Engines
    # ----------------------------
    msea = list(
      engine_id = "msea",
      label = "Pathway Enrichment (MSEA)",
      category = "enrichment",
      description = "Metabolite Set Enrichment Analysis - pathway over-representation using KEGG/Reactome.",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        requires_metabobase = TRUE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(
        msterp_schema_field("pathway_db", "choice", "Pathway database",
                            default = "all",
                            choices = c("kegg", "reactome", "all"),
                            choice_labels = c("KEGG only", "Reactome only", "KEGG + Reactome")),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
                            default = 0.05, min = 0, max = 1),
        msterp_schema_field("min_pathway_size", "int", "Min pathway size",
                            default = 3, min = 1, max = 100),
        msterp_schema_field("min_overlap", "int", "Min overlap",
                            default = 2, min = 1, max = 50,
                            help = "Minimum number of query metabolites in pathway"),
        msterp_schema_field("max_terms", "int", "Max terms to show",
                            default = 20, min = 1, max = 200)
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type",
                            default = "bar",
                            choices = c("bar", "dot"),
                            choice_labels = c("Bar chart", "Dot plot")),
        msterp_schema_field("color_mode", "choice", "Color by",
                            default = "fdr",
                            choices = c("fdr", "flat"),
                            choice_labels = c("FDR value", "Flat color")),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#4A90D9", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity",
                            default = 0.8, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 12, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)",
                            default = 10, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 8, min = 2, max = 24, advanced = TRUE)
      ),
      outputs = list(figures = c("msea_plot"), tables = c("msea_results"), interactive = TRUE),
      render_spec = list(plots = c("msea_plot"), tables = c("msea_results"), tabs = NULL)
    ),

    class_enrichment = list(
      engine_id = "class_enrichment",
      label = "Chemical Class Enrichment",
      category = "enrichment",
      description = "Enrichment analysis of chemical classes (lipids, amino acids, etc.).",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      requirements = list(
        min_groups = 1,
        requires_terpbase = FALSE,
        requires_metabobase = TRUE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(
        msterp_schema_field("class_level", "choice", "Classification level",
                            default = "class",
                            choices = c("superclass", "class", "subclass"),
                            choice_labels = c("Superclass", "Class", "Subclass")),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
                            default = 0.05, min = 0, max = 1),
        msterp_schema_field("min_class_size", "int", "Min class size",
                            default = 3, min = 1, max = 100),
        msterp_schema_field("max_terms", "int", "Max terms to show",
                            default = 20, min = 1, max = 200)
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type",
                            default = "bar",
                            choices = c("bar", "dot"),
                            choice_labels = c("Bar chart", "Dot plot")),
        msterp_schema_field("color_mode", "choice", "Color by",
                            default = "fdr",
                            choices = c("fdr", "flat"),
                            choice_labels = c("FDR value", "Flat color")),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#7B68EE", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity",
                            default = 0.8, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 12, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)",
                            default = 10, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 8, min = 2, max = 24, advanced = TRUE)
      ),
      outputs = list(figures = c("class_enrichment_plot"), tables = c("class_enrichment_results"), interactive = TRUE),
      render_spec = list(plots = c("class_enrichment_plot"), tables = c("class_enrichment_results"), tabs = NULL)
    ),

    pathway_fcs = list(
      engine_id = "pathway_fcs",
      label = "1D Pathway FCS",
      category = "enrichment",
      description = "1D Pathway Functional Class Scoring on a ranked metabolite list.",
      supports_sequential = FALSE,
      accepted_input_levels = c("metabolite"),
      requirements = list(
        min_groups = 2,
        requires_terpbase = FALSE,
        requires_metabobase = TRUE,
        required_ids = c("metabolite"),
        analysis_levels = c("metabolite")
      ),
      params_schema = list(
        msterp_schema_field("pathway_db", "choice", "Pathway database",
                            default = "all",
                            choices = c("kegg", "reactome", "all"),
                            choice_labels = c("KEGG only", "Reactome only", "KEGG + Reactome")),
        msterp_schema_field("fdr_cutoff", "num", "FDR cutoff",
                            default = 0.03, min = 0, max = 1),
        msterp_schema_field("min_pathway_size", "int", "Min pathway size",
                            default = 5, min = 1, max = 100),
        msterp_schema_field("min_overlap", "int", "Min overlap",
                            default = 3, min = 1, max = 50),
        msterp_schema_field("max_terms", "int", "Max terms per database",
                            default = 20, min = 1, max = 200)
      ),
      style_schema = list(
        msterp_schema_field("plot_type", "choice", "Plot type",
                            default = "bar",
                            choices = c("bar", "dot"),
                            choice_labels = c("Bar chart", "Dot plot")),
        msterp_schema_field("color_mode", "choice", "Color by",
                            default = "score",
                            choices = c("score", "fdr", "flat"),
                            choice_labels = c("Enrichment score", "FDR value", "Flat color")),
        msterp_schema_field("flat_color", "string", "Flat color (hex)",
                            default = "#20B2AA", advanced = TRUE),
        msterp_schema_field("alpha", "num", "Opacity",
                            default = 0.8, min = 0, max = 1, advanced = TRUE),
        msterp_schema_field("axis_text_size", "int", "Axis text size",
                            default = 12, min = 6, max = 40, advanced = TRUE),
        msterp_schema_field("width", "num", "Plot width (in)",
                            default = 14, min = 2, max = 24, advanced = TRUE),
        msterp_schema_field("height", "num", "Plot height (in)",
                            default = 6, min = 2, max = 24, advanced = TRUE)
      ),
      outputs = list(figures = c("pathway_fcs_plot"), tables = c("pathway_fcs_results"), interactive = TRUE),
      render_spec = list(plots = c("pathway_fcs_plot"), tables = c("pathway_fcs_results"), tabs = NULL)
    )

  )

  for (eid in names(engines)) {
    if (is.null(engines[[eid]]$type) || !nzchar(as.character(engines[[eid]]$type))) {
      engines[[eid]]$type <- "engine"
    }
    engines[[eid]]$picker_hidden <- isTRUE(engines[[eid]]$picker_hidden %||% FALSE)
    engines[[eid]]$locked_parent <- isTRUE(engines[[eid]]$locked_parent %||% FALSE)
    if (is.null(engines[[eid]]$viewer_schema)) engines[[eid]]$viewer_schema <- list()
  }

  .registry_cache <<- list(registry_version = registry_version, engines = engines)
  .registry_cache
}

msterp_engine_picker_hidden <- function(engine) {
  isTRUE(engine$picker_hidden %||% FALSE)
}

msterp_engine_locked_parent <- function(engine) {
  isTRUE(engine$locked_parent %||% FALSE)
}

msterp_engine_requirements <- function(engine) {
  req <- engine$requirements %||% list()
  req$min_groups <- req$min_groups %||% engine$min_groups %||% 1
  req$requires_terpbase <- req$requires_terpbase %||% engine$requires_terpbase %||% FALSE
  req$required_ids <- req$required_ids %||% engine$required_ids %||% character()
  req$analysis_levels <- req$analysis_levels %||% engine$analysis_levels %||% character()
  req
}

msterp_engine_outputs <- function(engine) {
  out <- engine$outputs %||% list()
  if (is.character(out)) out <- list()
  out$figures <- out$figures %||% character()
  out$tables <- out$tables %||% character()
  out$interactive <- isTRUE(out$interactive %||% FALSE)
  out
}

msterp_engine_get <- function(engine_id, registry = msterp_engine_registry()) {
  registry$engines[[engine_id]]
}

get_engine_def <- function(engine_id, registry = msterp_engine_registry()) {
  if (is.null(engine_id)) return(NULL)
  engine_id <- as.character(engine_id)
  if (!length(engine_id) || !nzchar(engine_id[[1]])) return(NULL)

  eid <- migrate_legacy_engine_name(engine_id[[1]])
  msterp_engine_get(tolower(eid), registry = registry)
}

msterp_engine_is_picker_hidden <- function(engine_id, registry = msterp_engine_registry()) {
  msterp_engine_picker_hidden(msterp_engine_get(engine_id, registry = registry) %||% list())
}

msterp_engine_is_locked_parent <- function(engine_id, registry = msterp_engine_registry()) {
  msterp_engine_locked_parent(msterp_engine_get(engine_id, registry = registry) %||% list())
}

msterp_engine_ids <- function(registry = msterp_engine_registry()) {
  names(registry$engines)
}

# ---- Registry Sanity Check ---------------------------------------------------

tb_registry_render_spec_sanity <- function(registry = msterp_engine_registry()) {
  if (is.null(registry) || is.null(registry$engines)) {
    warning("Registry is NULL or has no engines")
    return(invisible(FALSE))
  }

  issues <- character()

  for (eng_id in names(registry$engines)) {
    eng <- registry$engines[[eng_id]]
    render_spec <- eng$render_spec

    if (is.null(render_spec)) {
      issues <- c(issues, sprintf("Engine '%s': missing render_spec", eng_id))
      next
    }

    # Check that plots is a character vector
    plots <- render_spec$plots
    if (!is.null(plots) && !is.character(plots)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$plots must be character vector, got %s",
        eng_id, class(plots)[[1]]
      ))
    }

    # Check for duplicate plot names
    if (is.character(plots) && length(plots) > 0) {
      if (any(duplicated(plots))) {
        issues <- c(issues, sprintf(
          "Engine '%s': render_spec$plots has duplicates: %s",
          eng_id, paste(plots[duplicated(plots)], collapse = ", ")
        ))
      }
    }

    # Check that tables is a character vector
    tables <- render_spec$tables
    if (!is.null(tables) && !is.character(tables)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$tables must be character vector, got %s",
        eng_id, class(tables)[[1]]
      ))
    }

    # Check for duplicate table names
    if (is.character(tables) && length(tables) > 0) {
      if (any(duplicated(tables))) {
        issues <- c(issues, sprintf(
          "Engine '%s': render_spec$tables has duplicates: %s",
          eng_id, paste(tables[duplicated(tables)], collapse = ", ")
        ))
      }
    }

    # Check tabs
    tabs <- render_spec$tabs
    if (!is.null(tabs) && !is.character(tabs)) {
      issues <- c(issues, sprintf(
        "Engine '%s': render_spec$tabs must be NULL or character vector, got %s",
        eng_id, class(tabs)[[1]]
      ))
    }
  }

  if (length(issues) > 0) {
    for (issue in issues) {
      warning(issue)
    }
    return(invisible(FALSE))
  }

  message(sprintf(
    "Registry render_spec sanity check passed for %d engines",
    length(registry$engines)
  ))
  invisible(TRUE)
}

# ---- Registry Metadata Validation --------------------------------------------
# Optional validator for engine metadata shape and enums.
# Not called automatically; intended for development/testing use.

msterp_validate_registry <- function(registry = msterp_engine_registry()) {

  # Canonical allowed values for input levels (lowercase only)
  valid_input_levels <- c("protein", "peptide", "metabolite")

  # Valid field types for schema fields
  valid_field_types <- c("choice", "bool", "int", "num", "string", "range")

  if (is.null(registry) || is.null(registry$engines)) {
    stop("Registry is NULL or has no engines")
  }

  issues <- character()
  warnings <- character()

  for (eng_id in names(registry$engines)) {
    eng <- registry$engines[[eng_id]]

    # -------------------------------------------------------
    # 1. Check accepted_input_levels exists and is valid
    # -------------------------------------------------------
    ail <- eng$accepted_input_levels
    if (is.null(ail)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing accepted_input_levels", eng_id
      ))
    } else if (!is.character(ail) || length(ail) == 0) {
      issues <- c(issues, sprintf(
        "Engine '%s': accepted_input_levels must be non-empty character vector", eng_id
      ))
    } else {
      invalid <- setdiff(ail, valid_input_levels)
      if (length(invalid) > 0) {
        issues <- c(issues, sprintf(
          "Engine '%s': accepted_input_levels contains invalid values: %s",
          eng_id, paste(invalid, collapse = ", ")
        ))
      }
    }

    # -------------------------------------------------------
    # 2. Check analysis_levels in requirements (existing field)
    # -------------------------------------------------------
    req <- eng$requirements
    if (!is.null(req$analysis_levels)) {
      al <- req$analysis_levels
      if (!is.character(al)) {
        issues <- c(issues, sprintf(
          "Engine '%s': requirements$analysis_levels must be character vector", eng_id
        ))
      }
    }

    # -------------------------------------------------------
    # 3. Validate params_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$params_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing params_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$params_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': params_schema must be a list", eng_id
      ))
    } else {
      # Validate each field in params_schema
      for (i in seq_along(eng$params_schema)) {
        field <- eng$params_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)

        # Check field type is valid
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': params_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }

        # Check default exists for compute params
        if (is.null(field$default) && !isTRUE(field$hidden)) {
          warnings <- c(warnings, sprintf(
            "Engine '%s': params_schema$%s missing default value",
            eng_id, fname
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 4. Validate style_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$style_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing style_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$style_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': style_schema must be a list", eng_id
      ))
    } else {
      # Validate each field in style_schema
      for (i in seq_along(eng$style_schema)) {
        field <- eng$style_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)

        # Check field type is valid
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': style_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 4b. Validate viewer_schema exists (can be empty list)
    # -------------------------------------------------------
    if (is.null(eng$viewer_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing viewer_schema (use list() if empty)", eng_id
      ))
    } else if (!is.list(eng$viewer_schema)) {
      issues <- c(issues, sprintf(
        "Engine '%s': viewer_schema must be a list", eng_id
      ))
    } else {
      for (i in seq_along(eng$viewer_schema)) {
        field <- eng$viewer_schema[[i]]
        if (!is.list(field)) next
        fname <- field$name %||% sprintf("field[%d]", i)
        if (!is.null(field$type) && !field$type %in% valid_field_types) {
          issues <- c(issues, sprintf(
            "Engine '%s': viewer_schema$%s has invalid type '%s'",
            eng_id, fname, field$type
          ))
        }
      }
    }

    # -------------------------------------------------------
    # 5. Check for known misplacements (compute fields in style)
    # -------------------------------------------------------
    # Known compute-time fields that should NOT be in style_schema
    compute_only_keywords <- c("threshold", "cutoff", "transform", "compare")

    # Known viewer-time mode suffixes (allowed in style_schema)
    allowed_style_modes <- c(
      "range_mode",   # axis range mode (auto/manual)
      "color_mode",   # coloring mode (group/flat/density)
      "label_mode",   # label display mode (color_sig/hide_nonsig)
      "limit_mode",   # axis limit mode (auto/manual)
      "plot_mode"     # plot rendering mode (ggplot/plotly)
    )

    if (is.list(eng$style_schema)) {
      for (i in seq_along(eng$style_schema)) {
        field <- eng$style_schema[[i]]
        if (!is.list(field)) next
        fname <- tolower(field$name %||% "")

        # Check if field name suggests compute-time behavior
        for (kw in compute_only_keywords) {
          if (grepl(kw, fname, fixed = TRUE)) {
            # Check if it's an allowed style mode
            is_allowed <- any(sapply(allowed_style_modes, function(am) grepl(am, fname, fixed = TRUE)))
            if (!is_allowed) {
              warnings <- c(warnings, sprintf(
                "Engine '%s': style_schema field '%s' may be compute-time (contains '%s')",
                eng_id, field$name, kw
              ))
            }
          }
        }
      }
    }

    # -------------------------------------------------------
    # 6. Check render_spec exists
    # -------------------------------------------------------
    if (is.null(eng$render_spec)) {
      issues <- c(issues, sprintf(
        "Engine '%s': missing render_spec", eng_id
      ))
    }
  }

  # Report warnings (non-fatal)
  if (length(warnings) > 0) {
    for (w in warnings) {
      warning(w)
    }
  }

  # Report errors (fatal)
  if (length(issues) > 0) {
    stop(paste(c("Registry validation failed:", issues), collapse = "\n  "))
  }

  message(sprintf(
    "Registry validation passed: %d engines, %d warnings",
    length(registry$engines), length(warnings)
  ))
  invisible(TRUE)
}

#' Migrate legacy engine IDs to current engine IDs
#'
#' This is a narrow compatibility layer for older `.terpflow`/`.terpbook` files
#' that reference historical engine IDs.
#'
#' @param engine_id Character vector of engine IDs (any case)
#' @return Character vector of migrated engine IDs (lowercase)
migrate_legacy_engine_name <- function(engine_id) {
  if (is.null(engine_id)) return(engine_id)
  x <- as.character(engine_id)
  if (length(x) == 0) return(x)

    map <- c(
      # GO-FCS legacy IDs
      "gofcs_1d" = "1dgofcs",
      "gofcs1d" = "1dgofcs",
      "gofcs_2d" = "2dgofcs",
      "gofcs2d" = "2dgofcs",
      # Legacy IDQuant CV view
      "idquant_cv" = "idquant_cv_scatter"
    )

  y <- tolower(x)
  hit <- y %in% names(map)
  y[hit] <- unname(map[y[hit]])
  y
}

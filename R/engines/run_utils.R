# =========================================================
# R/engines/run_utils.R ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Â New Run utilities and validators
#
# Centralized functions for:
#  - Analysis level normalization (protein-only)
#  - Input loaders for New Run
#  - Validation functions
#  - Run plan compilation
# =========================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =========================================================
# Analysis Level Normalization
# =========================================================

#' Canonical analysis levels (lowercase only)
#' @return Character vector of valid analysis levels
nr_valid_analysis_levels <- function() {
  c("protein", "peptide", "metabolite")
}

#' Normalize analysis level to canonical lowercase form
#' @param level Character string (e.g., "Protein", "PROTEIN", "protein")
#' @return Normalized lowercase string or NULL if invalid
nr_normalize_analysis_level <- function(level) {
  # Ensure we have a single scalar value
  if (is.null(level) || length(level) == 0) return(NULL)
  level <- as.character(level)[1]  # Take first element if vector
  if (!nzchar(level)) return(NULL)
  lvl <- tolower(trimws(level))
  if (!lvl %in% nr_valid_analysis_levels()) return(NULL)
  lvl
}

#' Validate analysis level against canonical vocabulary
#' @param level Character string
#' @return list(ok, normalized, error)
nr_validate_analysis_level <- function(level) {
  # Ensure we have a single scalar value
  if (is.null(level) || length(level) == 0) {
    return(list(ok = FALSE, normalized = NULL, error = "Missing analysis_level"))
  }
  level <- as.character(level)[1]  # Take first element if vector
  if (!nzchar(level)) {
    return(list(ok = FALSE, normalized = NULL, error = "Missing analysis_level"))
  }

  normalized <- nr_normalize_analysis_level(level)
  if (is.null(normalized)) {
    return(list(
      ok = FALSE,
      normalized = NULL,
      error = sprintf(
        "Invalid analysis_level '%s'. Must be one of: %s",
        level, paste(nr_valid_analysis_levels(), collapse = ", ")
      )
    ))
  }

  list(ok = TRUE, normalized = normalized, error = NULL)
}

# =========================================================
# Input Loaders
# =========================================================

#' Load formatted Excel file with normalized metadata
#' @param path Path to .xlsx file with data + design sheets
#' @return list(path, data, design, metadata) or error
nr_load_formatted <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, error = "Formatted file not found"))
  }

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) {
    return(list(ok = FALSE, error = "Expected .xlsx file"))
  }

  # Use existing loader if available
  if (!exists("msterp_read_formatted_xlsx", mode = "function")) {
    return(list(ok = FALSE, error = "msterp_read_formatted_xlsx not available"))
  }

  obj <- tryCatch(
    msterp_read_formatted_xlsx(path),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )

  if (!is.null(obj$error)) return(obj)

  # Extract and normalize metadata
  metadata <- nr_extract_metadata(obj$design)
  if (!isTRUE(metadata$ok)) {
    return(list(ok = FALSE, error = metadata$error))
  }

  list(
    ok = TRUE,
    path = path,
    data = obj$data,
    design = obj$design,
    metadata = metadata$metadata
  )
}

#' Extract and normalize metadata from design sheet
#' @param design_df Design data.frame
#' @return list(ok, metadata, error)
nr_extract_metadata <- function(design_df) {
  if (!is.data.frame(design_df)) {
    return(list(ok = FALSE, error = "Design must be a data.frame"))
  }

  if (!"record_type" %in% names(design_df)) {
    return(list(ok = FALSE, error = "Design missing record_type column"))
  }

  # Extract meta rows

  meta_rows <- design_df[tolower(design_df$record_type) == "meta", , drop = FALSE]
  if (!"key" %in% names(meta_rows) || !"value" %in% names(meta_rows)) {
    return(list(ok = FALSE, error = "Meta rows require key and value columns"))
  }

  meta_map <- stats::setNames(
    as.character(meta_rows$value),
    tolower(as.character(meta_rows$key))
  )

  # Normalize analysis_level (meta_map is a named vector, use [ not [[)
  raw_level <- if ("analysis_level" %in% names(meta_map)) {
    meta_map["analysis_level"]
  } else {
    NULL
  }
  level_check <- nr_validate_analysis_level(raw_level)
  if (!isTRUE(level_check$ok)) {
    return(list(ok = FALSE, error = level_check$error))
  }

  # Extract groups
  group_rows <- design_df[tolower(design_df$record_type) == "group", , drop = FALSE]
  groups <- if (nrow(group_rows) > 0) {
    cols <- intersect(c("group_id", "group_name", "color", "is_control"), names(group_rows))
    g <- group_rows[, cols, drop = FALSE]
    # Normalize is_control to logical, preserving NA when provided
    if ("is_control" %in% names(g)) {
      if (is.logical(g$is_control)) {
        # keep as-is
      } else if (is.numeric(g$is_control) || is.integer(g$is_control)) {
        g$is_control <- ifelse(is.na(g$is_control), NA, g$is_control != 0)
      } else {
        v <- tolower(trimws(as.character(g$is_control)))
        g$is_control <- ifelse(is.na(v) | !nzchar(v), NA, v %in% c("true", "t", "1", "yes"))
      }
    } else {
      g$is_control <- FALSE
    }
    g
  } else {
    data.frame(group_id = character(), group_name = character(),
               color = character(), is_control = logical(),
               stringsAsFactors = FALSE)
  }

  # Extract columns
  col_rows <- design_df[tolower(design_df$record_type) == "column", , drop = FALSE]
  if ("include" %in% names(col_rows)) {
    if (is.logical(col_rows$include)) {
      col_rows <- col_rows[!is.na(col_rows$include) & col_rows$include, , drop = FALSE]
    } else {
      include <- tolower(as.character(col_rows$include)) %in% c("true", "t", "1", "yes")
      col_rows <- col_rows[include, , drop = FALSE]
    }
  }

  # Helper to safely get meta value (meta_map is a named vector, not a list)
  # Always returns a single scalar value (first match if duplicates exist)
  get_meta <- function(key, default = "") {
    if (key %in% names(meta_map)) {
      val <- as.character(meta_map[key])
      # Return only the first value if there are duplicates
      if (length(val) > 1) val <- val[1]
      val
    } else {
      default
    }
  }

  list(
    ok = TRUE,
    metadata = list(
      analysis_level = level_check$normalized,
      id_primary_col = get_meta("id_primary_col"),
      id_protein_col = get_meta("id_protein_col"),
      id_gene_col = get_meta("id_gene_col"),
      id_metabolite_col = get_meta("id_metabolite_col"),
      groups = groups,
      columns = col_rows,
      raw_meta = meta_map
    ),
    error = NULL
  )
}

#' Load .terpflow pipeline file
#' @param path Path to .terpflow file
#' @return list(ok, flow, error)
nr_load_terpflow <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, error = "Terpflow file not found"))
  }

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("terpflow", "rds")) {
    return(list(ok = FALSE, error = "Expected .terpflow file"))
  }

  registry <- if (exists("msterp_engine_registry", mode = "function")) {
    tryCatch(msterp_engine_registry(), error = function(e) NULL)
  } else {
    NULL
  }

  flow <- tryCatch({
    if (exists("msterp_terpflow_load", mode = "function")) {
      msterp_terpflow_load(path, registry = registry)
    } else {
      readRDS(path)
    }
  }, error = function(e) {
    return(list(ok = FALSE, error = paste("Failed to read:", conditionMessage(e))))
  })

  if (!is.null(flow$error)) return(flow)

  if (!is.list(flow)) {
    return(list(ok = FALSE, error = "Invalid terpflow structure"))
  }

  steps <- flow$steps %||% list()
  if (!is.list(steps) || length(steps) == 0) {
    return(list(ok = FALSE, error = "Terpflow has no steps"))
  }

  # Validate all steps have engine_id
 for (i in seq_along(steps)) {
    s <- steps[[i]]
    if (!is.list(s) || !nzchar(s$engine_id %||% "")) {
      return(list(ok = FALSE, error = sprintf("Step %d missing engine_id", i)))
    }
  }

  list(ok = TRUE, flow = flow, error = NULL)
}

#' Load .terpbase file
#' @param path Path to .terpbase file
#' @return list(ok, terpbase, error)
nr_load_terpbase <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, error = "Terpbase file not found", required = FALSE))
  }

  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("terpbase", "rds")) {
    return(list(ok = FALSE, error = "Expected .terpbase file"))
  }

  terpbase <- tryCatch(readRDS(path), error = function(e) {
    return(list(ok = FALSE, error = paste("Failed to read:", conditionMessage(e))))
  })

  if (!is.null(terpbase$error)) return(terpbase)

  list(ok = TRUE, terpbase = terpbase, error = NULL)
}

# =========================================================
# Validation Functions
# =========================================================

#' Validate formatted data object
#' @param formatted Formatted object from nr_load_formatted
#' @return list(ok, errors, warnings)
nr_validate_formatted <- function(formatted) {
  errors <- character()
  warnings <- character()

  if (!isTRUE(formatted$ok)) {
    return(list(ok = FALSE, errors = formatted$error, warnings = character()))
  }

  data <- formatted$data
  meta <- formatted$metadata
  if (is.list(meta) && is.null(meta$current_analysis_level)) {
    meta$current_analysis_level <- nr_normalize_analysis_level(meta$analysis_level %||% NULL) %||% (meta$analysis_level %||% NULL)
  }

  # Check data exists
  if (!is.data.frame(data) || nrow(data) == 0) {
    errors <- c(errors, "Data sheet is empty or invalid")
  }

  # Check primary ID column exists in data
  # Ensure id_col is a single scalar string
  id_col <- as.character(meta$id_primary_col %||% "")[1]
  if (!nzchar(id_col)) {
    errors <- c(errors, "Missing id_primary_col in metadata")
  } else if (!id_col %in% names(data)) {
    errors <- c(errors, sprintf("Primary ID column '%s' not found in data", id_col))
  }

  # Check protein ID column for protein-level analysis
  # Ensure analysis_level is a single scalar string
  analysis_level <- as.character(meta$analysis_level %||% "")[1]
  if (analysis_level %in% c("protein", "peptide")) {
    prot_col <- as.character(meta$id_protein_col %||% "")[1]
    if (!nzchar(prot_col)) {
      errors <- c(errors, sprintf("Analysis level '%s' requires id_protein_col", analysis_level))
    } else if (!prot_col %in% names(data)) {
      errors <- c(errors, sprintf("Protein ID column '%s' not found in data", prot_col))
    }
  }

  # Check measurement columns exist
  if (nrow(meta$columns) == 0) {
    errors <- c(errors, "No measurement columns defined")
  } else {
    meas_cols <- as.character(meta$columns$data_col)
    missing <- setdiff(meas_cols, names(data))
    if (length(missing) > 0) {
      errors <- c(errors, sprintf(
        "Missing measurement columns: %s",
        paste(head(missing, 3), collapse = ", ")
      ))
    }
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

#' Validate terpflow pipeline
#' @param terpflow Terpflow object from nr_load_terpflow
#' @param registry Engine registry (optional)
#' @return list(ok, errors, warnings)
nr_validate_terpflow <- function(terpflow, registry = NULL) {
  errors <- character()
  warnings <- character()

  if (!isTRUE(terpflow$ok)) {
    return(list(ok = FALSE, errors = terpflow$error, warnings = character()))
  }

  flow <- terpflow$flow

  # Get registry
  if (is.null(registry) && exists("msterp_engine_registry", mode = "function")) {
    registry <- msterp_engine_registry()
  }

  if (is.null(registry)) {
    warnings <- c(warnings, "Registry not available for engine validation")
    return(list(ok = TRUE, errors = errors, warnings = warnings))
  }

  engine_ids <- names(registry$engines %||% list())

  # Validate each step
  for (i in seq_along(flow$steps)) {
    s <- flow$steps[[i]]
    eid <- s$engine_id %||% ""

    if (!eid %in% engine_ids) {
      errors <- c(errors, sprintf("Step %d: unknown engine '%s'", i, eid))
    }
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

#' Validate cross-compatibility of all inputs
#' @param formatted Formatted object
#' @param terpflow Terpflow object
#' @param terpbase Terpbase object (optional)
#' @param registry Engine registry
#' @return list(ok, errors, warnings)
nr_validate_run_compatibility <- function(formatted, terpflow,
                                          terpbase = NULL, registry = NULL) {
  errors <- character()
  warnings <- character()

  if (!isTRUE(formatted$ok) || !isTRUE(terpflow$ok)) {
    return(list(ok = FALSE, errors = "Invalid inputs", warnings = character()))
  }

  meta <- formatted$metadata
  flow <- terpflow$flow

  # Get registry
  if (is.null(registry) && exists("msterp_engine_registry", mode = "function")) {
    registry <- msterp_engine_registry()
  }
  if (is.null(registry) || is.null(registry$engines) || !is.list(registry$engines)) {
    warnings <- c(warnings, "Registry not available for engine validation")
    return(list(ok = length(errors) == 0, errors = errors, warnings = warnings))
  }

  # Check if any step requires terpbase
  needs_terpbase <- FALSE
  terpbase_engines <- character()

  # Validate evolving analysis-level compatibility (supports containers + output_level transitions)
  data_level <- nr_normalize_analysis_level(meta$analysis_level %||% NULL)
  if (is.null(data_level)) {
    errors <- c(errors, sprintf("Invalid formatted analysis_level: %s", as.character(meta$analysis_level %||% "")))
  }

  n_groups <- nrow(meta$groups %||% data.frame())

  validate_step_requirements <- function(eng, where) {
    req <- eng$requirements %||% list()
    if (isTRUE(req$requires_terpbase)) {
      needs_terpbase <<- TRUE
      terpbase_engines <<- c(terpbase_engines, eng$engine_id %||% where)
    }

    min_groups <- req$min_groups %||% 1
    if (n_groups < min_groups) {
      errors <<- c(errors, sprintf(
        "%s requires at least %d groups, found %d",
        where, min_groups, n_groups
      ))
    }
  }

  validate_engine_input_level <- function(eng, level, where) {
    accepted <- eng$accepted_input_levels %||% c("protein")
    accepted <- tolower(accepted)
    if (!level %in% accepted) {
      errors <<- c(errors, sprintf("%s does not accept '%s' level data", where, level))
      return(FALSE)
    }
    TRUE
  }

  apply_engine_output_level <- function(eng, level) {
    out <- nr_normalize_analysis_level(eng$output_level %||% NULL)
    if (!is.null(out)) return(out)
    level
  }

  if (!is.null(data_level)) {
    for (i in seq_along(flow$steps %||% list())) {
      s <- flow$steps[[i]]
      eid <- s$engine_id %||% ""
      eng <- registry$engines[[eid]] %||% NULL

      where <- sprintf("Step %d (%s)", i, eid %||% "unknown")
      if (is.null(eng)) {
        errors <- c(errors, sprintf("%s: unknown engine", where))
        next
      }

      step_type <- tolower(s$type %||% eng$type %||% "engine")

      if (identical(step_type, "container")) {
        validate_step_requirements(eng, where)
        validate_engine_input_level(eng, data_level, where)

        allowed <- eng$allowed_child_engines %||% character()
        allowed <- tolower(allowed)

        forced_eid <- tolower(eng$forced_final_substep_engine_id %||% "")
        exit_level <- nr_normalize_analysis_level(eng$exit_level %||% NULL)

        subs <- s$substeps %||% list()
        if (is.list(subs) && length(subs) > 0) {
          subs <- subs[order(vapply(subs, `[[`, integer(1), "order"))]
        } else {
          subs <- list()
        }

        # system_generated substeps must trail
        if (length(subs) > 1) {
          is_sys <- vapply(subs, function(x) isTRUE((x %||% list())$system_generated), logical(1))
          if (any(is_sys) && any(!is_sys & (seq_along(is_sys) > min(which(is_sys))))) {
            errors <- c(errors, sprintf("%s: system_generated substeps must be at the end", where))
          }
        }

        if (nzchar(forced_eid) && length(subs) > 0) {
          forced_pos <- which(vapply(subs, function(x) identical(tolower((x %||% list())$engine_id %||% ""), forced_eid), logical(1)))
          if (length(forced_pos) > 0 && max(forced_pos) != length(subs)) {
            errors <- c(errors, sprintf("%s: forced final substep '%s' must be last", where, forced_eid))
          }
        }

        forced_present <- FALSE

        for (j in seq_along(subs)) {
          ss <- subs[[j]] %||% list()
          seid <- tolower(ss$engine_id %||% "")
          sub_eng <- registry$engines[[seid]] %||% NULL
          sub_where <- sprintf("%s substep %d (%s)", where, j, seid %||% "unknown")

          if (is.null(sub_eng)) {
            errors <- c(errors, sprintf("%s: unknown engine", sub_where))
            next
          }

          if (nzchar(forced_eid) && identical(seid, forced_eid)) forced_present <- TRUE

          # Enforce child allowlist (but always allow forced engine id)
          if (length(allowed) > 0 && !seid %in% allowed && !(nzchar(forced_eid) && identical(seid, forced_eid))) {
            errors <- c(errors, sprintf("%s: engine not allowed inside container", sub_where))
          }

          validate_step_requirements(sub_eng, sub_where)
          validate_engine_input_level(sub_eng, data_level, sub_where)
          data_level <- apply_engine_output_level(sub_eng, data_level)
        }

        if (nzchar(forced_eid) && !forced_present) {
          warnings <- c(warnings, sprintf("%s: missing forced substep '%s' (will be inserted during plan compilation)", where, forced_eid))
          forced_eng <- registry$engines[[forced_eid]] %||% NULL
          if (!is.null(forced_eng)) {
            validate_step_requirements(forced_eng, sprintf("%s forced substep (%s)", where, forced_eid))
            validate_engine_input_level(forced_eng, data_level, sprintf("%s forced substep (%s)", where, forced_eid))
            data_level <- apply_engine_output_level(forced_eng, data_level)
          } else if (!is.null(exit_level)) {
            data_level <- exit_level
          }
        }

        if (!is.null(exit_level) && !identical(data_level, exit_level)) {
          errors <- c(errors, sprintf("%s: container must exit at '%s' level, but current level is '%s'", where, exit_level, data_level))
          data_level <- exit_level
        }

        next
      }

      validate_step_requirements(eng, where)
      validate_engine_input_level(eng, data_level, where)
      data_level <- apply_engine_output_level(eng, data_level)
    }
  }

  if (needs_terpbase && !isTRUE(terpbase$ok)) {
    errors <- c(errors, sprintf(
      "Terpbase required for engines: %s",
      paste(unique(terpbase_engines), collapse = ", ")
    ))
  }

  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}

# =========================================================
# Run Plan Compilation
# =========================================================

#' Compile a deterministic run plan from terpflow + registry
#'
#' Produces a run plan including paired analysis children. For Volcano and PCA
#' steps with enabled paired analysis, child views are compiled with compute-time
#' thresholds frozen from the parent step.
#'
#' PAIRED ANALYSIS INVARIANTS:
#' - Child GO analyses are defined at compile time (not viewer time)
#' - Volcano fc_threshold and p_threshold determine sig_up/sig_down gene lists
#' - These thresholds are stored in child params and cannot be changed by viewer
#' - Viewer can only adjust style (colors, label visibility) not which genes are included
#'
#' @param terpflow Terpflow object
#' @param formatted Formatted object (for metadata)
#' @param registry Engine registry
#' @return list(ok, plan, error)
nr_compile_run_plan <- function(terpflow, formatted, registry = NULL) {
  if (!isTRUE(terpflow$ok) || !isTRUE(formatted$ok)) {
    return(list(ok = FALSE, error = "Invalid inputs"))
  }

  flow <- terpflow$flow
  meta <- formatted$metadata

  # Get registry
  if (is.null(registry) && exists("msterp_engine_registry", mode = "function")) {
    registry <- msterp_engine_registry()
  }
  if (is.null(registry) || is.null(registry$engines) || !is.list(registry$engines)) {
    return(list(ok = FALSE, error = "Engine registry not available"))
  }

  initial_level <- nr_normalize_analysis_level(meta$analysis_level %||% NULL)
  if (is.null(initial_level)) {
    return(list(ok = FALSE, error = sprintf("Invalid formatted analysis_level: %s", as.character(meta$analysis_level %||% ""))))
  }

  schema_defaults_safe <- function(schema) {
    if (exists("msterp_schema_defaults", mode = "function")) {
      return(msterp_schema_defaults(schema))
    }
    list()
  }

  merge_defaults_safe <- function(defaults, values) {
    if (exists("msterp_merge_defaults", mode = "function")) {
      return(msterp_merge_defaults(defaults, values))
    }
    if (is.null(defaults) || length(defaults) == 0) return(values %||% list())
    if (is.null(values) || length(values) == 0) return(defaults)
    modifyList(defaults, values)
  }

  style_defaults_safe <- function(eng) {
    if (is.null(eng)) return(list())
    schema_defaults_safe(c(eng$style_schema %||% list(), eng$viewer_schema %||% list()))
  }

  style_defaults_for_engine_id <- function(engine_id) {
    eng_def <- registry$engines[[tolower(engine_id)]] %||% NULL
    style_defaults_safe(eng_def)
  }

  merge_style_defaults <- function(engine_id, style) {
    merge_defaults_safe(style_defaults_for_engine_id(engine_id), style %||% list())
  }

  apply_output_level <- function(eng, level) {
    out <- nr_normalize_analysis_level(eng$output_level %||% NULL)
    if (!is.null(out)) out else level
  }

  plan <- list(
    meta = list(
      pipeline_id = flow$pipeline_id %||% "unknown",
      pipeline_name = flow$pipeline_name %||% "Unnamed Pipeline",
      analysis_level = initial_level,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      n_steps = length(flow$steps)
    ),
    steps = list()
  )

  current_level <- initial_level

  # Build step entries in deterministic order
  for (i in seq_along(flow$steps)) {
    s <- flow$steps[[i]]
    eid <- tolower(s$engine_id %||% "")
    eng <- registry$engines[[eid]] %||% NULL
    if (is.null(eng)) {
      return(list(ok = FALSE, error = sprintf("Step %d has unknown engine: %s", i, eid)))
    }

    step_type <- tolower(s$type %||% eng$type %||% "engine")

    if (identical(step_type, "container")) {
      container_entry <- list(
        step_index = i,
        step_id = s$step_id %||% sprintf("step_%d", i),
        engine_id = eid,
        type = "container",
        label = s$label %||% (eng$label %||% eid),
        params = s$params %||% list(),
        style = merge_defaults_safe(style_defaults_safe(eng), s$style %||% list()),
        enabled = isTRUE(s$enabled %||% TRUE),
        input_level = current_level,
        output_level = NULL,
        substeps = list()
      )

      subs <- s$substeps %||% list()
      if (!is.list(subs)) subs <- list()
      if (length(subs) > 0) subs <- subs[order(vapply(subs, `[[`, integer(1), "order"))]

      forced_eid <- tolower(eng$forced_final_substep_engine_id %||% "")
      exit_level <- nr_normalize_analysis_level(eng$exit_level %||% NULL)

      for (j in seq_along(subs)) {
        ss <- subs[[j]] %||% list()
        seid <- tolower(ss$engine_id %||% "")
        sub_eng <- registry$engines[[seid]] %||% NULL
        if (is.null(sub_eng)) {
          return(list(ok = FALSE, error = sprintf("Container step %d has unknown substep engine: %s", i, seid)))
        }

        sub_entry <- list(
          step_index = i,
          parent_step_id = container_entry$step_id,
          substep_index = j,
          step_id = ss$step_id %||% sprintf("%s__sub_%d", container_entry$step_id, j),
          engine_id = seid,
          type = "engine",
          label = ss$label %||% (sub_eng$label %||% seid),
          params = ss$params %||% list(),
          style = merge_defaults_safe(style_defaults_safe(sub_eng), ss$style %||% list()),
          enabled = isTRUE(ss$enabled %||% TRUE),
          system_generated = isTRUE(ss$system_generated %||% FALSE),
          input_level = current_level,
          output_level = NULL,
          paired_children = NULL
        )

        out_level <- apply_output_level(sub_eng, current_level)
        sub_entry$output_level <- out_level
        current_level <- out_level

        container_entry$substeps[[length(container_entry$substeps) + 1L]] <- sub_entry
      }

      if (nzchar(forced_eid)) {
        forced_present <- any(vapply(container_entry$substeps, function(x) identical(tolower((x %||% list())$engine_id %||% ""), forced_eid), logical(1)))
        if (!forced_present) {
          forced_eng <- registry$engines[[forced_eid]] %||% NULL
          if (is.null(forced_eng)) {
            return(list(ok = FALSE, error = sprintf("Forced substep engine not found in registry: %s", forced_eid)))
          }

          ssid <- sprintf("%s__sys__%s", container_entry$step_id, forced_eid)
          sub_entry <- list(
            step_index = i,
            parent_step_id = container_entry$step_id,
            substep_index = length(container_entry$substeps) + 1L,
            step_id = ssid,
            engine_id = forced_eid,
            type = "engine",
            label = forced_eng$label %||% forced_eid,
            params = schema_defaults_safe(forced_eng$params_schema),
            style = style_defaults_safe(forced_eng),
            enabled = TRUE,
            system_generated = TRUE,
            input_level = current_level,
            output_level = NULL,
            paired_children = NULL
          )

          out_level <- apply_output_level(forced_eng, current_level)
          sub_entry$output_level <- out_level
          current_level <- out_level

          container_entry$substeps[[length(container_entry$substeps) + 1L]] <- sub_entry
        }
      }

      # Peptide Analysis: always include CV bar-only IDQuant view before final aggregation.
      if (identical(eid, "peptide_analysis")) {
        cv_eid <- "idquant_cv_bar"
        cv_present <- any(vapply(container_entry$substeps, function(x) identical(tolower((x %||% list())$engine_id %||% ""), cv_eid), logical(1)))

        if (!cv_present && nzchar(forced_eid)) {
          forced_idx <- which(vapply(container_entry$substeps, function(x) identical(tolower((x %||% list())$engine_id %||% ""), forced_eid), logical(1)))
          if (length(forced_idx) == 1 && forced_idx == length(container_entry$substeps)) {
            forced_ss <- container_entry$substeps[[forced_idx]]
            container_entry$substeps <- container_entry$substeps[-forced_idx]

            # Rewind to the level just before aggregation so CV runs on peptide-level context.
            current_level <- forced_ss$input_level %||% current_level

            cv_eng <- registry$engines[[cv_eid]] %||% NULL
            if (is.null(cv_eng)) {
              return(list(ok = FALSE, error = sprintf("Required Peptide Analysis substep engine not found in registry: %s", cv_eid)))
            }

            ssid <- sprintf("%s__sys__%s", container_entry$step_id, cv_eid)
            sub_entry <- list(
              step_index = i,
              parent_step_id = container_entry$step_id,
              substep_index = length(container_entry$substeps) + 1L,
              step_id = ssid,
              engine_id = cv_eid,
              type = "engine",
              label = cv_eng$label %||% cv_eid,
              params = schema_defaults_safe(cv_eng$params_schema),
              style = style_defaults_safe(cv_eng),
              enabled = TRUE,
              system_generated = TRUE,
              input_level = current_level,
              output_level = NULL,
              paired_children = NULL
            )

            out_level <- apply_output_level(cv_eng, current_level)
            sub_entry$output_level <- out_level
            current_level <- out_level
            container_entry$substeps[[length(container_entry$substeps) + 1L]] <- sub_entry

            # Re-append the forced final aggregation last.
            forced_ss$substep_index <- length(container_entry$substeps) + 1L
            container_entry$substeps[[length(container_entry$substeps) + 1L]] <- forced_ss
            current_level <- forced_ss$output_level %||% current_level
          }
        }
      }

      if (!is.null(exit_level) && !identical(current_level, exit_level)) {
        return(list(ok = FALSE, error = sprintf(
          "Container step %d must exit at '%s' level, but exits at '%s'",
          i, exit_level, current_level
        )))
      }

      container_entry$output_level <- current_level
      plan$steps[[length(plan$steps) + 1L]] <- container_entry
      next
    }

    step_entry <- list(
      step_index = i,
      step_id = s$step_id %||% sprintf("step_%d", i),
      engine_id = eid,
      label = s$label %||% (eng$label %||% eid),
      params = s$params %||% list(),
      style = merge_defaults_safe(style_defaults_safe(eng), s$style %||% list()),
      enabled = isTRUE(s$enabled %||% TRUE),
      type = "engine",
      input_level = current_level,
      output_level = NULL,
      paired_children = NULL  # Will be populated if paired analysis is enabled
    )

    # =========================================================
    # PAIRED ANALYSIS: Compile child views for Volcano/PCA
    # =========================================================
    # This encodes the paired analysis invariants at compile time.
    # Child GO analyses receive frozen compute-time thresholds.

    paired_cfg <- s$paired %||% list()
    paired_enabled <- isTRUE(paired_cfg$enabled)

    if (paired_enabled && eid %in% c("volcano", "pca")) {
      children <- list()

      # Get parent compute params (thresholds that define child gene lists)
      parent_params <- s$params %||% list()

      if (eid == "volcano") {
        # Volcano ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ GO-ORA children per pairwise comparison
        # CRITICAL: Significance semantics are frozen here at compile time.
        # These determine which proteins are included in GO analysis.

        # Extract frozen thresholds from parent volcano params
        fc_threshold <- parent_params$fc_threshold %||% c(-1, 1)

        # apply_fdr: Whether to use FDR-adjusted p-values (TRUE) or raw p-values (FALSE)
        # This must be explicit per notes.md invariants
        apply_fdr <- parent_params$apply_fdr %||% paired_cfg$apply_fdr %||% TRUE

        # sig_threshold: The p-value cutoff (applies to apply_fdr setting)
        sig_threshold <- parent_params$p_threshold %||% paired_cfg$sig_threshold %||% 0.05
        # Normalize to numeric
        if (is.character(sig_threshold)) {
          sig_threshold <- as.numeric(sig_threshold)
        }

        # Get paired engine config (goora params)
        paired_engine_id <- paired_cfg$engine_id %||% "goora"

        # Get GO-ORA configurations (multi-config support)
        # Backwards compatibility: if no configs array, migrate from old format
        goora_configs <- paired_cfg$configs
        if (is.null(goora_configs) || length(goora_configs) == 0) {
          # Legacy single-config format
          legacy_fdr <- (paired_cfg$params %||% list())$fdr_cutoff %||% 0.03
          legacy_include_unique <- isTRUE(paired_cfg$include_unique_in_sig)
          goora_configs <- list(list(
            config_id = "cfg_1",
            name = "GO-ORA",
            fdr_cutoff = legacy_fdr,
            min_overlap = 1,
            include_unique_in_sig = legacy_include_unique
          ))
        }

        # Get global params (shared across all configs)
        global_params <- paired_cfg$global_params %||% list()
        if (length(global_params) == 0) {
          # Migrate from legacy params
          global_params <- list(
            min_term_size = (paired_cfg$params %||% list())$min_term_size %||% 5,
            max_terms = (paired_cfg$params %||% list())$max_terms %||% 20
          )
        }

        # Get groups from formatted metadata to generate per-comparison children
        groups <- meta$groups$group_name %||% character()
        is_control_raw <- meta$groups$is_control %||% rep(FALSE, length(groups))

        # FIX: Normalize is_control (same pattern as volcano.R)
        # Handles character "true", "t", "1", "yes" as well as logical TRUE
        is_control <- if (is.character(is_control_raw)) {
          tolower(as.character(is_control_raw)) %in% c("true", "t", "1", "yes")
        } else {
          as.logical(is_control_raw)
        }
        is_control[is.na(is_control)] <- FALSE

        # Control-only mode: only compare against control group
        control_only <- isTRUE(parent_params$control_only)
        control_idx <- which(is_control)
        has_control <- length(control_idx) == 1

        if (length(groups) >= 2) {
          # Generate per-comparison children (one per pairwise comparison)
          for (gi in seq_len(length(groups) - 1)) {
            for (gj in (gi + 1):length(groups)) {
              # Skip non-control comparisons if control_only is enabled
              if (control_only && has_control) {
                if (!gi %in% control_idx && !gj %in% control_idx) {
                  next  # Skip comparison not involving control
                }
              }

              grp_a <- groups[gi]
              grp_b <- groups[gj]

              # Control orientation: control should always be the denominator/baseline (grp_a)
              # Fold change is calculated as B/A, so positive log2fc means higher in B than A
              # For ko1_vs_control, we want ko1/control, so control must be grp_a
              # Only swap when control is in gj position (needs to move to grp_a)
              if (has_control && gj %in% control_idx && !(gi %in% control_idx)) {
                # grp_b is control but should be grp_a (baseline) - swap needed
                tmp <- grp_a; grp_a <- grp_b; grp_b <- tmp
              }
              # If gi is control, grp_a is already control (baseline) - no swap needed

              comparison_label <- paste0(grp_b, "_vs_", grp_a)
              comparison_label_safe <- gsub("[^A-Za-z0-9_]", "_", comparison_label)

              # Generate children for each GO-ORA config
              for (cfg in goora_configs) {
                cfg_id <- cfg$config_id %||% "cfg_1"
                cfg_name <- cfg$name %||% "GO-ORA"
                cfg_id_safe <- gsub("[^A-Za-z0-9_]", "_", cfg_id)

                # Build per-config params (merge global + config-specific)
                config_params <- c(
                  global_params,
                  list(
                    fdr_cutoff = cfg$fdr_cutoff %||% 0.05,
                    min_overlap = cfg$min_overlap %||% 1
                  )
                )

                include_unique_in_sig <- isTRUE(cfg$include_unique_in_sig)

                # Child: config-specific comparison-specific sig_up
                # Naming: {config_id}__{comparison}_up
                child_up_id <- paste0(cfg_id_safe, "__", comparison_label_safe, "_up")
                children[[child_up_id]] <- list(
                  view_id = child_up_id,
                  label = sprintf("%s %s Up", cfg_name, comparison_label),
                  engine_id = paired_engine_id,
                  config_id = cfg_id,
                  config_name = cfg_name,
                  params = c(
                    config_params,
                    list(
                      direction = "up",
                      comparison = comparison_label,
                      source = sprintf("volcano:%s", step_entry$step_id),
                      apply_fdr = apply_fdr,
                      sig_threshold = sig_threshold,
                      fc_threshold = fc_threshold,
                      include_unique_quantified = include_unique_in_sig
                    )
                  ),
                  style = merge_style_defaults(paired_engine_id, paired_cfg$style)
                )

                # Child: config-specific comparison-specific sig_down
                child_down_id <- paste0(cfg_id_safe, "__", comparison_label_safe, "_down")
                children[[child_down_id]] <- list(
                  view_id = child_down_id,
                  label = sprintf("%s %s Down", cfg_name, comparison_label),
                  engine_id = paired_engine_id,
                  config_id = cfg_id,
                  config_name = cfg_name,
                  params = c(
                    config_params,
                    list(
                      direction = "down",
                      comparison = comparison_label,
                      source = sprintf("volcano:%s", step_entry$step_id),
                      apply_fdr = apply_fdr,
                      sig_threshold = sig_threshold,
                      fc_threshold = fc_threshold,
                      include_unique_quantified = include_unique_in_sig
                    )
                  ),
                  style = merge_style_defaults(paired_engine_id, paired_cfg$style)
                )
              }
            }
          }
        } else {
          # Fallback: single comparison (2 groups) - use combined sig_up/sig_down per config
          for (cfg in goora_configs) {
            cfg_id <- cfg$config_id %||% "cfg_1"
            cfg_name <- cfg$name %||% "GO-ORA"
            cfg_id_safe <- gsub("[^A-Za-z0-9_]", "_", cfg_id)

            config_params <- c(
              global_params,
              list(
                fdr_cutoff = cfg$fdr_cutoff %||% 0.05,
                min_overlap = cfg$min_overlap %||% 1
              )
            )

            include_unique_in_sig <- isTRUE(cfg$include_unique_in_sig)

            child_up_id <- paste0(cfg_id_safe, "__sig_up")
            children[[child_up_id]] <- list(
              view_id = child_up_id,
              label = sprintf("%s Up", cfg_name),
              engine_id = paired_engine_id,
              config_id = cfg_id,
              config_name = cfg_name,
              params = c(
                config_params,
                list(
                  direction = "up",
                  source = sprintf("volcano:%s", step_entry$step_id),
                  apply_fdr = apply_fdr,
                  sig_threshold = sig_threshold,
                  fc_threshold = fc_threshold,
                  include_unique_quantified = include_unique_in_sig
                )
              ),
              style = merge_style_defaults(paired_engine_id, paired_cfg$style)
            )

            child_down_id <- paste0(cfg_id_safe, "__sig_down")
            children[[child_down_id]] <- list(
              view_id = child_down_id,
              label = sprintf("%s Down", cfg_name),
              engine_id = paired_engine_id,
              config_id = cfg_id,
              config_name = cfg_name,
              params = c(
                config_params,
                list(
                  direction = "down",
                  source = sprintf("volcano:%s", step_entry$step_id),
                  apply_fdr = apply_fdr,
                  sig_threshold = sig_threshold,
                  fc_threshold = fc_threshold,
                  include_unique_quantified = include_unique_in_sig
                )
              ),
              style = merge_style_defaults(paired_engine_id, paired_cfg$style)
            )
          }
        }

        # Optional: Unique Quantified children (proteins present in all replicates of a group)
        # These are group-specific and computed at run time. When enabled, creates
        # unique_quantified_A and unique_quantified_B children for GO-ORA analysis
        # of proteins uniquely quantified in each comparison group.
        unique_quant_enabled <- isTRUE(paired_cfg$unique_quantified$enabled)

        if (unique_quant_enabled) {
          uq_params <- paired_cfg$unique_quantified$params %||% list()
          uq_style <- paired_cfg$unique_quantified$style %||% list()
          uq_engine_id <- paired_cfg$unique_quantified$engine_id %||% "goora"

          # Child: unique_quantified_A (proteins quantified in all replicates of group A only)
          children$unique_quantified_A <- list(
            view_id = "unique_quantified_A",
            label = "Unique Quantified (Group A)",
            engine_id = uq_engine_id,
            params = c(
              uq_params,
              list(
                direction = "unique_A",
                source = sprintf("volcano:%s", step_entry$step_id),
                # FROZEN - computed at run time, cannot be changed by viewer
                unique_quantified = TRUE,
                group_selection = "A"
              )
            ),
            style = merge_style_defaults(uq_engine_id, uq_style)
          )

          # Child: unique_quantified_B (proteins quantified in all replicates of group B only)
          children$unique_quantified_B <- list(
            view_id = "unique_quantified_B",
            label = "Unique Quantified (Group B)",
            engine_id = uq_engine_id,
            params = c(
              uq_params,
              list(
                direction = "unique_B",
                source = sprintf("volcano:%s", step_entry$step_id),
                # FROZEN - computed at run time, cannot be changed by viewer
                unique_quantified = TRUE,
                group_selection = "B"
              )
            ),
            style = merge_style_defaults(uq_engine_id, uq_style)
          )
        }

      } else if (eid == "pca") {
        # PCA ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ Paired analysis children (PC loadings)
        # Supports up to 3 PCs with method selection:
        #   - "1dgofcs": 1D GO-FCS on ranked loadings (all proteins)
        #   - "goora": GO-ORA on top/bottom N proteins by loading

        # Multi-engine format: paired$engines[[peid]] = config
        paired_engines <- paired_cfg$engines %||% list()

        # Determine which PCs to include (max 3, default PC1-PC2)
        max_pcs <- min(as.integer(paired_cfg$max_pcs %||% 2), 3)
        pc_list <- seq_len(max_pcs)

        # Get top_n for GO-ORA path (frozen at compile time)
        top_n <- as.integer(paired_cfg$top_n %||% parent_params$top_n %||% 50)

        # Method: 1D GO-FCS (ranked loadings, all proteins)
        if (isTRUE(paired_engines[["1dgofcs"]]$enabled)) {
          gofcs_params <- paired_engines[["1dgofcs"]]$params %||% list()
          gofcs_style <- paired_engines[["1dgofcs"]]$style %||% list()

          for (pc_idx in pc_list) {
            child_id <- sprintf("pc%d_gofcs", pc_idx)
            children[[child_id]] <- list(
              view_id = child_id,
              label = sprintf("PC%d GO-FCS", pc_idx),
              engine_id = "1dgofcs",
              params = c(
                gofcs_params,
                list(
                  ranking = sprintf("PC%d_loadings", pc_idx),
                  source = sprintf("pca:%s", step_entry$step_id)
                )
              ),
              style = merge_style_defaults("1dgofcs", gofcs_style)
            )
          }

        }

        # Method: 2D-GOFCS (PC1 vs PC2, etc.) - direct children of PCA
        if (isTRUE(paired_engines[["2dgofcs"]]$enabled) && max_pcs >= 2) {
          gofcs_2d_params <- paired_engines[["2dgofcs"]]$params %||% list()
          gofcs_2d_style <- paired_engines[["2dgofcs"]]$style %||% list()

          # Generate 2D combinations
          pc_pairs <- list()
          if (max_pcs >= 2) pc_pairs <- c(pc_pairs, list(c(1, 2)))
          if (max_pcs >= 3) pc_pairs <- c(pc_pairs, list(c(1, 3), c(2, 3)))

          for (pair in pc_pairs) {
            pc_x <- pair[1]
            pc_y <- pair[2]
            child_2d_id <- sprintf("pc%d_pc%d_2dgofcs", pc_x, pc_y)

            # Direct child of PCA (same level as 1dgofcs and goora)
            children[[child_2d_id]] <- list(
              view_id = child_2d_id,
              label = sprintf("PC%d vs PC%d 2D GO-FCS", pc_x, pc_y),
              engine_id = "2dgofcs",
              params = c(
                gofcs_2d_params,
                list(
                  x_ranking = sprintf("PC%d_loadings", pc_x),
                  y_ranking = sprintf("PC%d_loadings", pc_y),
                  source = sprintf("pca:%s", step_entry$step_id)
                )
              ),
              style = merge_style_defaults("2dgofcs", gofcs_2d_style)
            )
          }

        }

        # Method: GO-ORA (top/bottom N proteins by loading)
        if (isTRUE(paired_engines[["goora"]]$enabled)) {
          goora_params <- paired_engines[["goora"]]$params %||% list()
          goora_style <- paired_engines[["goora"]]$style %||% list()

          for (pc_idx in pc_list) {
            # Positive loadings (top N)
            child_id_pos <- sprintf("pc%d_top_goora", pc_idx)
            children[[child_id_pos]] <- list(
              view_id = child_id_pos,
              label = sprintf("PC%d Top %d (GO-ORA)", pc_idx, top_n),
              engine_id = "goora",
              params = c(
                goora_params,
                list(
                  direction = "top",
                  comparison = sprintf("PC%d", pc_idx),
                  pc = pc_idx,
                  top_n = top_n,  # FROZEN at compile time
                  source = sprintf("pca:%s", step_entry$step_id)
                )
              ),
              style = merge_style_defaults("goora", goora_style)
            )

            # Negative loadings (bottom N)
            child_id_neg <- sprintf("pc%d_bottom_goora", pc_idx)
            children[[child_id_neg]] <- list(
              view_id = child_id_neg,
              label = sprintf("PC%d Bottom %d (GO-ORA)", pc_idx, top_n),
              engine_id = "goora",
              params = c(
                goora_params,
                list(
                  direction = "bottom",
                  comparison = sprintf("PC%d", pc_idx),
                  pc = pc_idx,
                  top_n = top_n,  # FROZEN at compile time
                  source = sprintf("pca:%s", step_entry$step_id)
                )
              ),
              style = merge_style_defaults("goora", goora_style)
            )
          }

        }
      }

      if (length(children) > 0) {
        step_entry$paired_children <- children
      }
    }

    out_level <- apply_output_level(eng, current_level)
    step_entry$output_level <- out_level
    current_level <- out_level

    plan$steps[[length(plan$steps) + 1L]] <- step_entry
  }

  list(ok = TRUE, plan = plan, error = NULL)
}

#' Get run plan as a data.frame for preview display
#' @param plan Run plan from nr_compile_run_plan
#' @return data.frame
nr_plan_preview_df <- function(plan) {
  if (!is.list(plan$steps) || length(plan$steps) == 0) {
    return(data.frame(
      Step = integer(),
      Engine = character(),
      Label = character(),
      Enabled = logical(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    Step = vapply(plan$steps, function(s) s$step_index, integer(1)),
    Engine = vapply(plan$steps, function(s) s$engine_id, character(1)),
    Label = vapply(plan$steps, function(s) s$label, character(1)),
    Enabled = vapply(plan$steps, function(s) s$enabled, logical(1)),
    stringsAsFactors = FALSE
  )
}

# =========================================================
# Engine Input Builders (Context + Payload)
# =========================================================

#' Build canonical execution context from formatted data
#' @param formatted Formatted object from nr_load_formatted
#' @param terpbase Optional terpbase object
#' @return list(mat, samples, groups, id_cols, metadata, terpbase)
nr_build_context <- function(formatted, terpbase = NULL) {
  if (!isTRUE(formatted$ok)) {
    return(list(ok = FALSE, error = "Invalid formatted data"))
  }

  data <- formatted$data
  meta <- formatted$metadata

  # Get ID columns - ensure each is a single scalar string
  id_protein <- as.character(meta$id_protein_col %||% "")[1]
  id_gene <- as.character(meta$id_gene_col %||% "")[1]
  id_primary <- as.character(meta$id_primary_col %||% "")[1]

  id_cols <- c()
  if (nzchar(id_protein)) id_cols <- c(id_cols, id_protein)
  if (nzchar(id_gene)) id_cols <- c(id_cols, id_gene)
  if (nzchar(id_primary) && !id_primary %in% id_cols) {
    id_cols <- c(id_primary, id_cols)
  }
  id_cols <- unique(id_cols)

  # Get measurement columns from metadata
  col_meta <- meta$columns
  if (!is.data.frame(col_meta) || nrow(col_meta) == 0) {
    return(list(ok = FALSE, error = "No measurement columns defined in metadata"))
  }

  # Ensure data_col exists
  if (!"data_col" %in% names(col_meta)) {
    return(list(ok = FALSE, error = "Column metadata missing 'data_col' field"))
  }

  meas_cols <- as.character(col_meta$data_col)
  meas_cols <- meas_cols[!is.na(meas_cols) & nzchar(meas_cols)]
  meas_cols <- intersect(meas_cols, names(data))

  if (length(meas_cols) == 0) {
    return(list(ok = FALSE, error = "No valid measurement columns found in data"))
  }

  # Build samples table from columns metadata with safe matching
  col_data_col <- as.character(col_meta$data_col)
  match_idx <- match(meas_cols, col_data_col)

  # Safely extract group_name and replicate
  group_names <- if ("group_name" %in% names(col_meta)) {
    as.character(col_meta$group_name[match_idx])
  } else {
    rep(NA_character_, length(meas_cols))
  }

  replicates <- if ("replicate" %in% names(col_meta)) {
    as.integer(col_meta$replicate[match_idx])
  } else {
    rep(NA_integer_, length(meas_cols))
  }

  samples <- data.frame(
    sample_col = meas_cols,
    group_name = group_names,
    replicate = replicates,
    stringsAsFactors = FALSE
  )

  # Get unique groups
  groups <- unique(samples$group_name)
  groups <- groups[!is.na(groups) & nzchar(groups)]

  # Build matrix (IDs x samples)
  mat <- as.matrix(data[, meas_cols, drop = FALSE])

  # Set rownames from primary ID column if available
  if (nzchar(id_primary) && id_primary %in% names(data)) {
    rownames(mat) <- as.character(data[[id_primary]])
  } else {
    rownames(mat) <- as.character(seq_len(nrow(mat)))
  }

  # ID mapping table
  valid_id_cols <- intersect(id_cols, names(data))
  ids <- if (length(valid_id_cols) > 0) {
    data[, valid_id_cols, drop = FALSE]
  } else {
    data.frame(row_id = seq_len(nrow(data)), stringsAsFactors = FALSE)
  }

  # Load contaminants from contaminants.txt if it exists
  contaminants <- character(0)
  contaminants_file <- "contaminants.txt"
  if (file.exists(contaminants_file)) {
    tryCatch({
      lines <- readLines(contaminants_file, warn = FALSE)
      lines <- trimws(lines)
      contaminants <- lines[nzchar(lines) & !grepl("^#", lines)]  # Skip empty and comment lines
      message(sprintf("[nr_build_context] Loaded %d contaminants from %s", length(contaminants), contaminants_file))
    }, error = function(e) {
      message(sprintf("[nr_build_context] Failed to load contaminants: %s", e$message))
    })
  }

  # Extract and augment terpbase with engine-friendly mappings
  terpbase_obj <- if (isTRUE(terpbase$ok)) terpbase$terpbase else NULL

  if (!is.null(terpbase_obj)) {
    # Build protein_to_go mapping from annot_long if not already present
    if (is.null(terpbase_obj$protein_to_go) && !is.null(terpbase_obj$annot_long)) {
      annot <- terpbase_obj$annot_long
      if ("gene" %in% names(annot) && "ID" %in% names(annot)) {
        # Group GO IDs by gene/protein
        genes <- unique(annot$gene)
        protein_to_go <- lapply(genes, function(g) {
          unique(annot$ID[annot$gene == g])
        })
        names(protein_to_go) <- genes
        terpbase_obj$protein_to_go <- protein_to_go
      }
    }

    # Build go_terms table from terms_by_id if not already present
    if (is.null(terpbase_obj$go_terms) && !is.null(terpbase_obj$terms_by_id)) {
      terms <- terpbase_obj$terms_by_id
      if (all(c("ID", "Description", "ONTOLOGY") %in% names(terms))) {
        terpbase_obj$go_terms <- data.frame(
          term_id = terms$ID,
          go_id = terms$ID,
          term_name = terms$Description,
          name = terms$Description,
          ontology = terms$ONTOLOGY,
          namespace = terms$ONTOLOGY,
          stringsAsFactors = FALSE
        )
      }
    }

    # Build protein_to_loc mapping from gene_meta if not already present
    if (is.null(terpbase_obj$protein_to_loc) && !is.null(terpbase_obj$gene_meta)) {
      gene_meta <- terpbase_obj$gene_meta
      if ("entry" %in% names(gene_meta) && "subcell_location" %in% names(gene_meta)) {
        terpbase_obj$protein_to_loc <- data.frame(
          protein_id = gene_meta$entry,
          subcellular_location = gene_meta$subcell_location,
          stringsAsFactors = FALSE
        )
      }
    }

    # Build background universe from gene_meta if not already present
    if (is.null(terpbase_obj$background) && !is.null(terpbase_obj$gene_meta)) {
      gene_meta <- terpbase_obj$gene_meta
      if ("entry" %in% names(gene_meta) || "gene_symbol" %in% names(gene_meta)) {
        bg <- c(
          if ("entry" %in% names(gene_meta)) as.character(gene_meta$entry) else character(),
          if ("gene_symbol" %in% names(gene_meta)) as.character(gene_meta$gene_symbol) else character()
        )
        bg <- bg[!is.na(bg) & nzchar(bg)]
        terpbase_obj$background <- unique(bg)
      }
    }
  }

  list(
    ok = TRUE,
    mat = mat,
    ids = ids,
    samples = samples,
    groups = groups,
    n_groups = length(groups),
    id_cols = id_cols,
    meas_cols = meas_cols,
    metadata = meta,
    terpbase = terpbase_obj,
    contaminants = contaminants
  )
}

#' Build payload for a specific engine step
#'
#' Creates the canonical input payload for engine execution. All engines receive
#' the same payload shape; they access the fields they need.
#'
#' @param ctx Context from nr_build_context
#' @param step Step definition from plan
#' @param registry Engine registry
#'
#' @return A list with the following structure (ENGINE INPUT PAYLOAD CONTRACT):
#'
#' ## Control fields:
#' - `ok` (logical): TRUE if payload is valid, FALSE if construction failed
#' - `error` (character): Error message if ok=FALSE
#' - `engine_id` (character): Lowercase engine identifier
#' - `step_id` (character): Unique step identifier from plan
#' - `step_index` (integer): 1-based step position
#' - `label` (character): Display label for the step
#'
#' ## Compute parameters:
#' - `params` (list): Engine-specific compute parameters from .terpflow
#' - `style` (list): Engine-specific style parameters (for results viewer)
#'
#' ## Data context (protein-level matrix):
#' - `mat` (matrix): Numeric matrix with rows=protein IDs, cols=sample columns
#'   - rownames: protein ID strings
#'   - colnames: measurement column names matching samples$sample_col
#' - `ids` (data.frame): ID mapping table with columns:
#'   - protein_id, gene_id (if available), row_id (fallback)
#' - `samples` (data.frame): Sample metadata with columns:
#'   - sample_col: column name in mat
#'   - group_name: group assignment
#'   - replicate: replicate number within group
#' - `groups` (character): Vector of unique group names
#' - `n_groups` (integer): Number of groups
#'
#' ## Metadata:
#' - `metadata` (list): Full metadata from formatted file including:
#'   - analysis_level: "protein" (normalized lowercase)
#'   - id_primary_col, id_protein_col, id_gene_col
#'   - groups (data.frame), columns (data.frame)
#'
#' ## Terpbase (optional):
#' - `terpbase` (list or NULL): Loaded .terpbase object if provided
#' - `has_terpbase` (logical): TRUE if terpbase is available
#'
nr_build_step_payload <- function(ctx, step, registry = NULL) {
  if (!isTRUE(ctx$ok)) {
    return(list(ok = FALSE, error = "Invalid context"))
  }

  eid <- step$engine_id
  params <- step$params %||% list()
  style <- step$style %||% list()

  # Get engine requirements
  eng <- NULL
  if (!is.null(registry)) {
    eng <- registry$engines[[eid]]
  }

  payload <- list(
    ok = TRUE,
    engine_id = eid,
    step_id = step$step_id,
    step_index = step$step_index,
    label = step$label,
    params = params,
    style = style,

    # Data context
    mat = ctx$mat,
    ids = ctx$ids,
    samples = ctx$samples,
    groups = ctx$groups,
    n_groups = ctx$n_groups,
    metadata = ctx$metadata,

    # Terpbase (if required)
    terpbase = ctx$terpbase,
    has_terpbase = !is.null(ctx$terpbase),

    # Contaminants list (for dataprocessor)
    contaminants = ctx$contaminants %||% character(0)
  )

  # Validate terpbase requirement
  if (!is.null(eng)) {
    req <- eng$requirements %||% list()
    if (isTRUE(req$requires_terpbase) && is.null(ctx$terpbase)) {
      payload$ok <- FALSE
      payload$error <- sprintf("Engine '%s' requires terpbase", eid)
    }
  }

  payload
}

# =========================================================
# Engine Dispatcher
# =========================================================
# NOTE: All engine implementations have been moved to R/engines/stats/
# Each engine exports stats_<engine>_run(payload, params, context)

#' Canonical engine IDs
#'
#' TODO: These validation lists should be derived programmatically from
#' msterp_engine_registry() to prevent drift between registry and validation.
#'
#' @return Character vector of valid engine IDs
nr_engine_ids <- function() {
  c("dataprocessor", "idquant", "spearman", "hor_dis", "vert_dis",
    "pca", "volcano", "rankplot", "goora", "1dgofcs", "2dgofcs", "subloc", "heatmap", "ftest_heatmap",
    "peptide_aggregate_to_protein",
    "idquant_id_quant", "idquant_average_value",
    "idquant_group", "idquant_replicate", "idquant_cv_scatter", "idquant_cv_bar",
    "idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified")
}

#' Get list of fully implemented engines (not stubs)
#'
#' Returns engine IDs that have real implementations (not placeholder stubs).
#' Used for pre-run validation to warn or fail early when unimplemented engines
#' are requested.
#'
#' @return Character vector of implemented engine IDs
nr_implemented_engines <- function() {
  # All engines in this list have implementations in R/engines/stats/
  c("dataprocessor", "idquant", "spearman", "hor_dis", "vert_dis",
    "pca", "volcano", "rankplot", "goora", "1dgofcs", "2dgofcs", "subloc", "heatmap", "ftest_heatmap",
    "peptide_aggregate_to_protein",
    "idquant_id_quant", "idquant_average_value",
    "idquant_group", "idquant_replicate", "idquant_cv_scatter", "idquant_cv_bar",
    "idquant_overlap", "idquant_overlap_detected", "idquant_overlap_quantified")
}

#' Check if an engine is implemented (not a stub)
#'
#' @param engine_id Engine identifier
#' @return logical TRUE if implemented, FALSE if stub or unknown
nr_is_engine_implemented <- function(engine_id) {
  engine_id %in% nr_implemented_engines()
}

#' Validate run plan engines before execution
#'
#' Checks all engines in a run plan and returns information about which are

#' implemented vs stubs. Use `fail_on_stubs = TRUE` to enforce early failure
#' when unimplemented engines are present.
#'
#' @param plan Run plan from nr_compile_run_plan
#' @param fail_on_stubs If TRUE, returns error when stub engines are present
#' @return list(ok, implemented, stubs, unknown, error)
nr_validate_plan_engines <- function(plan, fail_on_stubs = FALSE) {
  if (!is.list(plan$steps) || length(plan$steps) == 0) {
    return(list(ok = FALSE, error = "Plan has no steps"))
  }

  all_engine_ids <- nr_engine_ids()
  implemented <- nr_implemented_engines()

  collect_engines <- function(steps) {
    out <- character()
    for (s in (steps %||% list())) {
      if (!is.list(s)) next
      is_container <- identical(tolower(s$type %||% "engine"), "container")
      if (!is_container) {
        out <- c(out, as.character(s$engine_id %||% ""))
      }
      if (is_container && is.list(s$substeps)) {
        out <- c(out, collect_engines(s$substeps))
      }
      if (is.list(s$paired_children) && length(s$paired_children) > 0) {
        pc <- unname(s$paired_children)
        if (is.list(pc)) {
          out <- c(out, vapply(pc, function(x) as.character((x %||% list())$engine_id %||% ""), character(1)))
        }
      }
    }
    out
  }

  plan_engines <- unique(collect_engines(plan$steps))
  plan_engines <- plan_engines[nzchar(plan_engines)]

  # Categorize engines
  unknown_engines <- setdiff(plan_engines, all_engine_ids)
  known_engines <- intersect(plan_engines, all_engine_ids)
  stub_engines <- setdiff(known_engines, implemented)
  impl_engines <- intersect(known_engines, implemented)

  # Build result
  result <- list(
    ok = TRUE,
    implemented = impl_engines,
    stubs = stub_engines,
    unknown = unknown_engines,
    error = NULL
  )

  # Fail on unknown engines (always)
  if (length(unknown_engines) > 0) {
    result$ok <- FALSE
    result$error <- sprintf(
      "Unknown engines in plan: %s",
      paste(unknown_engines, collapse = ", ")
    )
    return(result)
  }

  # Optionally fail on stubs
  if (fail_on_stubs && length(stub_engines) > 0) {
    result$ok <- FALSE
    result$error <- sprintf(
      "Unimplemented engines in plan (stubs only): %s. Implemented engines: %s",
      paste(stub_engines, collapse = ", "),
      paste(implemented, collapse = ", ")
    )
    return(result)
  }

  result
}

#' Single dispatcher for all engines
#'
#' Calls the appropriate engine function and validates the output envelope.
#' Missing engines fail with a clear error message.
#'
#' @param engine_id Engine identifier (lowercase)
#' @param payload Payload from nr_build_step_payload
#' @param context Optional context from nr_build_context (provides contaminants, terpbase, etc.)
#' @return Contract-compliant results object with engine_id, params, data
nr_run_engine <- function(engine_id, payload, context = NULL) {
  if (!isTRUE(payload$ok)) {
    return(nr_engine_error_result(engine_id, payload$error %||% "Invalid payload"))
  }

  # Normalize context to consistent structure
  # When context is the full ctx from nr_build_context, extract only the needed fields
  # to avoid passing raw data structures that could cause issues
  if (is.null(context) || !is.list(context)) {
    context <- list(
      contaminants = payload$contaminants %||% character(0),
      terpbase = payload$terpbase
    )
  } else {
    # Ensure context has the expected structure
    context <- list(
      contaminants = context$contaminants %||% character(0),
      terpbase = context$terpbase
    )
  }

  # Dispatch to stats engine implementations in R/engines/stats/
  # All engines now use the standardized stats_<engine>_run(payload, params, context) API
  result <- switch(
    engine_id,
    "dataprocessor" = stats_dataprocessor_run(payload, payload$params, context),
    "idquant"       = stats_idquant_run(payload, payload$params, context),
    "idquant_id_quant" = stats_idquant_id_quant_run(payload, payload$params, context),
    "idquant_average_value" = stats_idquant_average_value_run(payload, payload$params, context),
    "idquant_cv_scatter" = stats_idquant_cv_scatter_run(payload, payload$params, context),
    "idquant_cv_bar" = stats_idquant_cv_bar_run(payload, payload$params, context),
    "idquant_overlap" = stats_idquant_overlap_run(payload, payload$params, context),
    "idquant_overlap_detected" = stats_idquant_overlap_detected_run(payload, payload$params, context),
    "idquant_overlap_quantified" = stats_idquant_overlap_quantified_run(payload, payload$params, context),
    "spearman"      = stats_spearman_run(payload, payload$params, context),
    "hor_dis"       = stats_hor_dis_run(payload, payload$params, context),
    "vert_dis"      = stats_vert_dis_run(payload, payload$params, context),
    "pca"           = stats_pca_run(payload, payload$params, context),
    "volcano"       = stats_volcano_run(payload, payload$params, context),
    "rankplot"      = stats_rankplot_run(payload, payload$params, context),
    "goora"         = stats_goora_run(payload, payload$params, context),
    "1dgofcs"       = stats_1dgofcs_run(payload, payload$params, context),
    "2dgofcs"       = stats_2dgofcs_run(payload, payload$params, context),
    "subloc"        = stats_subloc_run(payload, payload$params, context),
    "heatmap"       = stats_heatmap_run(payload, payload$params, context),
    "ftest_heatmap" = stats_ftest_heatmap_run(payload, payload$params, context),
    "peptide_aggregate_to_protein" = stats_peptide_aggregate_to_protein_run(payload, payload$params, context),
    # Unknown engine
    nr_engine_error_result(engine_id, sprintf("Engine '%s' not implemented", engine_id))
  )

  # Validate output envelope
  validation <- nr_validate_engine_output(result, engine_id)
  if (!isTRUE(validation$valid)) {
    nr_log_message(sprintf("Engine '%s' output validation warning: %s",
                           engine_id, validation$message))
  }

  result
}

#' Validate engine output envelope
#' @param result Engine result object
#' @param engine_id Expected engine ID
#' @return list(valid, message)
nr_validate_engine_output <- function(result, engine_id) {
  issues <- character()

  if (!is.list(result)) {
    return(list(valid = FALSE, message = "Result must be a list"))
  }

  # Required: engine_id
  if (is.null(result$engine_id)) {
    issues <- c(issues, "missing 'engine_id'")
  } else if (result$engine_id != engine_id) {
    issues <- c(issues, sprintf("engine_id mismatch: expected '%s', got '%s'",
                                engine_id, result$engine_id))
  }

  # Required: data
  if (is.null(result$data)) {
    issues <- c(issues, "missing 'data' field")
  }

  # Recommended: params (warning only)
  if (is.null(result$params)) {
    issues <- c(issues, "missing 'params' field (recommended)")
  }

  if (length(issues) > 0) {
    return(list(valid = FALSE, message = paste(issues, collapse = "; ")))
  }

  list(valid = TRUE, message = "OK")
}

#' Create error result object
#' @param engine_id Engine ID
#' @param error_msg Error message
#' @return Contract-compliant error result
nr_engine_error_result <- function(engine_id, error_msg) {
  list(
    engine_id = engine_id,
    params = list(),
    status = "error",
    data = list(
      error = error_msg,
      log = data.frame(
        time = format(Sys.time()),
        level = "ERROR",
        message = error_msg,
        stringsAsFactors = FALSE
      )
    )
  )
}

#' Log message helper (for engine output issues)
#' @param msg Message to log
nr_log_message <- function(msg) {
  message("[nr_engine] ", msg)
}

# =========================================================
# Terpbook Writer (Contract-Compliant)
# =========================================================

#' Create a new terpbook run directory
#' @param out_dir Parent directory for output
#' @param run_name Name for the run
#' @return Path to run root directory
nr_create_terpbook <- function(out_dir, run_name = NULL) {
  if (is.null(run_name) || !nzchar(run_name)) {
    run_name <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  # Sanitize
  run_name <- gsub("[^A-Za-z0-9._-]+", "_", run_name)

  run_root <- file.path(out_dir, paste0(run_name, ".terpbook"))

  # If directory already exists (e.g., created by UI for progress polling),
  # just use it rather than creating a different one. This ensures the UI
  # and the runner use the same log.txt path.
  if (!dir.exists(run_root)) {
    dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
  }

  # Initialize empty manifest
  manifest <- list(steps = list())
  nr_write_json(manifest, file.path(run_root, "manifest.json"))

  # Initialize log
  nr_log(run_root, sprintf("Created terpbook: %s", run_root))

  run_root
}

#' Write JSON file
#' @param x Object to write
#' @param path File path
nr_write_json <- function(x, path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

#' Read JSON file
#' @param path File path
nr_read_json <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required")
  }
  # Use simplifyVector = FALSE to preserve list structure for arrays
  # This prevents issues when appending to manifest$steps
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' Append to log file
#' @param run_root Run root directory
#' @param msg Message to log
#' @param level Log level (INFO, WARN, ERROR) - not displayed in output
nr_log <- function(run_root, msg, level = "INFO") {
  log_path <- file.path(run_root, "log.txt")
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

#' Append engine log entries to main log file
#' @param run_root Run root directory
#' @param engine_id Engine identifier (for prefixing log entries)
#' @param results Engine results object (expected to have data$log data.frame)
nr_append_engine_log <- function(run_root, engine_id, results) {
  if (is.null(results) || is.null(results$data) || is.null(results$data$log)) {
    return(invisible(NULL))
  }

  engine_log <- results$data$log
  if (!is.data.frame(engine_log) || nrow(engine_log) == 0) {
    return(invisible(NULL))
  }

  log_path <- file.path(run_root, "log.txt")

  # Extract log entries and write to main log with engine prefix
  for (i in seq_len(nrow(engine_log))) {
    entry <- engine_log[i, , drop = FALSE]
    msg <- as.character(entry$message %||% "")
    # Use time-only format for engine log entries (main log already has full date on first line)
    time_str <- format(Sys.time(), "%H:%M:%S")

    # Format: [time] [engine_id] message
    line <- sprintf("[%s] [%s] %s\n", time_str, engine_id, msg)
    cat(line, file = log_path, append = TRUE)
  }

  invisible(NULL)
}

#' Write a child view (recursive helper)
#'
#' Writes a single view node to disk, then recursively writes any child views.
#' Contract:
#'   - view.json: engine_id, label, meta (optional views[] for children)
#'   - results.rds: engine output with `data` field
#'
#' @param parent_dir Absolute path to parent directory (step or parent view)
#' @param view_id Unique view ID (used in path and view.json)
#' @param engine_id Engine ID for this view
#' @param label Display label
#' @param results Results object (must have `data` field)
#' @param style Optional style defaults
#' @param views Optional list of child views (each with view_id, engine_id, label, results, ...)
#' @param run_root Run root for logging
#' @return Relative path to view directory (e.g., "views/sig_up")
nr_write_view <- function(parent_dir, view_id, engine_id, label, results,
                          style = NULL, views = NULL, meta = NULL, run_root = NULL) {
  # Create view directory: parent_dir/views/<view_id>
  view_rel <- file.path("views", view_id)
  view_path <- file.path(parent_dir, view_rel)
  dir.create(view_path, recursive = TRUE, showWarnings = FALSE)

  # Write results.rds
  saveRDS(results, file.path(view_path, "results.rds"))

  # Build view descriptor
  view_desc <- list(
    engine_id = tolower(engine_id),
    label = as.character(label %||% view_id),
    meta = list(
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  if (is.list(meta) && length(meta) > 0) {
    view_desc$meta <- modifyList(view_desc$meta, meta)
  }
  if (!is.null(style) && length(style) > 0) {
    view_desc$style <- style
  }

  # Process child views (recursive)
  if (!is.null(views) && length(views) > 0) {
    child_refs <- list()
    for (cv in views) {
      cv_id <- cv$view_id %||% cv$id %||% ""
      if (!nzchar(cv_id)) next

      cv_path <- nr_write_view(
        parent_dir = view_path,
        view_id = cv_id,
        engine_id = cv$engine_id %||% engine_id,
        label = cv$label %||% cv_id,
        results = cv$results,
        style = cv$style,
        views = cv$views,  # nested grandchildren
        meta = cv$meta,
        run_root = run_root
      )

      # Add reference to parent's views array
      child_refs <- c(child_refs, list(list(
        view_id = cv_id,
        label = cv$label %||% cv_id,
        path = cv_path,
        engine_id = tolower(cv$engine_id %||% engine_id)
      )))
    }
    if (length(child_refs) > 0) {
      view_desc$views <- child_refs
    }
  }

  # Write view.json
  nr_write_json(view_desc, file.path(view_path, "view.json"))

  if (!is.null(run_root)) {
    nr_log(run_root, sprintf("Wrote view: %s (%s)", view_id, engine_id))
  }

  view_rel
}

#' Write step results to terpbook
#'
#' Writes step to disk with optional nested child views.
#' Contract:
#'   - step_###/step.json: engine_id, label, meta, optional views[]
#'   - step_###/results.rds: engine output with `data` field
#'   - Child views written to step_###/views/<view_id>/
#'
#' @param run_root Run root directory
#' @param step_index Step index (1-based)
#' @param step_id Step ID
#' @param engine_id Engine ID
#' @param results Results object to save
#' @param style Optional style object
#' @param views Optional list of child views. Each view should have:
#'   - view_id: unique identifier
#'   - engine_id: engine for this view (inherits parent if omitted)
#'   - label: display label
#'   - results: results object
#'   - style: optional style
#'   - views: optional nested children (grandchildren)
#' @return Step directory relative path (e.g., "step_001")
nr_write_step <- function(run_root, step_index, step_id, engine_id, results,
                          style = NULL, views = NULL) {
  # Create step directory
  step_dir <- sprintf("step_%03d", as.integer(step_index))
  step_path <- file.path(run_root, step_dir)
  dir.create(step_path, recursive = TRUE, showWarnings = FALSE)

  # Write results.rds (contract requirement)
  saveRDS(results, file.path(step_path, "results.rds"))

  # Write step.json descriptor
  step_desc <- list(
    engine_id = tolower(engine_id),
    label = step_id,
    meta = list(
      step_index = as.integer(step_index),
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  if (!is.null(style) && length(style) > 0) {
    step_desc$style <- style
  }

  # Process child views
  if (!is.null(views) && length(views) > 0) {
    view_refs <- list()
    for (v in views) {
      v_id <- v$view_id %||% v$id %||% ""
      if (!nzchar(v_id)) next

      v_path <- nr_write_view(
        parent_dir = step_path,
        view_id = v_id,
        engine_id = v$engine_id %||% engine_id,
        label = v$label %||% v_id,
        results = v$results,
        style = v$style,
        views = v$views,  # nested grandchildren
        meta = v$meta,
        run_root = run_root
      )

      # Add reference to step's views array
      view_refs <- c(view_refs, list(list(
        view_id = v_id,
        label = v$label %||% v_id,
        path = v_path,
        engine_id = tolower(v$engine_id %||% engine_id)
      )))
    }
    if (length(view_refs) > 0) {
      step_desc$views <- view_refs
    }
  }

  nr_write_json(step_desc, file.path(step_path, "step.json"))

  # Update manifest
  manifest_path <- file.path(run_root, "manifest.json")
  manifest <- nr_read_json(manifest_path)
  manifest$steps <- c(manifest$steps, list(list(
    step_index = as.integer(step_index),
    step_id = step_id,
    engine_id = tolower(engine_id),
    step_dir = step_dir
  )))
  nr_write_json(manifest, manifest_path)

  nr_log(run_root, sprintf("Wrote step %d: %s (%s)%s",
    step_index, engine_id, step_id,
    if (!is.null(views) && length(views) > 0) sprintf(" with %d views", length(views)) else ""
  ))

  step_dir
}

#' Finalize terpbook (ready for zipping)
#' @param run_root Run root directory
#' @param status Final status ("done" or "error")
nr_finalize_terpbook <- function(run_root, status = "done") {
  nr_log(run_root, sprintf("Terpbook finalized with status: %s", status))
}

#' Zip terpbook directory to .terpbook file
#' @param run_root Run root directory
#' @param output_path Optional output path (defaults to run_root + .zip)
#' @return Path to zip file
nr_zip_terpbook <- function(run_root, output_path = NULL) {
  if (is.null(output_path)) {
    output_path <- paste0(run_root, ".zip")
  }

  old_wd <- setwd(dirname(run_root))
  on.exit(setwd(old_wd), add = TRUE)

  base_name <- basename(run_root)

  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = output_path, files = base_name)
  } else {
    utils::zip(zipfile = output_path, files = base_name, flags = "-r9X")
  }

  output_path
}

# =========================================================
# Paired Children Execution
# =========================================================

#' Execute paired child views for a parent step
#'
#' Called when a parent engine (volcano/pca) has paired_children defined.
#' Uses the parent's frozen sets (from results$data$sets) to build child payloads.
#'
#' INVARIANTS:
#' - Child query_proteins come from parent frozen sets (not recomputed)
#' - Thresholds (fc_threshold, sig_threshold) are frozen at compile time
#' - Child engines receive protein IDs, not the full matrix
#'
#' @param step Parent step definition with paired_children
#' @param parent_results Results from parent engine execution
#' @param ctx Execution context
#' @param run_root Run root for logging
#' @return List of child view objects for nr_write_step
nr_execute_paired_children <- function(step, parent_results, ctx, run_root) {
  if (is.null(step$paired_children) || length(step$paired_children) == 0) {
    return(NULL)
  }

  parent_engine <- step$engine_id
  # Include both combined sets and per-comparison data for volcano children
  parent_sets <- parent_results$data$sets %||% list()
  parent_sets$comparisons <- parent_results$data$comparisons %||% NULL
  parent_loadings <- parent_results$data$loadings %||% NULL

  child_views <- list()

  child_defs <- step$paired_children

  for (child_id in names(child_defs)) {
    child_def <- child_defs[[child_id]]
    child_engine <- child_def$engine_id %||% "goora"

    nr_log(run_root, sprintf("    Child: %s (%s)", child_id, child_engine))

    # Build child payload based on parent type and child direction
    child_payload <- nr_build_child_payload(
      child_def = child_def,
      parent_engine = parent_engine,
      parent_sets = parent_sets,
      parent_loadings = parent_loadings,
      ctx = ctx
    )

    if (!isTRUE(child_payload$ok)) {
      nr_log(run_root, sprintf("      WARN: %s", child_payload$error %||% "Invalid child payload"))
      # Create error result for child
      child_results <- list(
        engine_id = child_engine,
        params = child_def$params %||% list(),
        data = list(
          log = data.frame(
            time = format(Sys.time()),
            level = "ERROR",
            message = child_payload$error %||% "Failed to build child payload",
            stringsAsFactors = FALSE
          )
        )
      )
    } else {
      # Execute child engine
      child_results <- nr_run_engine(child_engine, child_payload, ctx)
    }

    # Append child engine log entries to main run log
    nr_append_engine_log(run_root, sprintf("%s/%s", child_engine, child_id), child_results)


    # Process grandchildren recursively (for PCAÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢1DGOFCSÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢2DGOFCS paths)
    grandchild_views <- NULL
    if (!is.null(child_def$children) && length(child_def$children) > 0) {
      grandchild_views <- nr_execute_grandchildren(
        child_def = child_def,
        child_results = child_results,
        parent_loadings = parent_loadings,
        ctx = ctx,
        run_root = run_root
      )
    }

    # Build child view object
    child_views[[child_id]] <- list(
      view_id = child_id,
      label = child_def$label %||% child_id,
      engine_id = child_engine,
      results = child_results,
      style = child_def$style %||% list(),
      views = grandchild_views
    )
  }

  if (length(child_views) == 0) return(NULL)
  child_views
}

#' Build payload for a paired child engine
#'
#' Extracts query_proteins from parent frozen sets for GO-ORA,
#' or scores vector from parent loadings for GO-FCS.
#'
#' @param child_def Child view definition from plan
#' @param parent_engine Parent engine ID (volcano/pca)
#' @param parent_sets Parent frozen sets (sig_up, sig_down, etc.)
#' @param parent_loadings Parent PCA loadings (for GOFCS)
#' @param ctx Execution context
#' @return Child payload with ok, query_proteins, scores, etc.
nr_build_child_payload <- function(child_def, parent_engine, parent_sets,
                                   parent_loadings, ctx) {
  child_engine <- child_def$engine_id %||% "goora"
  child_params <- child_def$params %||% list()
  direction <- child_params$direction %||% ""

  # Build base payload structure
  payload <- list(
    ok = TRUE,
    engine_id = child_engine,
    params = child_params,
    terpbase = ctx$terpbase,
    has_terpbase = !is.null(ctx$terpbase)
  )

  # Prefer gene symbols as GO identifiers when possible.
  # Many terpbase objects include both UniProt Entry IDs and gene symbols as keys in
  # protein_to_go; keeping background inclusive enables either input.
  gene_map <- NULL
  if (!is.null(ctx$ids) && is.data.frame(ctx$ids) && !is.null(ctx$metadata)) {
    prot_col <- as.character(ctx$metadata$id_protein_col %||% "")[1]
    gene_col <- as.character(ctx$metadata$id_gene_col %||% "")[1]
    if (nzchar(prot_col) && nzchar(gene_col) &&
        prot_col %in% names(ctx$ids) && gene_col %in% names(ctx$ids)) {
      prot_vals <- as.character(ctx$ids[[prot_col]])
      gene_vals <- as.character(ctx$ids[[gene_col]])
      ok <- !is.na(prot_vals) & nzchar(prot_vals)
      prot_vals <- prot_vals[ok]
      gene_vals <- gene_vals[ok]
      gene_map <- tapply(gene_vals, prot_vals, function(v) {
        v <- as.character(v)
        v <- v[!is.na(v) & nzchar(v)]
        if (length(v) > 0) v[[1]] else NA_character_
      })
      gene_map <- unlist(gene_map, use.names = TRUE)
      gene_map <- stats::setNames(as.character(gene_map), names(gene_map))
    }
  }

  map_ids_to_gene_symbols <- function(ids_vec) {
    ids_vec <- as.character(ids_vec %||% character())
    if (length(ids_vec) == 0) return(character())
    if (is.null(gene_map) || length(gene_map) == 0) return(ids_vec)

    mapped <- unname(gene_map[ids_vec])
    out <- ids_vec
    ok <- !is.na(mapped) & nzchar(mapped)
    out[ok] <- mapped[ok]
    out
  }

  rename_scores_to_gene_symbols <- function(scores) {
    if (is.null(scores) || length(scores) == 0) return(scores)
    nms <- names(scores)
    if (is.null(nms)) return(scores)

    names(scores) <- map_ids_to_gene_symbols(nms)

    # Collapse duplicates deterministically: keep the largest |score| per gene symbol.
    if (anyDuplicated(names(scores))) {
      ord <- order(abs(as.numeric(scores)), decreasing = TRUE)
      scores <- scores[ord]
      scores <- scores[!duplicated(names(scores))]
    }

    scores
  }

  # Volcano ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ GO-ORA: Use frozen protein sets (per-comparison or combined)
  if (parent_engine == "volcano" && child_engine == "goora") {
    # Check if this is a per-comparison child
    comparison <- child_params$comparison %||% NULL
    query_proteins <- character()

    if (!is.null(comparison) && !is.null(parent_sets$comparisons)) {
      # Per-comparison mode: extract from specific comparison's sets
      comp_data <- parent_sets$comparisons[[comparison]]
      if (is.null(comp_data)) {
        return(list(
          ok = FALSE,
          error = sprintf("Parent volcano missing comparison: %s", comparison)
        ))
      }

      # Get sig_up or sig_down from this specific comparison
      if (direction == "up") {
        query_proteins <- comp_data$sets$sig_up %||% character()
      } else if (direction == "down") {
        query_proteins <- comp_data$sets$sig_down %||% character()
      }
    } else {
      # Combined mode: use combined sets across all comparisons
      set_key <- switch(
        direction,
        "up" = "sig_up",
        "down" = "sig_down",
        "unique_A" = "unique_quantified_A",
        "unique_B" = "unique_quantified_B",
        NULL
      )

      if (!is.null(set_key) && !is.null(parent_sets[[set_key]])) {
        query_proteins <- parent_sets[[set_key]]
      }
    }

    # Option: Include uniquely quantified proteins in input list (union with sig set)
    # This is enabled via include_unique_quantified param on sig_up/sig_down children
    if (isTRUE(child_params$include_unique_quantified) && direction %in% c("up", "down")) {
      # For "up" direction, include unique_quantified_B (proteins unique to the "higher" group)
      # For "down" direction, include unique_quantified_A (proteins unique to the "lower" group)
      unique_set_key <- if (direction == "up") "unique_quantified_B" else "unique_quantified_A"

      if (!is.null(parent_sets[[unique_set_key]])) {
        unique_proteins <- parent_sets[[unique_set_key]]
        query_proteins <- unique(c(query_proteins, unique_proteins))
      }
    }

    if (length(query_proteins) == 0) {
      return(list(
        ok = FALSE,
        error = sprintf("No significant proteins for direction '%s'%s",
                        direction, if (!is.null(comparison)) paste0(" in ", comparison) else "")
      ))
    }

    query_proteins <- map_ids_to_gene_symbols(query_proteins)
    query_proteins <- query_proteins[!is.na(query_proteins) & nzchar(query_proteins)]
    query_proteins <- query_proteins[!duplicated(query_proteins)]

    payload$query_proteins <- query_proteins
    payload$n_query <- length(query_proteins)
  }

  # PCA ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ 1D-GOFCS: Use PC loadings as scores
  else if (parent_engine == "pca" && child_engine == "1dgofcs") {
    ranking <- child_params$ranking %||% "PC1_loadings"
    pc_num <- as.integer(gsub("^PC(\\d+).*", "\\1", ranking))

    if (is.null(parent_loadings)) {
      return(list(ok = FALSE, error = "Parent PCA missing loadings"))
    }

    # Extract loadings for the specified PC
    if (is.matrix(parent_loadings) && ncol(parent_loadings) >= pc_num) {
      scores <- parent_loadings[, pc_num]
      names(scores) <- rownames(parent_loadings)
    } else if (is.data.frame(parent_loadings)) {
      pc_col <- paste0("PC", pc_num)
      if (pc_col %in% names(parent_loadings)) {
        id_col <- intersect(c("protein_id", "id", "rownames"), names(parent_loadings))[1]
        if (!is.na(id_col)) {
          scores <- parent_loadings[[pc_col]]
          names(scores) <- as.character(parent_loadings[[id_col]])
        } else {
          scores <- parent_loadings[[pc_col]]
          names(scores) <- rownames(parent_loadings)
        }
      } else {
        return(list(ok = FALSE, error = sprintf("Parent PCA missing %s", pc_col)))
      }
    } else {
      return(list(ok = FALSE, error = "Parent PCA loadings format unrecognized"))
    }

    scores2 <- rename_scores_to_gene_symbols(scores)
    payload$scores <- scores2
    payload$n_proteins <- length(scores2)
    # FIX: Set score_label so 1dgofcs axis reflects PC source
    payload$score_label <- paste0("PC", pc_num)
  }

  # PCA ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ GO-ORA (top/bottom N by loading): Use ranked protein sets
  else if (parent_engine == "pca" && child_engine == "goora") {
    pc <- child_params$pc %||% 1
    direction <- child_params$direction %||% "top"
    top_n <- child_params$top_n %||% 50

    if (is.null(parent_loadings)) {
      return(list(ok = FALSE, error = "Parent PCA missing loadings"))
    }

    # Get loadings for specified PC - handle both matrix and data.frame formats
    loadings_vec <- NULL
    protein_ids <- NULL

    if (is.matrix(parent_loadings) && ncol(parent_loadings) >= pc) {
      loadings_vec <- parent_loadings[, pc]
      protein_ids <- rownames(parent_loadings)
    } else if (is.data.frame(parent_loadings)) {
      pc_col <- paste0("PC", pc)
      if (pc_col %in% names(parent_loadings)) {
        loadings_vec <- parent_loadings[[pc_col]]
        # Find protein ID column
        id_col <- intersect(c("protein_id", "id", "rownames"), names(parent_loadings))[1]
        if (!is.na(id_col)) {
          protein_ids <- as.character(parent_loadings[[id_col]])
        } else {
          protein_ids <- rownames(parent_loadings)
        }
      } else {
        return(list(ok = FALSE, error = sprintf("Parent PCA missing %s column in loadings data.frame", pc_col)))
      }
    } else {
      return(list(ok = FALSE, error = sprintf("Parent PCA missing PC%d loadings (unrecognized format)", pc)))
    }

    if (is.null(loadings_vec) || is.null(protein_ids) || length(loadings_vec) == 0) {
      return(list(ok = FALSE, error = "Could not extract loadings from PCA results"))
    }

    # Rank and select top/bottom N
    if (direction == "top") {
      ranked_idx <- order(loadings_vec, decreasing = TRUE)
    } else {
      ranked_idx <- order(loadings_vec, decreasing = FALSE)
    }

    selected_idx <- head(ranked_idx, top_n)
    query_proteins <- map_ids_to_gene_symbols(protein_ids[selected_idx])
    query_proteins <- query_proteins[!is.na(query_proteins) & nzchar(query_proteins)]
    query_proteins <- query_proteins[!duplicated(query_proteins)]

    payload$query_proteins <- query_proteins
    payload$n_query <- length(query_proteins)
  }

  # PCA ÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢ 2D-GOFCS: Use PC loadings as x and y scores
  else if (parent_engine == "pca" && child_engine == "2dgofcs") {
    x_ranking <- child_params$x_ranking %||% "PC1_loadings"
    y_ranking <- child_params$y_ranking %||% "PC2_loadings"

    pc_x <- as.integer(gsub("^PC(\\d+).*", "\\1", x_ranking))
    pc_y <- as.integer(gsub("^PC(\\d+).*", "\\1", y_ranking))

    if (is.null(parent_loadings)) {
      return(list(ok = FALSE, error = "Parent PCA missing loadings"))
    }

    # Extract score vectors - handle both matrix and data.frame formats
    scores_x <- NULL
    scores_y <- NULL

    if (is.matrix(parent_loadings)) {
      if (ncol(parent_loadings) >= pc_x) {
        scores_x <- parent_loadings[, pc_x]
        names(scores_x) <- rownames(parent_loadings)
      }
      if (ncol(parent_loadings) >= pc_y) {
        scores_y <- parent_loadings[, pc_y]
        names(scores_y) <- rownames(parent_loadings)
      }
    } else if (is.data.frame(parent_loadings)) {
      pc_col_x <- paste0("PC", pc_x)
      pc_col_y <- paste0("PC", pc_y)
      # Find protein ID column
      id_col <- intersect(c("protein_id", "id", "rownames"), names(parent_loadings))[1]
      protein_ids <- if (!is.na(id_col)) as.character(parent_loadings[[id_col]]) else rownames(parent_loadings)

      if (pc_col_x %in% names(parent_loadings)) {
        scores_x <- parent_loadings[[pc_col_x]]
        names(scores_x) <- protein_ids
      }
      if (pc_col_y %in% names(parent_loadings)) {
        scores_y <- parent_loadings[[pc_col_y]]
        names(scores_y) <- protein_ids
      }
    }

    if (is.null(scores_x) || is.null(scores_y)) {
      return(list(ok = FALSE, error = sprintf("Parent PCA missing PC%d or PC%d loadings", pc_x, pc_y)))
    }

    sx <- rename_scores_to_gene_symbols(scores_x)
    sy <- rename_scores_to_gene_symbols(scores_y)
    payload$scores_x <- sx
    payload$scores_y <- sy
    payload$n_proteins <- length(sx)

    # FIX: Add axis labels for PCA-paired 2D GOFCS: "PC1 Top N" / "PC2 Top N"
    payload$x_score_label <- sprintf("PC%d Top %d", pc_x, length(sx))
    payload$y_score_label <- sprintf("PC%d Top %d", pc_y, length(sy))
  }

  # Unknown combination
  else {
    return(list(
      ok = FALSE,
      error = sprintf("Unsupported parentÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢child: %sÃƒÂ¢Ã¢â‚¬Â Ã¢â‚¬â„¢%s", parent_engine, child_engine)
    ))
  }

  payload
}

#' Execute grandchildren (legacy path, retained for backward compatibility)
#' Note: 2dgofcs is now a direct child of PCA, not a grandchild of 1dgofcs
#'
#' @param child_def Child definition containing grandchildren
#' @param child_results Results from child engine
#' @param parent_loadings PCA loadings from grandparent
#' @param ctx Execution context
#' @param run_root Run root for logging
#' @return List of grandchild view objects
nr_execute_grandchildren <- function(child_def, child_results, parent_loadings,
                                     ctx, run_root) {
  grandchild_views <- list()

  for (gc_id in names(child_def$children)) {
    gc_def <- child_def$children[[gc_id]]
    gc_engine <- gc_def$engine_id %||% "2dgofcs"
    gc_params <- gc_def$params %||% list()

    nr_log(run_root, sprintf("      Grandchild: %s (%s)", gc_id, gc_engine))

    # 2D-GOFCS needs two score vectors from PCA loadings
    if (gc_engine == "2dgofcs") {
      x_ranking <- gc_params$x_ranking %||% "PC1_loadings"
      y_ranking <- gc_params$y_ranking %||% "PC2_loadings"

      pc_x <- as.integer(gsub("^PC(\\d+).*", "\\1", x_ranking))
      pc_y <- as.integer(gsub("^PC(\\d+).*", "\\1", y_ranking))

      if (is.null(parent_loadings)) {
        gc_results <- list(
          engine_id = gc_engine,
          params = gc_params,
          data = list(
            log = data.frame(
              time = format(Sys.time()),
              level = "ERROR",
              message = "Grandparent PCA loadings not available",
              stringsAsFactors = FALSE
            )
          )
        )
      } else {
        # Extract score vectors - handle both matrix and data.frame formats
        scores_x <- NULL
        scores_y <- NULL

        if (is.matrix(parent_loadings)) {
          if (ncol(parent_loadings) >= pc_x) {
            scores_x <- parent_loadings[, pc_x]
            names(scores_x) <- rownames(parent_loadings)
          }
          if (ncol(parent_loadings) >= pc_y) {
            scores_y <- parent_loadings[, pc_y]
            names(scores_y) <- rownames(parent_loadings)
          }
        } else if (is.data.frame(parent_loadings)) {
          pc_col_x <- paste0("PC", pc_x)
          pc_col_y <- paste0("PC", pc_y)
          # Find protein ID column
          id_col <- intersect(c("protein_id", "id", "rownames"), names(parent_loadings))[1]
          protein_ids <- if (!is.na(id_col)) as.character(parent_loadings[[id_col]]) else rownames(parent_loadings)

          if (pc_col_x %in% names(parent_loadings)) {
            scores_x <- parent_loadings[[pc_col_x]]
            names(scores_x) <- protein_ids
          }
          if (pc_col_y %in% names(parent_loadings)) {
            scores_y <- parent_loadings[[pc_col_y]]
            names(scores_y) <- protein_ids
          }
        }

        gc_payload <- list(
          ok = TRUE,
          engine_id = gc_engine,
          params = gc_params,
          terpbase = ctx$terpbase,
          has_terpbase = !is.null(ctx$terpbase)
        )

        # Pass scores to 2DGOFCS via context
        gc_context <- list(
          scores_x = scores_x,
          scores_y = scores_y,
          terpbase = ctx$terpbase
        )

        gc_results <- nr_run_engine(gc_engine, gc_payload, gc_context)
      }
    } else {
      # Unknown grandchild engine
      gc_results <- list(
        engine_id = gc_engine,
        params = gc_params,
        data = list(
          log = data.frame(
            time = format(Sys.time()),
            level = "WARN",
            message = sprintf("Unknown grandchild engine: %s", gc_engine),
            stringsAsFactors = FALSE
          )
        )
      )
    }

    # Append grandchild engine log entries to main run log
    nr_append_engine_log(run_root, sprintf("%s/%s", gc_engine, gc_id), gc_results)

    grandchild_views[[gc_id]] <- list(
      view_id = gc_id,
      label = gc_def$label %||% gc_id,
      engine_id = gc_engine,
      results = gc_results,
      style = gc_def$style %||% list()
    )
  }

  if (length(grandchild_views) == 0) return(NULL)
  grandchild_views
}

# =========================================================
# Full Execution Pipeline
# =========================================================

#' Execute a complete New Run pipeline
#' @param formatted_path Path to formatted .xlsx
#' @param terpflow_path Path to .terpflow
#' @param terpbase_path Path to .terpbase (optional)
#' @param out_dir Output directory
#' @param run_name Optional run name
#' @param progress_callback Optional callback function(status, message, pct)
#' @return list(ok, run_root, error)
nr_execute_run <- function(formatted_path,
                           terpflow_path,
                           terpbase_path = NULL,
                           out_dir,
                           run_name = NULL,
                           progress_callback = NULL) {

  # Track total run time
  run_start_time <- Sys.time()

  update_progress <- function(status, msg, pct) {
    if (!is.null(progress_callback)) {
      progress_callback(status, msg, pct)
    }
  }

  update_progress("running", "Loading formatted data...", 10)

  # Load inputs
  formatted <- nr_load_formatted(formatted_path)
  if (!isTRUE(formatted$ok)) {
    return(list(ok = FALSE, error = formatted$error))
  }

  update_progress("running", "Loading pipeline...", 12)

  terpflow <- nr_load_terpflow(terpflow_path)
  if (!isTRUE(terpflow$ok)) {
    return(list(ok = FALSE, error = terpflow$error))
  }

  terpbase <- if (!is.null(terpbase_path) && nzchar(terpbase_path)) {
    update_progress("running", "Loading terpbase...", 14)
    nr_load_terpbase(terpbase_path)
  } else {
    list(ok = FALSE, required = FALSE)
  }

  update_progress("running", "Validating inputs...", 16)

  # Validate
  v_formatted <- nr_validate_formatted(formatted)
  if (!isTRUE(v_formatted$ok)) {
    return(list(ok = FALSE, error = paste(v_formatted$errors, collapse = "; ")))
  }

  v_terpflow <- nr_validate_terpflow(terpflow)
  if (!isTRUE(v_terpflow$ok)) {
    return(list(ok = FALSE, error = paste(v_terpflow$errors, collapse = "; ")))
  }

  v_compat <- nr_validate_run_compatibility(formatted, terpflow, terpbase)
  if (!isTRUE(v_compat$ok)) {
    return(list(ok = FALSE, error = paste(v_compat$errors, collapse = "; ")))
  }

  update_progress("running", "Compiling run plan...", 20)

  # Compile plan
  plan_result <- nr_compile_run_plan(terpflow, formatted)
  if (!isTRUE(plan_result$ok)) {
    return(list(ok = FALSE, error = plan_result$error))
  }
  plan <- plan_result$plan

  # Validate engines in plan (early failure for unknown engines)
  # Note: fail_on_stubs=FALSE allows stub engines for development; set TRUE for production
  engine_check <- nr_validate_plan_engines(plan, fail_on_stubs = FALSE)
  if (!isTRUE(engine_check$ok)) {
    return(list(ok = FALSE, error = engine_check$error))
  }

  # Log engine status for transparency
  if (length(engine_check$stubs) > 0) {
    message(sprintf(
      "[nr_execute_run] Warning: %d stub engine(s) in plan: %s",
      length(engine_check$stubs),
      paste(engine_check$stubs, collapse = ", ")
    ))
  }

  # Build context
  ctx <- nr_build_context(formatted, terpbase)
  if (!isTRUE(ctx$ok)) {
    return(list(ok = FALSE, error = ctx$error))
  }

  update_progress("running", "Creating terpbook output...", 25)

  # Create terpbook
  run_root <- nr_create_terpbook(out_dir, run_name %||% plan$meta$pipeline_name)

  nr_log(run_root, sprintf("Pipeline: %s", plan$meta$pipeline_name))
  nr_log(run_root, sprintf("Steps: %d", plan$meta$n_steps))
  nr_log(run_root, sprintf("Analysis level: %s", plan$meta$analysis_level))

  # Execute each top-level step (containers execute nested substeps)
  n_steps <- length(plan$steps)

  work_units <- 0L
  for (s in (plan$steps %||% list())) {
    if (!is.list(s)) next
    if (identical(tolower(s$type %||% "engine"), "container")) {
      work_units <- work_units + max(1L, length(s$substeps %||% list()))
    } else {
      work_units <- work_units + 1L
    }
  }
  if (work_units < 1L) work_units <- 1L
  unit_idx <- 0L

  update_ctx_from_results <- function(ctx, engine_id, results) {
    if (engine_id %in% c("dataprocessor", "peptide_aggregate_to_protein") &&
        !is.null(results$data$mat) && !is.null(results$data$ids)) {
      ctx$mat <- results$data$mat
      ctx$ids <- results$data$ids
      nr_log(run_root, sprintf("  Context updated: %d rows x %d cols", nrow(ctx$mat), ncol(ctx$mat)))

      if (identical(engine_id, "peptide_aggregate_to_protein")) {
        if (is.null(ctx$metadata) || !is.list(ctx$metadata)) ctx$metadata <- list()
        ctx$metadata$current_analysis_level <- "protein"
        nr_log(run_root, "  Context analysis level updated to protein")
      }
    }
    ctx
  }

  for (i in seq_along(plan$steps)) {
    step <- plan$steps[[i]]
    step_start_time <- Sys.time()

    step_type <- tolower(step$type %||% "engine")

    # AUTO-SKIP: peptide_analysis block when input data is Protein level
    if (identical(tolower(step$engine_id %||% ""), "peptide_analysis")) {
      data_level <- nr_normalize_analysis_level(plan$meta$analysis_level %||% NULL)
      if (identical(data_level, "protein")) {
        nr_log(run_root, sprintf("=== Step %d: %s (SKIPPED - data is protein level) ===", i, step$engine_id))
        next
      }
    }

    if (identical(step_type, "container")) {
      subs <- step$substeps %||% list()

      nr_log(run_root, sprintf("=== Step %d: %s (container) ===", i, step$engine_id))

      sub_views <- list()
      for (j in seq_along(subs)) {
        ss <- subs[[j]]
        if (!is.list(ss)) next

        unit_idx <- unit_idx + 1L
        pct <- 25 + ((unit_idx - 1) / work_units) * 70
        update_progress(
          "running",
          sprintf("Step %d/%d: %s substep %d/%d (%s)...", i, n_steps, step$engine_id, j, length(subs), ss$engine_id),
          pct
        )

        nr_log(run_root, sprintf("  - Substep %d: %s", j, ss$engine_id))

        payload <- nr_build_step_payload(ctx, ss)
        results <- nr_run_engine(ss$engine_id, payload, ctx)

        nr_append_engine_log(run_root, sprintf("%s/substep_%03d/%s", step$engine_id, j, ss$engine_id), results)

        ctx <- update_ctx_from_results(ctx, ss$engine_id, results)

        sub_views[[length(sub_views) + 1L]] <- list(
          view_id = sprintf("substep_%03d", j),
          label = ss$label %||% sprintf("Substep %d", j),
          engine_id = ss$engine_id,
          results = results,
          style = ss$style %||% list(),
          meta = list(
            execution_order = j,
            step_id = ss$step_id %||% NULL,
            analysis_level = ss$input_level %||% NULL,
            output_level = ss$output_level %||% NULL,
            system_generated = isTRUE(ss$system_generated %||% FALSE)
          )
        )
      }

      container_results <- list(
        engine_id = step$engine_id,
        params = step$params %||% list(),
        data = list(
          type = "container",
          substeps = lapply(subs, function(ss) list(
            step_id = (ss %||% list())$step_id %||% NULL,
            engine_id = (ss %||% list())$engine_id %||% NULL,
            label = (ss %||% list())$label %||% NULL,
            input_level = (ss %||% list())$input_level %||% NULL,
            output_level = (ss %||% list())$output_level %||% NULL
          ))
        )
      )

      write_start_time <- Sys.time()
      nr_write_step(
        run_root = run_root,
        step_index = i,
        step_id = step$step_id,
        engine_id = step$engine_id,
        results = container_results,
        style = step$style,
        views = sub_views
      )
      write_duration <- as.numeric(difftime(Sys.time(), write_start_time, units = "secs"))

      step_duration <- as.numeric(difftime(Sys.time(), step_start_time, units = "secs"))
      nr_log(run_root, sprintf("  Step %d completed in %.2f seconds (write: %.2f sec)", i, step_duration, write_duration))
      next
    }

    unit_idx <- unit_idx + 1L
    pct <- 25 + ((unit_idx - 1) / work_units) * 70
    update_progress("running", sprintf("Step %d/%d: Running %s...", i, n_steps, step$engine_id), pct)

    nr_log(run_root, sprintf("=== Step %d: %s ===", i, step$engine_id))

    payload <- nr_build_step_payload(ctx, step)
    results <- nr_run_engine(step$engine_id, payload, ctx)

    nr_append_engine_log(run_root, step$engine_id, results)
    ctx <- update_ctx_from_results(ctx, step$engine_id, results)

    paired_views <- NULL
    if (!is.null(step$paired_children) && length(step$paired_children) > 0) {
      nr_log(run_root, sprintf("  Processing %d paired children...", length(step$paired_children)))
      paired_views <- nr_execute_paired_children(step, results, ctx, run_root)
    }

    child_views <- paired_views %||% list()
    if (length(child_views) == 0) child_views <- NULL

    write_start_time <- Sys.time()
    nr_write_step(
      run_root = run_root,
      step_index = i,
      step_id = step$step_id,
      engine_id = step$engine_id,
      results = results,
      style = step$style,
      views = child_views
    )
    write_duration <- as.numeric(difftime(Sys.time(), write_start_time, units = "secs"))

    step_duration <- as.numeric(difftime(Sys.time(), step_start_time, units = "secs"))
    nr_log(run_root, sprintf("  Step %d completed in %.2f seconds (write: %.2f sec)", i, step_duration, write_duration))
  }

  update_progress("running", "Validating output...", 95)

  # Validate terpbook structure before finalizing
  validation <- tryCatch({
    nr_validate_terpbook_structure(run_root)
  }, error = function(e) {
    nr_log(run_root, sprintf("Validation error: %s", conditionMessage(e)), level = "ERROR")
    list(valid = FALSE, errors = conditionMessage(e), warnings = character(), summary = conditionMessage(e))
  })
  if (!isTRUE(validation$valid)) {
    nr_log(run_root, sprintf("VALIDATION FAILED: %s", validation$summary), level = "ERROR")
    return(list(
      ok = FALSE,
      error = paste("Terpbook validation failed:", validation$summary),
      errors = validation$errors,
      run_root = run_root
    ))
  }

  nr_log(run_root, sprintf("Validation passed: %s", validation$summary))
  nr_finalize_terpbook(run_root, "done")

  # Log total run time
  total_run_time <- as.numeric(difftime(Sys.time(), run_start_time, units = "secs"))
  nr_log(run_root, sprintf("=== Total run time: %.2f seconds ===", total_run_time))

  update_progress("done", sprintf("Pipeline complete! (%.1f sec)", total_run_time), 100)

  list(
    ok = TRUE,
    run_root = run_root,
    manifest_path = file.path(run_root, "manifest.json"),
    log_path = file.path(run_root, "log.txt"),
    total_time = total_run_time,
    error = NULL
  )
}

# =========================================================
# Terpbook Structural Validator
# =========================================================

#' Validate terpbook structure against the contract
#' @param run_root Path to the terpbook run root directory
#' @return list(valid, errors, warnings, summary)
nr_validate_terpbook_structure <- function(run_root) {
  errors <- character()
  warnings <- character()

  # Helper: validate a nested view directory recursively
  validate_view <- function(view_dir_path, path_prefix) {
    # Check view.json exists
    view_json_path <- file.path(view_dir_path, "view.json")
    if (!file.exists(view_json_path)) {
      errors <<- c(errors, sprintf("%s: view.json not found", path_prefix))
      return()
    }

    # Load view.json
    view_desc <- tryCatch({
      nr_read_json(view_json_path)
    }, error = function(e) {
      errors <<- c(errors, sprintf("%s: failed to parse view.json: %s",
                                   path_prefix, conditionMessage(e)))
      NULL
    })

    if (is.null(view_desc)) return()

    # Validate view.json has engine_id
    if (is.null(view_desc$engine_id) || !nzchar(as.character(view_desc$engine_id))) {
      errors <<- c(errors, sprintf("%s/view.json: missing 'engine_id'", path_prefix))
    }

    # Check results.rds exists and is valid
    results_path <- file.path(view_dir_path, "results.rds")
    if (!file.exists(results_path)) {
      errors <<- c(errors, sprintf("%s: results.rds not found", path_prefix))
    } else {
      results <- tryCatch({
        readRDS(results_path)
      }, error = function(e) {
        errors <<- c(errors, sprintf("%s/results.rds: failed to load: %s",
                                     path_prefix, conditionMessage(e)))
        NULL
      })

      if (!is.null(results) && is.null(results$data)) {
        errors <<- c(errors, sprintf("%s/results.rds: missing 'data' field (required)",
                                     path_prefix))
      }
    }

    # Recursively validate child views
    child_views <- view_desc$views
    if (!is.null(child_views) && length(child_views) > 0) {
      # Normalize to list of views
      child_list <- if (is.data.frame(child_views)) {
        lapply(seq_len(nrow(child_views)), function(i) as.list(child_views[i, , drop = FALSE]))
      } else if (is.list(child_views)) {
        child_views
      } else {
        list()
      }

      for (cv in child_list) {
        cv_path <- if (is.list(cv)) (cv$path %||% "") else ""
        if (!nzchar(cv_path)) next

        cv_dir_path <- file.path(view_dir_path, cv_path)
        cv_prefix <- file.path(path_prefix, cv_path)

        if (!dir.exists(cv_dir_path)) {
          errors <<- c(errors, sprintf("%s: view directory does not exist", cv_prefix))
        } else {
          validate_view(cv_dir_path, cv_prefix)
        }
      }
    }
  }

  # Helper: validate step.json views[] (child views of a step)
  validate_step_views <- function(step_dir_path, step_prefix) {
    step_json_path <- file.path(step_dir_path, "step.json")
    if (!file.exists(step_json_path)) return()

    step_desc <- tryCatch({
      nr_read_json(step_json_path)
    }, error = function(e) NULL)

    if (is.null(step_desc)) return()

    views <- step_desc$views
    if (is.null(views) || length(views) == 0) return()

    # Normalize to list
    view_list <- if (is.data.frame(views)) {
      lapply(seq_len(nrow(views)), function(i) as.list(views[i, , drop = FALSE]))
    } else if (is.list(views)) {
      views
    } else {
      list()
    }

    for (v in view_list) {
      v_path <- if (is.list(v)) (v$path %||% "") else ""
      if (!nzchar(v_path)) next

      view_dir_path <- file.path(step_dir_path, v_path)
      view_prefix <- file.path(step_prefix, v_path)

      if (!dir.exists(view_dir_path)) {
        errors <<- c(errors, sprintf("%s: view directory does not exist", view_prefix))
      } else {
        validate_view(view_dir_path, view_prefix)
      }
    }
  }

  # 1. Check manifest.json exists
  manifest_path <- file.path(run_root, "manifest.json")
  if (!file.exists(manifest_path)) {
    errors <- c(errors, "manifest.json not found at run root")
    return(list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      summary = sprintf("%d error(s), %d warning(s)", length(errors), length(warnings))
    ))
  }

  # 2. Load and validate manifest structure
  manifest <- tryCatch({
    nr_read_json(manifest_path)
  }, error = function(e) {
    errors <<- c(errors, sprintf("Failed to parse manifest.json: %s", conditionMessage(e)))
    NULL
  })

  if (is.null(manifest)) {
    return(list(
      valid = FALSE,
      errors = errors,
      warnings = warnings,
      summary = sprintf("%d error(s), %d warning(s)", length(errors), length(warnings))
    ))
  }

  # 3. Check steps array exists and is non-empty
  steps <- manifest$steps
  if (is.null(steps)) {
    errors <- c(errors, "manifest.json: 'steps' array is missing")
  } else {
    # jsonlite may return steps as data.frame (simplifyVector=TRUE) or list
    # Normalize to list of step objects for consistent iteration
    step_list <- if (is.data.frame(steps)) {
      # Convert data.frame rows to list of lists
      lapply(seq_len(nrow(steps)), function(i) as.list(steps[i, , drop = FALSE]))
    } else if (is.list(steps)) {
      steps
    } else {
      list()
    }

    if (length(step_list) == 0) {
      errors <- c(errors, "manifest.json: 'steps' array is empty")
    }

    # 4. Validate each step entry
    for (i in seq_along(step_list)) {
      s <- step_list[[i]]
      step_prefix <- sprintf("manifest.steps[%d]", i)

      # Extract values (handle both list and data.frame row formats)
      s_step_index <- if (is.list(s)) s$step_index else s[["step_index"]]
      s_step_id <- if (is.list(s)) s$step_id else s[["step_id"]]
      s_engine_id <- if (is.list(s)) s$engine_id else s[["engine_id"]]
      s_step_dir <- if (is.list(s)) s$step_dir else s[["step_dir"]]

      # Helper for safe NA check (handles NULL, length-0, and vectors)
      safe_is_missing <- function(x) {
        is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x[1]))
      }

      # Required fields per contract: step_index, step_id, engine_id, step_dir
      if (safe_is_missing(s_step_index)) {
        errors <- c(errors, sprintf("%s: missing 'step_index'", step_prefix))
      }
      if (safe_is_missing(s_step_id) || !nzchar(as.character(s_step_id[1]))) {
        errors <- c(errors, sprintf("%s: missing or empty 'step_id'", step_prefix))
      }
      if (safe_is_missing(s_engine_id) || !nzchar(as.character(s_engine_id[1]))) {
        errors <- c(errors, sprintf("%s: missing or empty 'engine_id'", step_prefix))
      }
      if (safe_is_missing(s_step_dir) || !nzchar(as.character(s_step_dir[1]))) {
        errors <- c(errors, sprintf("%s: missing or empty 'step_dir'", step_prefix))
      } else {
        # 5. Check step directory exists
        step_dir_path <- file.path(run_root, as.character(s_step_dir))
        if (!dir.exists(step_dir_path)) {
          errors <- c(errors, sprintf("%s: step_dir '%s' does not exist", step_prefix, s_step_dir))
        } else {
          # 6. Check results.rds exists (required per contract)
          results_path <- file.path(step_dir_path, "results.rds")
          if (!file.exists(results_path)) {
            errors <- c(errors, sprintf("%s/%s: results.rds not found", s_step_dir, "results.rds"))
          } else {
            # 7. Validate results.rds can be loaded and has required structure
            results <- tryCatch({
              readRDS(results_path)
            }, error = function(e) {
              errors <<- c(errors, sprintf("%s/results.rds: failed to load: %s",
                                           s_step_dir, conditionMessage(e)))
              NULL
            })

            if (!is.null(results)) {
              # Validate results has engine_id (optional but recommended)
              if (is.null(results$engine_id)) {
                warnings <- c(warnings, sprintf("%s/results.rds: missing 'engine_id' field",
                                                s_step_dir))
              }
              # Validate results has data field (required per contract)
              if (is.null(results$data)) {
                errors <- c(errors, sprintf("%s/results.rds: missing 'data' field (required)",
                                            s_step_dir))
              }
            }
          }

          # 8. Validate nested views (if any)
          validate_step_views(step_dir_path, s_step_dir)
        }
      }
    }
  }

  # 9. Check log.txt exists (recommended)
  log_path <- file.path(run_root, "log.txt")
  if (!file.exists(log_path)) {
    warnings <- c(warnings, "log.txt not found (recommended)")
  }

  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    summary = sprintf("%d error(s), %d warning(s)", length(errors), length(warnings))
  )
}

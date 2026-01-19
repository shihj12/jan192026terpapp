# R/engines/loader.R
# Raw file loader + formatted workbook IO (data + design sheets)

msterp_require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         "\nInstall with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))")
  }
  invisible(TRUE)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && (length(a) > 1 || (!is.na(a) && nzchar(as.character(a))))) a else b


msterp_file_ext <- function(path) {
  tolower(tools::file_ext(path))
}

msterp_read_raw_file <- function(path, sheet = NULL) {
  ext <- msterp_file_ext(path)
  
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    msterp_require_pkgs(c("readxl"))
    sheets <- readxl::excel_sheets(path)
    if (length(sheets) == 0) stop("No sheets found in Excel file: ", path)
    if (is.null(sheet)) sheet <- sheets[[1]]
    df <- readxl::read_excel(path, sheet = sheet)
    df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    return(list(data = df, source = list(path = path, ext = ext, sheet = sheet, sheets = sheets)))
  }
  
  if (ext %in% c("csv", "tsv", "txt")) {
    # Default delimiter: csv -> comma; tsv/txt -> tab
    delim <- if (ext == "csv") "," else "\t"
    df <- utils::read.table(path, sep = delim, header = TRUE, quote = "\"",
                            comment.char = "", stringsAsFactors = FALSE,
                            check.names = FALSE)
    return(list(data = df, source = list(path = path, ext = ext, sheet = NULL, sheets = NULL)))
  }
  
  stop("Unsupported file type: .", ext, "\nSupported: .xlsx, .csv, .tsv, .txt")
}

msterp_build_design_sheet <- function(meta, groups_df, columns_df) {
  # meta: named list
  # groups_df: data.frame with group_id, group_name, color, is_control (optional)
  # columns_df: data.frame with data_col, display_name, group_id, group_name, replicate, include, source_file, source_col

  if (!is.list(meta) || length(meta) == 0) stop("meta must be a non-empty named list.")
  if (!all(c("group_id", "group_name", "color") %in% names(groups_df))) {
    stop("groups_df must contain: group_id, group_name, color")
  }
  if (!all(c("data_col", "display_name", "group_id", "group_name", "replicate", "include") %in% names(columns_df))) {
    stop("columns_df must contain: data_col, display_name, group_id, group_name, replicate, include")
  }

  # Meta rows
  meta_df <- data.frame(
    record_type = "meta",
    key = names(meta),
    value = vapply(meta, function(x) paste0(x, collapse = "|"), character(1)),
    stringsAsFactors = FALSE
  )

  # Group rows
  group_df <- groups_df
  group_df$record_type <- "group"
  group_df$key <- NA_character_
  group_df$value <- NA_character_

  # Ensure is_control is present (defaults to FALSE if not provided)
  if (!"is_control" %in% names(group_df)) {
    group_df$is_control <- FALSE
  }

  # Column rows
  col_df <- columns_df
  col_df$record_type <- "column"
  col_df$key <- NA_character_
  col_df$value <- NA_character_

  # Normalize column set
  all_cols <- c(
    "record_type", "key", "value",
    "group_id", "group_name", "color", "is_control",
    "data_col", "display_name", "replicate", "include",
    "source_file", "source_col"
  )

  # Ensure all exist
  for (nm in all_cols) {
    if (!nm %in% names(meta_df)) meta_df[[nm]] <- NA_character_
    if (!nm %in% names(group_df)) group_df[[nm]] <- NA_character_
    if (!nm %in% names(col_df))   col_df[[nm]]   <- NA_character_
  }

  meta_df <- meta_df[, all_cols, drop = FALSE]
  group_df <- group_df[, all_cols, drop = FALSE]
  col_df   <- col_df[, all_cols, drop = FALSE]

  rbind(meta_df, group_df, col_df)
}

msterp_group_name_regex <- function() {
  "^[A-Za-z][A-Za-z0-9 _.-]*$"
}

msterp_is_valid_group_name <- function(name, regex = msterp_group_name_regex()) {
  name <- as.character(name %||% "")
  vapply(name, function(x) {
    x <- trimws(as.character(x %||% ""))
    nzchar(x) && grepl(regex, x, perl = TRUE)
  }, logical(1))
}

msterp_write_formatted_xlsx <- function(out_path, data_df, design_df) {
  msterp_require_pkgs(c("openxlsx"))
  
  if (!is.data.frame(data_df)) stop("data_df must be a data.frame.")
  if (!is.data.frame(design_df)) stop("design_df must be a data.frame.")
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "data")
  openxlsx::addWorksheet(wb, "design")
  
  openxlsx::writeData(wb, sheet = "data", x = data_df)
  openxlsx::writeData(wb, sheet = "design", x = design_df)
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  invisible(out_path)
}

msterp_read_formatted_xlsx <- function(path) {
  msterp_require_pkgs(c("readxl"))
  
  sheets <- readxl::excel_sheets(path)
  if (!all(c("data", "design") %in% sheets)) {
    stop("Formatted file must contain sheets: 'data' and 'design'. Found: ", paste(sheets, collapse = ", "))
  }
  
  data_df <- readxl::read_excel(path, sheet = "data")
  data_df <- as.data.frame(data_df, stringsAsFactors = FALSE, check.names = FALSE)
  
  design_df <- readxl::read_excel(path, sheet = "design")
  design_df <- as.data.frame(design_df, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Coerce common cols if present
  if ("include" %in% names(design_df)) {
    # include might be logical/NA/character
    if (is.character(design_df$include)) {
      design_df$include <- tolower(design_df$include) %in% c("true", "t", "1", "yes", "y")
    } else {
      design_df$include <- as.logical(design_df$include)
    }
  }
  if ("replicate" %in% names(design_df)) {
    suppressWarnings(design_df$replicate <- as.integer(design_df$replicate))
  }
  
  list(path = path, data = data_df, design = design_df)
}

# R/engines/loader.R
# --- update msterp_validate_formatted() to enforce the new "secondary IDs required" rule ---
# Replace the "need_keys" section + add the level-specific checks shown below.

msterp_validate_formatted <- function(obj) {
  errors <- character(0)
  warnings <- character(0)
  
  if (is.null(obj$data) || !is.data.frame(obj$data)) errors <- c(errors, "Missing/invalid 'data' sheet.")
  if (is.null(obj$design) || !is.data.frame(obj$design)) errors <- c(errors, "Missing/invalid 'design' sheet.")
  
  if (length(errors) > 0) return(list(ok = FALSE, errors = errors, warnings = warnings))
  
  d <- obj$design
  if (!"record_type" %in% names(d)) errors <- c(errors, "Design sheet missing column: record_type")
  
  meta <- d[d$record_type == "meta", c("key", "value"), drop = FALSE]
  meta_keys <- meta$key
  meta_map <- stats::setNames(as.character(meta$value), meta$key)
  
  need_keys <- c("schema_version", "analysis_level", "id_primary_type", "id_primary_col", "created_at")
  missing_keys <- setdiff(need_keys, meta_keys)
  if (length(missing_keys) > 0) errors <- c(errors, paste0("Missing meta keys: ", paste(missing_keys, collapse = ", ")))
  
  # Analysis level validation:
  # - Canonical casing is lowercase ("protein"/"peptide")
  # - For peptide-level formatted inputs, a protein ID column is still required so the pipeline can aggregate peptides to proteins.
  lvl <- meta_map[["analysis_level"]]
  lvl_normalized <- tolower(trimws(as.character(lvl %||% "")))
  if (!lvl_normalized %in% c("protein", "peptide")) {
    errors <- c(errors, sprintf(
      "Only 'protein' or 'peptide' levels are supported. Found: '%s'", lvl %||% "(missing)"
    ))
  }
  if (is.na(meta_map[["id_protein_col"]]) || !nzchar(meta_map[["id_protein_col"]])) {
    errors <- c(errors, "Protein ID column is required (meta key id_protein_col must be non-empty).")
  } else if (!is.null(obj$data) && is.data.frame(obj$data) && nrow(obj$data) > 0) {
    prot_col <- as.character(meta_map[["id_protein_col"]])
    if (nzchar(prot_col) && !prot_col %in% names(obj$data)) {
      errors <- c(errors, sprintf("Protein ID column '%s' not found in data sheet.", prot_col))
    }
  }
  
  # ... keep the rest of your existing group/column checks unchanged ...
  
  list(ok = length(errors) == 0, errors = errors, warnings = warnings)
}


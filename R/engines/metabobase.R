# R/engines/metabobase.R
# MetaboBase: Metabolite annotation database for pathway and class enrichment
#
# Parallel structure to terpbase.R but for metabolites:
#  - Pathway mappings (KEGG, Reactome) instead of GO terms
#  - Chemical class mappings (lipid classes, etc.)
#  - Cross-reference ID mapping (HMDB, KEGG, ChEBI)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tibble)
})

# -----------------------------
# Constants / schema
# -----------------------------
METABOBASE_SCHEMA_VERSION <- 1L

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

#' Required columns for MetaboBase CSV input
#' @return Character vector of required column names
msterp_metabobase_required_csv_cols <- function() {
  c(
    "metabolite_id",  # Primary identifier (user's choice: HMDB, KEGG, ChEBI, or name)
    "name"            # Common metabolite name
  )
}

#' Optional columns for MetaboBase CSV input
#' @return Character vector of optional column names
msterp_metabobase_optional_csv_cols <- function() {
  c(
    "hmdb_id",          # HMDB ID (HMDB0000001)
    "kegg_id",          # KEGG compound ID (C00001)
    "chebi_id",         # ChEBI ID
    "pubchem_id",       # PubChem CID
    "inchikey",         # InChIKey
    "formula",          # Molecular formula
    "class",            # Chemical class (Lipid, Amino acid, etc.)
    "subclass",         # Chemical subclass
    "superclass",       # Chemical superclass
    "pathway_kegg",     # KEGG pathway IDs (semicolon-separated)
    "pathway_reactome"  # Reactome pathway IDs (semicolon-separated)
  )
}

# -----------------------------
# Header validation for CSV
# -----------------------------
metabobase_csv_headers <- function(path) {
  hdr <- read.csv(path, nrows = 0, check.names = FALSE)
  names(hdr)
}

metabobase_validate_csv <- function(path) {
  required <- msterp_metabobase_required_csv_cols()
  cols <- metabobase_csv_headers(path)
  cols_lower <- tolower(cols)
  missing <- setdiff(tolower(required), cols_lower)
  list(ok = length(missing) == 0, required = required, present = cols, missing = missing)
}

# -----------------------------
# Build from CSV
# -----------------------------
#' Build MetaboBase from a CSV file
#' @param path Path to CSV file with metabolite annotations
#' @param library_name Optional name for this library
#' @param progress_inc Progress callback (amount, detail)
#' @param progress_set Progress callback (value, detail)
#' @return MetaboBase object
metabobase_build_from_csv <- function(
    path,
    library_name = NULL,
    progress_inc = function(amount, detail = NULL) NULL,
    progress_set = function(value, detail = NULL) NULL
) {
  required_cols <- msterp_metabobase_required_csv_cols()

  progress_set(0.05, "Reading CSV")
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  # Normalize column names to lowercase for matching
  names(raw) <- tolower(names(raw))

  missing <- setdiff(tolower(required_cols), names(raw))
  if (length(missing)) {
    stop(
      "Missing required columns in CSV:\n  ",
      paste(missing, collapse = ", "),
      "\n\nRequired columns: ", paste(required_cols, collapse = ", ")
    )
  }

  progress_inc(0.10, "Parsing columns")

  # Build base metabolite table
  df_base <- tibble(
    metabolite_id = as.character(raw[["metabolite_id"]]),
    name          = as.character(raw[["name"]] %||% raw[["metabolite_id"]]),
    hmdb_id       = as.character(raw[["hmdb_id"]] %||% NA_character_),
    kegg_id       = as.character(raw[["kegg_id"]] %||% NA_character_),
    chebi_id      = as.character(raw[["chebi_id"]] %||% NA_character_),
    pubchem_id    = as.character(raw[["pubchem_id"]] %||% NA_character_),
    inchikey      = as.character(raw[["inchikey"]] %||% NA_character_),
    formula       = as.character(raw[["formula"]] %||% NA_character_),
    class         = as.character(raw[["class"]] %||% NA_character_),
    subclass      = as.character(raw[["subclass"]] %||% NA_character_),
    superclass    = as.character(raw[["superclass"]] %||% NA_character_),
    pathway_kegg  = as.character(raw[["pathway_kegg"]] %||% NA_character_),
    pathway_reactome = as.character(raw[["pathway_reactome"]] %||% NA_character_)
  )

  # Clean up empty strings to NA
  df_base <- df_base %>%
    mutate(across(everything(), ~ifelse(. == "" | . == "NA", NA_character_, .)))

  progress_inc(0.15, "Building pathway tables")

  # Parse KEGG pathways (semicolon-separated)
  kegg_long <- df_base %>%
    filter(!is.na(pathway_kegg), pathway_kegg != "") %>%
    select(metabolite_id, pathway_kegg) %>%
    mutate(pathway_items = strsplit(pathway_kegg, ";", fixed = TRUE)) %>%
    unnest(pathway_items) %>%
    mutate(
      pathway_items = str_trim(pathway_items),
      pathway_type = "KEGG"
    ) %>%
    filter(pathway_items != "") %>%
    transmute(
      metabolite_id,
      pathway_id = pathway_items,
      pathway_type
    )

  # Parse Reactome pathways (semicolon-separated)
  reactome_long <- df_base %>%
    filter(!is.na(pathway_reactome), pathway_reactome != "") %>%
    select(metabolite_id, pathway_reactome) %>%
    mutate(pathway_items = strsplit(pathway_reactome, ";", fixed = TRUE)) %>%
    unnest(pathway_items) %>%
    mutate(
      pathway_items = str_trim(pathway_items),
      pathway_type = "Reactome"
    ) %>%
    filter(pathway_items != "") %>%
    transmute(
      metabolite_id,
      pathway_id = pathway_items,
      pathway_type
    )

  # Combine pathway annotations
  annot_long <- bind_rows(kegg_long, reactome_long) %>%
    filter(!is.na(metabolite_id), metabolite_id != "") %>%
    distinct(metabolite_id, pathway_id, pathway_type)

  # Add pathway names (will be populated by API fetch if available)
  annot_long <- annot_long %>%
    mutate(pathway_name = pathway_id)  # Default: use ID as name

  progress_inc(0.15, "Building class mappings")

  # Build chemical class mappings
  class_mappings <- df_base %>%
    filter(!is.na(class) | !is.na(subclass) | !is.na(superclass)) %>%
    select(metabolite_id, class, subclass, superclass) %>%
    distinct()

  progress_inc(0.15, "Grouping pathways")

  # Build terms_by_id (pathway term info)
  terms_by_id <- annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  progress_inc(0.10, "Building ID cross-reference")

  # Build ID cross-reference table
  id_xref <- df_base %>%
    select(metabolite_id, hmdb_id, kegg_id, chebi_id, pubchem_id) %>%
    filter(!is.na(metabolite_id)) %>%
    distinct()

  progress_inc(0.10, "Building output")

  # Build metabolite metadata
  metabolite_meta <- df_base %>%
    select(metabolite_id, name, hmdb_id, kegg_id, chebi_id, formula, class, subclass) %>%
    distinct()

  progress_set(1, "Done")

  list(
    schema_version     = METABOBASE_SCHEMA_VERSION,
    metabobase_version = 1L,
    library_name       = library_name %||% NULL,
    created            = Sys.time(),
    n_raw_rows         = nrow(raw),
    n_metabolites      = length(unique(df_base$metabolite_id)),
    annot_long         = annot_long,
    terms_by_id        = terms_by_id,
    class_mappings     = class_mappings,
    id_xref            = id_xref,
    metabolite_meta    = metabolite_meta
  )
}

# -----------------------------
# KEGG API functions
# -----------------------------
#' Fetch pathway annotations from KEGG API
#' @param compound_ids Character vector of KEGG compound IDs (e.g., "C00001")
#' @param progress_inc Progress callback
#' @return Data frame with metabolite_id, pathway_id, pathway_name, pathway_type
metabobase_fetch_kegg <- function(
    compound_ids,
    progress_inc = function(amount, detail = NULL) NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for KEGG API access. Install with: install.packages('httr')")
  }

  # Clean compound IDs
  compound_ids <- unique(trimws(as.character(compound_ids)))
  compound_ids <- compound_ids[nzchar(compound_ids)]

  if (length(compound_ids) == 0) {
    return(tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    ))
  }

  results <- list()
  batch_size <- 10  # KEGG API limit
  n_batches <- ceiling(length(compound_ids) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(compound_ids))
    batch <- compound_ids[start_idx:end_idx]

    progress_inc(0.8 / n_batches, paste0("Fetching KEGG batch ", i, "/", n_batches))

    for (cpd_id in batch) {
      tryCatch({
        # KEGG API: get compound info including pathways
        url <- paste0("https://rest.kegg.jp/get/", cpd_id)
        resp <- httr::GET(url, httr::timeout(30))

        if (httr::status_code(resp) == 200) {
          content <- httr::content(resp, "text", encoding = "UTF-8")

          # Parse PATHWAY section
          pathway_section <- FALSE
          pathways <- list()

          for (line in strsplit(content, "\n")[[1]]) {
            if (grepl("^PATHWAY", line)) {
              pathway_section <- TRUE
              # First pathway on same line
              match <- regmatches(line, regexec("(map\\d+)\\s+(.+)", line))[[1]]
              if (length(match) == 3) {
                pathways <- c(pathways, list(c(match[2], trimws(match[3]))))
              }
            } else if (pathway_section && grepl("^\\s+", line)) {
              # Continuation of PATHWAY section
              match <- regmatches(line, regexec("(map\\d+)\\s+(.+)", line))[[1]]
              if (length(match) == 3) {
                pathways <- c(pathways, list(c(match[2], trimws(match[3]))))
              }
            } else if (pathway_section && !grepl("^\\s+", line)) {
              # End of PATHWAY section
              pathway_section <- FALSE
            }
          }

          if (length(pathways) > 0) {
            for (pw in pathways) {
              results <- c(results, list(tibble(
                metabolite_id = cpd_id,
                pathway_id = pw[1],
                pathway_name = pw[2],
                pathway_type = "KEGG"
              )))
            }
          }
        }

        Sys.sleep(0.1)  # Rate limiting
      }, error = function(e) {
        warning(paste("Failed to fetch KEGG data for", cpd_id, ":", e$message))
      })
    }
  }

  if (length(results) > 0) {
    bind_rows(results)
  } else {
    tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    )
  }
}

#' Fetch pathway annotations from Reactome API
#' @param chebi_ids Character vector of ChEBI IDs (e.g., "15377" for water)
#' @param progress_inc Progress callback
#' @return Data frame with metabolite_id, pathway_id, pathway_name, pathway_type
metabobase_fetch_reactome <- function(
    chebi_ids,
    progress_inc = function(amount, detail = NULL) NULL
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for Reactome API access. Install with: install.packages('httr')")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for Reactome API access. Install with: install.packages('jsonlite')")
  }

  # Clean ChEBI IDs (remove "CHEBI:" prefix if present)
  chebi_ids <- unique(trimws(as.character(chebi_ids)))
  chebi_ids <- chebi_ids[nzchar(chebi_ids)]
  chebi_ids <- gsub("^CHEBI:", "", chebi_ids, ignore.case = TRUE)

  if (length(chebi_ids) == 0) {
    return(tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    ))
  }

  results <- list()

  for (i in seq_along(chebi_ids)) {
    chebi_id <- chebi_ids[i]
    progress_inc(0.8 / length(chebi_ids), paste0("Fetching Reactome ", i, "/", length(chebi_ids)))

    tryCatch({
      # Reactome API: query by ChEBI ID
      url <- paste0("https://reactome.org/ContentService/data/query/CHEBI:", chebi_id, "/pathways")
      resp <- httr::GET(url, httr::timeout(30), httr::add_headers(Accept = "application/json"))

      if (httr::status_code(resp) == 200) {
        content <- httr::content(resp, "text", encoding = "UTF-8")
        pathways <- jsonlite::fromJSON(content, simplifyVector = FALSE)

        if (length(pathways) > 0) {
          for (pw in pathways) {
            results <- c(results, list(tibble(
              metabolite_id = paste0("CHEBI:", chebi_id),
              pathway_id = pw$stId %||% NA_character_,
              pathway_name = pw$displayName %||% pw$name %||% NA_character_,
              pathway_type = "Reactome"
            )))
          }
        }
      }

      Sys.sleep(0.1)  # Rate limiting
    }, error = function(e) {
      warning(paste("Failed to fetch Reactome data for CHEBI:", chebi_id, ":", e$message))
    })
  }

  if (length(results) > 0) {
    bind_rows(results) %>%
      filter(!is.na(pathway_id))
  } else {
    tibble(
      metabolite_id = character(),
      pathway_id = character(),
      pathway_name = character(),
      pathway_type = character()
    )
  }
}

# -----------------------------
# ID mapping functions
# -----------------------------
#' Map metabolite IDs between different systems using MetaboBase
#' @param ids Character vector of IDs to map
#' @param from_type Source ID type: "hmdb", "kegg", "chebi", "pubchem", "metabolite_id"
#' @param to_type Target ID type: "hmdb", "kegg", "chebi", "pubchem", "metabolite_id"
#' @param metabobase MetaboBase object
#' @return Named character vector mapping input IDs to output IDs
metabobase_map_ids <- function(ids, from_type, to_type, metabobase) {
  if (is.null(metabobase$id_xref) || !is.data.frame(metabobase$id_xref)) {
    warning("MetaboBase has no ID cross-reference table")
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }

  from_col <- paste0(from_type, "_id")
  to_col <- paste0(to_type, "_id")

  # Handle "metabolite_id" as special case (no "_id" suffix)
  if (from_type == "metabolite_id") from_col <- "metabolite_id"
  if (to_type == "metabolite_id") to_col <- "metabolite_id"

  if (!from_col %in% names(metabobase$id_xref)) {
    warning(paste("Source ID type not found in MetaboBase:", from_type))
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }
  if (!to_col %in% names(metabobase$id_xref)) {
    warning(paste("Target ID type not found in MetaboBase:", to_type))
    return(stats::setNames(rep(NA_character_, length(ids)), ids))
  }

  xref <- metabobase$id_xref
  mapping <- xref[[to_col]]
  names(mapping) <- xref[[from_col]]

  result <- mapping[as.character(ids)]
  names(result) <- ids
  result
}

# -----------------------------
# Validate / load / save
# -----------------------------
metabobase_validate <- function(x) {
  errs <- character(0)

  if (!is.list(x)) errs <- c(errs, "MetaboBase object is not a list.")
  if (is.list(x)) {
    req_fields <- c("schema_version", "metabobase_version", "created", "annot_long", "terms_by_id", "metabolite_meta")
    missing <- setdiff(req_fields, names(x))
    if (length(missing)) errs <- c(errs, paste0("Missing fields: ", paste(missing, collapse = ", ")))

    if (!("schema_version" %in% names(x)) || !is.numeric(x$schema_version)) {
      errs <- c(errs, "schema_version missing or not numeric.")
    }

    if (!("annot_long" %in% names(x)) || !is.data.frame(x$annot_long)) {
      errs <- c(errs, "annot_long missing or not a data.frame.")
    } else {
      needed <- c("metabolite_id", "pathway_id", "pathway_type")
      miss2 <- setdiff(needed, names(x$annot_long))
      if (length(miss2)) errs <- c(errs, paste0("annot_long missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("terms_by_id" %in% names(x)) || !is.data.frame(x$terms_by_id)) {
      errs <- c(errs, "terms_by_id missing or not a data.frame.")
    } else {
      needed <- c("pathway_type", "pathway_id", "term_metabolites", "n_metabolites")
      miss2 <- setdiff(needed, names(x$terms_by_id))
      if (length(miss2)) errs <- c(errs, paste0("terms_by_id missing columns: ", paste(miss2, collapse = ", ")))
    }

    if (!("metabolite_meta" %in% names(x)) || !is.data.frame(x$metabolite_meta)) {
      errs <- c(errs, "metabolite_meta missing or not a data.frame.")
    } else {
      needed <- c("metabolite_id", "name")
      miss2 <- setdiff(needed, names(x$metabolite_meta))
      if (length(miss2)) errs <- c(errs, paste0("metabolite_meta missing columns: ", paste(miss2, collapse = ", ")))
    }
  }

  list(ok = length(errs) == 0, errors = errs)
}

metabobase_save <- function(x, file) {
  v <- metabobase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  saveRDS(x, file = file)
  invisible(TRUE)
}

metabobase_load <- function(file) {
  x <- readRDS(file)
  v <- metabobase_validate(x)
  if (!v$ok) stop(paste(v$errors, collapse = "\n"))
  x
}

metabobase_summary_lines <- function(x) {
  v <- metabobase_validate(x)
  if (!v$ok) return(c("Invalid MetaboBase object:", paste0("- ", v$errors)))

  n_ids <- length(unique(x$annot_long$metabolite_id))

  pathway_counts <- x$terms_by_id %>%
    count(pathway_type, name = "n_pathways") %>%
    arrange(pathway_type)

  pathway_str <- if (nrow(pathway_counts)) {
    paste(pathway_counts$pathway_type, pathway_counts$n_pathways, sep = ": ", collapse = ", ")
  } else {
    "none"
  }

  class_count <- if (!is.null(x$class_mappings) && nrow(x$class_mappings) > 0) {
    length(unique(x$class_mappings$class[!is.na(x$class_mappings$class)]))
  } else {
    0
  }

  c(
    paste0("Library name: ", x$library_name %||% "(none)"),
    paste0("Created: ", format(x$created)),
    paste0("Rows in source: ", x$n_raw_rows),
    paste0("Total metabolites: ", x$n_metabolites %||% n_ids),
    paste0("Metabolites with pathway annotations: ", n_ids),
    paste0("Pathways: ", pathway_str),
    paste0("Chemical classes: ", class_count)
  )
}

#' Merge pathway annotations into an existing MetaboBase
#' @param metabobase Existing MetaboBase object
#' @param new_annotations Data frame with columns: metabolite_id, pathway_id, pathway_name, pathway_type
#' @return Updated MetaboBase object
metabobase_merge_annotations <- function(metabobase, new_annotations) {
  if (!is.data.frame(new_annotations) || nrow(new_annotations) == 0) {
    return(metabobase)
  }

  # Merge into annot_long
  metabobase$annot_long <- bind_rows(
    metabobase$annot_long,
    new_annotations %>%
      select(any_of(c("metabolite_id", "pathway_id", "pathway_name", "pathway_type")))
  ) %>%
    distinct(metabolite_id, pathway_id, pathway_type, .keep_all = TRUE)

  # Rebuild terms_by_id
  metabobase$terms_by_id <- metabobase$annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  metabobase
}

# -----------------------------
# Pre-built Library Functions
# -----------------------------

#' List available pre-built MetaboBase libraries
#' @param metabobase_dir Directory containing .metabobase files (default: metabobase/)
#' @return Data frame with library info (name, organism, pathways, file)
metabobase_list_libraries <- function(metabobase_dir = NULL) {
  if (is.null(metabobase_dir)) {
    # Try to find metabobase directory relative to app
    candidates <- c(
      "metabobase",
      file.path(getwd(), "metabobase"),
      file.path(dirname(getwd()), "metabobase")
    )
    for (cand in candidates) {
      if (dir.exists(cand)) {
        metabobase_dir <- cand
        break
      }
    }
  }

  if (is.null(metabobase_dir) || !dir.exists(metabobase_dir)) {
    return(data.frame(
      name = character(0),
      organism = character(0),
      n_metabolites = integer(0),
      n_pathways = integer(0),
      file = character(0),
      stringsAsFactors = FALSE
    ))
  }

  files <- list.files(metabobase_dir, pattern = "\\.metabobase$", full.names = TRUE)

  if (length(files) == 0) {
    return(data.frame(
      name = character(0),
      organism = character(0),
      n_metabolites = integer(0),
      n_pathways = integer(0),
      file = character(0),
      stringsAsFactors = FALSE
    ))
  }

  results <- lapply(files, function(f) {
    tryCatch({
      mb <- readRDS(f)
      data.frame(
        name = mb$library_name %||% basename(f),
        organism = mb$organism %||% "unknown",
        n_metabolites = mb$n_metabolites %||% length(unique(mb$annot_long$metabolite_id)),
        n_pathways = nrow(mb$terms_by_id),
        file = f,
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  do.call(rbind, Filter(Negate(is.null), results))
}

#' Build a complete KEGG pathway library for an organism
#' @param organism KEGG organism code (e.g., "hsa" for human, "mmu" for mouse)
#' @param library_name Name for the library
#' @param output_file Output file path (optional, will save if provided)
#' @param progress_callback Progress callback function(message)
#' @return MetaboBase object with KEGG pathways
metabobase_build_kegg_library <- function(
    organism = "hsa",
    library_name = NULL,
    output_file = NULL,
    progress_callback = function(msg) message(msg)
) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install with: install.packages('httr')")
  }

  organism_names <- c(
    hsa = "Homo sapiens (Human)",
    mmu = "Mus musculus (Mouse)",
    rno = "Rattus norvegicus (Rat)",
    dme = "Drosophila melanogaster (Fruit fly)",
    cel = "Caenorhabditis elegans (Nematode)",
    sce = "Saccharomyces cerevisiae (Yeast)",
    eco = "Escherichia coli",
    ath = "Arabidopsis thaliana"
  )

  org_name <- organism_names[organism] %||% organism
  library_name <- library_name %||% paste0("KEGG ", org_name)

  progress_callback(paste0("Building KEGG library for ", org_name))

 # Step 1: Get all metabolic pathways for organism
  progress_callback("Fetching pathway list from KEGG...")
  pathway_url <- paste0("https://rest.kegg.jp/list/pathway/", organism)
  resp <- httr::GET(pathway_url, httr::timeout(60))

  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch KEGG pathway list")
  }

  pathway_text <- httr::content(resp, "text", encoding = "UTF-8")
  pathway_lines <- strsplit(pathway_text, "\n")[[1]]
  pathway_lines <- pathway_lines[nzchar(pathway_lines)]

  pathways <- lapply(pathway_lines, function(line) {
    parts <- strsplit(line, "\t")[[1]]
    if (length(parts) >= 2) {
      list(
        id = gsub("^path:", "", parts[1]),
        name = parts[2]
      )
    } else NULL
  })
  pathways <- Filter(Negate(is.null), pathways)

  progress_callback(sprintf("Found %d pathways", length(pathways)))

  # Step 2: Get compounds for each pathway
  all_annotations <- list()
  all_compounds <- list()

  for (i in seq_along(pathways)) {
    pw <- pathways[[i]]

    if (i %% 10 == 0 || i == length(pathways)) {
      progress_callback(sprintf("Processing pathway %d/%d: %s", i, length(pathways), pw$name))
    }

    # Get pathway compounds
    cpd_url <- paste0("https://rest.kegg.jp/link/compound/", pw$id)
    cpd_resp <- httr::GET(cpd_url, httr::timeout(30))

    if (httr::status_code(cpd_resp) == 200) {
      cpd_text <- httr::content(cpd_resp, "text", encoding = "UTF-8")
      cpd_lines <- strsplit(cpd_text, "\n")[[1]]
      cpd_lines <- cpd_lines[nzchar(cpd_lines)]

      for (cpd_line in cpd_lines) {
        parts <- strsplit(cpd_line, "\t")[[1]]
        if (length(parts) >= 2) {
          cpd_id <- gsub("^cpd:", "", parts[2])
          all_annotations <- c(all_annotations, list(data.frame(
            metabolite_id = cpd_id,
            pathway_id = pw$id,
            pathway_name = pw$name,
            pathway_type = "KEGG",
            stringsAsFactors = FALSE
          )))
          all_compounds[[cpd_id]] <- TRUE
        }
      }
    }

    Sys.sleep(0.1)  # Rate limiting
  }

  if (length(all_annotations) == 0) {
    stop("No compound-pathway associations found")
  }

  annot_long <- do.call(rbind, all_annotations)
  progress_callback(sprintf("Found %d compound-pathway associations", nrow(annot_long)))

  # Step 3: Get compound names
  progress_callback("Fetching compound names...")
  compound_ids <- names(all_compounds)
  compound_names <- stats::setNames(compound_ids, compound_ids)  # Default: ID as name

  # Batch fetch compound info (KEGG allows ~10 at a time)
  batch_size <- 10
  n_batches <- ceiling(length(compound_ids) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(compound_ids))
    batch <- compound_ids[start_idx:end_idx]

    if (i %% 5 == 0 || i == n_batches) {
      progress_callback(sprintf("Fetching compound info batch %d/%d", i, n_batches))
    }

    for (cpd_id in batch) {
      tryCatch({
        cpd_url <- paste0("https://rest.kegg.jp/get/", cpd_id)
        cpd_resp <- httr::GET(cpd_url, httr::timeout(30))

        if (httr::status_code(cpd_resp) == 200) {
          cpd_text <- httr::content(cpd_resp, "text", encoding = "UTF-8")

          # Parse NAME field
          name_match <- regmatches(cpd_text, regexec("NAME\\s+([^\\n;]+)", cpd_text))[[1]]
          if (length(name_match) >= 2) {
            compound_names[cpd_id] <- trimws(name_match[2])
          }
        }
      }, error = function(e) NULL)

      Sys.sleep(0.05)
    }
  }

  # Build metabolite_meta
  metabolite_meta <- data.frame(
    metabolite_id = compound_ids,
    name = compound_names[compound_ids],
    kegg_id = compound_ids,
    stringsAsFactors = FALSE
  )

  # Build terms_by_id
  terms_by_id <- annot_long %>%
    group_by(pathway_type, pathway_id, pathway_name) %>%
    summarise(
      term_metabolites = list(sort(unique(metabolite_id))),
      n_metabolites = length(unique(metabolite_id)),
      .groups = "drop"
    )

  # Build final metabobase
  metabobase <- list(
    schema_version = METABOBASE_SCHEMA_VERSION,
    metabobase_version = 1L,
    library_name = library_name,
    organism = org_name,
    organism_code = organism,
    source = "KEGG",
    created = Sys.time(),
    n_raw_rows = nrow(annot_long),
    n_metabolites = length(compound_ids),
    annot_long = annot_long,
    terms_by_id = terms_by_id,
    class_mappings = data.frame(
      metabolite_id = character(0),
      class = character(0),
      subclass = character(0),
      superclass = character(0),
      stringsAsFactors = FALSE
    ),
    id_xref = data.frame(
      metabolite_id = compound_ids,
      kegg_id = compound_ids,
      stringsAsFactors = FALSE
    ),
    metabolite_meta = metabolite_meta
  )

  progress_callback(sprintf("Built MetaboBase: %d metabolites, %d pathways",
                            length(compound_ids), nrow(terms_by_id)))

  # Save if output file provided
 if (!is.null(output_file)) {
    metabobase_save(metabobase, output_file)
    progress_callback(sprintf("Saved to: %s", output_file))
  }

  metabobase
}

#' Load a pre-built MetaboBase library by name
#' @param name Library name (partial match supported)
#' @param metabobase_dir Directory containing .metabobase files
#' @return MetaboBase object
metabobase_load_library <- function(name, metabobase_dir = NULL) {
  libs <- metabobase_list_libraries(metabobase_dir)

  if (nrow(libs) == 0) {
    stop("No MetaboBase libraries found. Build one with metabobase_build_kegg_library()")
  }

  # Try exact match first
  idx <- which(tolower(libs$name) == tolower(name))

  # Try partial match
  if (length(idx) == 0) {
    idx <- grep(name, libs$name, ignore.case = TRUE)
  }

  if (length(idx) == 0) {
    stop(paste0(
      "Library '", name, "' not found.\nAvailable libraries:\n  ",
      paste(libs$name, collapse = "\n  ")
    ))
  }

  if (length(idx) > 1) {
    warning(paste0("Multiple matches found, using first: ", libs$name[idx[1]]))
    idx <- idx[1]
  }

  metabobase_load(libs$file[idx])
}

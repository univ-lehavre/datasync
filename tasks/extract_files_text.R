#!/usr/bin/env Rscript
# Extraction de texte depuis les fichiers binaires d'un instrument REDCap.
# Usage: Rscript tasks/extract_files_text.R --task /path/to/task.json
# Sortie stdout JSON: {"ok":true,"n_extracted":18,"n_skipped":2,"n_biblio_refs":143}

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(args, regexpr("(?<=--file=).*", args, perl = TRUE))
  if (length(m) == 1L && nchar(m) > 0L) {
    normalizePath(file.path(dirname(normalizePath(m, mustWork = FALSE)), "..", "R"), mustWork = FALSE)
  } else {
    normalizePath(file.path(getwd(), "R"), mustWork = FALSE)
  }
})

source(file.path(script_dir, "config.R"))
source(file.path(script_dir, "extract_text.R"))
check_extract_packages()

suppressPackageStartupMessages({
  library(pdftools)
  library(officer)
  library(rvest)
})

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

required_fields <- c("files_dir", "output_dir", "field")
missing_fields <- required_fields[!required_fields %in% names(task)]
if (length(missing_fields) > 0L) {
  stop(paste("Champs task manquants :", paste(missing_fields, collapse = ", ")))
}

dir_create(task$output_dir, recurse = TRUE)

result <- run_extract_pipeline(
  files_dir  = task$files_dir,
  output_dir = task$output_dir,
  field      = task$field
)

cat(toJSON(result, auto_unbox = TRUE))

#!/usr/bin/env Rscript
# Télécharge les fichiers binaires (portraits, PDFs) pour les enregistrements identifiables
# Usage: Rscript tasks/download_files.R --task /path/to/task.json
# Sortie stdout JSON: { "ok": true, "n_files": 12, "paths": [...] }

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(args, regexpr("(?<=--file=).*", args, perl = TRUE))
  if (length(m) == 1L && nchar(m) > 0L) {
    dirname(normalizePath(dirname(m), mustWork = FALSE))
  } else {
    "."
  }
})

source(file.path(script_dir, "config.R"))
source(file.path(script_dir, "api_redcap.R"))
source(file.path(script_dir, "download.R"))

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

config <- task$instrument
config$file_fields <- as.character(config$file_fields)

if (length(config$file_fields) == 0L) {
  cat(toJSON(list(ok = TRUE, n_files = 0L, paths = character(0)), auto_unbox = TRUE))
  quit(status = 0)
}

metadata <- fromJSON(task$metadata_path, simplifyDataFrame = TRUE)
id_field <- metadata$field_name[1]

ident_records <- read_csv(task$ident_csv_path, show_col_types = FALSE)

files <- download_instrument_files(
  task$api_url, task$token, ident_records, id_field, config
)

cat(toJSON(list(
  ok = TRUE,
  n_files = length(files),
  paths = files
), auto_unbox = TRUE))

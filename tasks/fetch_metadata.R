#!/usr/bin/env Rscript
# Récupère instruments + metadata depuis REDCap et les écrit dans downloads/metadata/
# Usage: Rscript tasks/fetch_metadata.R --task /path/to/task.json
# Sortie: downloads/metadata/instruments.json, metadata.json, dictionnaire.csv

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
source(file.path(script_dir, "api_redcap.R"))
source(file.path(script_dir, "rapport.R"))

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

metadata_dir <- if (!is.null(task$metadata_dir) && nchar(task$metadata_dir) > 0) {
  task$metadata_dir
} else {
  METADATA_DIR
}
dir_create(metadata_dir, recurse = TRUE)

instruments <- get_instruments(task$api_url, task$token)
write(
  toJSON(instruments, pretty = TRUE, auto_unbox = TRUE),
  file.path(metadata_dir, "instruments.json")
)

metadata <- get_metadata(task$api_url, task$token)
write(
  toJSON(metadata, pretty = TRUE, auto_unbox = TRUE),
  file.path(metadata_dir, "metadata.json")
)

generate_variables_csv(metadata, file.path(metadata_dir, "dictionnaire.csv"))

cat(toJSON(list(
  ok = TRUE,
  n_instruments = nrow(instruments),
  n_variables = nrow(metadata)
), auto_unbox = TRUE))

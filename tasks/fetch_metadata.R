#!/usr/bin/env Rscript
# Récupère instruments + metadata depuis REDCap et les écrit dans downloads/metadata/
# Usage: Rscript tasks/fetch_metadata.R --task /path/to/task.json
# Sortie: downloads/metadata/instruments.json, metadata.json, dictionnaire.csv

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
source(file.path(script_dir, "rapport.R"))

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

dir_create(METADATA_DIR, recurse = TRUE)

instruments <- get_instruments(task$api_url, task$token)
write(
  toJSON(instruments, pretty = TRUE, auto_unbox = TRUE),
  file.path(METADATA_DIR, "instruments.json")
)

metadata <- get_metadata(task$api_url, task$token)
write(
  toJSON(metadata, pretty = TRUE, auto_unbox = TRUE),
  file.path(METADATA_DIR, "metadata.json")
)

generate_variables_csv(metadata, file.path(METADATA_DIR, "dictionnaire.csv"))

cat(toJSON(list(
  ok = TRUE,
  n_instruments = nrow(instruments),
  n_variables = nrow(metadata)
), auto_unbox = TRUE))

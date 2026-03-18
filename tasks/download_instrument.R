#!/usr/bin/env Rscript
# Télécharge les vagues 2-5 pour un instrument et écrit les CSV dans downloads/data/
# Usage: Rscript tasks/download_instrument.R --task /path/to/task.json
# Sortie stdout JSON: { "ok": true, "n_ident": 12, "n_pseudo": 34, "n_anon": 5, ... }

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
source(file.path(script_dir, "instruments.R"))
source(file.path(script_dir, "download.R"))

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

metadata <- fromJSON(task$metadata_path, simplifyDataFrame = TRUE)
id_field <- metadata$field_name[1]

config <- task$instrument
config$file_fields <- as.character(config$file_fields)

data_dir <- if (!is.null(task$data_dir) && nchar(task$data_dir) > 0) task$data_dir else DATA_DIR
dir_create(data_dir, recurse = TRUE)

result <- download_instrument_data(
  task$api_url, task$token, metadata, id_field, task$audience, config,
  data_dir = data_dir
)

n_ident <- if (!is.null(result$ident_records)) nrow(result$ident_records) else 0L
n_pseudo <- if (!is.null(result$pseudo_records)) nrow(result$pseudo_records) else 0L
n_anon <- if (!is.null(result$anon_records)) nrow(result$anon_records) else 0L

csv_files <- list(
  vague2 = file.path(data_dir, sprintf("vague2_%s_identifiables.csv", config$name)),
  vague3 = file.path(data_dir, sprintf("vague3_%s_pseudonymises.csv", config$name)),
  vague4 = file.path(data_dir, sprintf("vague4_%s_anonymises.csv", config$name)),
  vague5 = file.path(data_dir, sprintf("vague5_%s_statistiques.csv", config$name))
)
# Garder seulement les fichiers qui existent
csv_files <- Filter(file.exists, csv_files)

cat(toJSON(list(
  ok = TRUE,
  n_ident = n_ident,
  n_pseudo = n_pseudo,
  n_anon = n_anon,
  n_stats_no_response = result$stats$no_response,
  n_stats_audience_filtered = result$stats$audience_filtered,
  n_stats_aggregated = result$stats$aggregated,
  csv_files = csv_files
), auto_unbox = TRUE))

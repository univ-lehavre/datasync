#!/usr/bin/env Rscript
# Génère le fichier Quarto (.qmd) depuis les résultats téléchargés
# Usage: Rscript tasks/generate_qmd.R --task /path/to/task.json
# Sortie stdout JSON: { "ok": true, "qmd_path": "..." }

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
source(file.path(script_dir, "instruments.R"))
source(file.path(script_dir, "download.R"))
source(file.path(script_dir, "rapport.R"))

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

metadata <- fromJSON(task$metadata_path, simplifyDataFrame = TRUE)

# Reconstruire les objets result attendus par generate_report_qmd()
# à partir des CSV sur disque + des counts dans la task
results <- lapply(task$results, function(r) {
  name <- r$name

  tpl <- "show_col_types = FALSE), error = function(e) NULL)"
  read_safe <- function(path) {
    tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
  }

  list(
    config = list(
      name = name,
      label = r$label,
      has_identifiers = isTRUE(r$has_identifiers),
      file_fields = if (is.null(r$file_fields)) character(0) else as.character(r$file_fields)
    ),
    ident_records = read_safe(file.path(DATA_DIR, sprintf("vague2_%s_identifiables.csv", name))),
    pseudo_records = read_safe(file.path(DATA_DIR, sprintf("vague3_%s_pseudonymises.csv", name))),
    anon_records = read_safe(file.path(DATA_DIR, sprintf("vague4_%s_anonymises.csv", name))),
    stats = list(
      no_response = as.integer(r$n_stats_no_response),
      audience_filtered = as.integer(r$n_stats_audience_filtered),
      aggregated = as.integer(r$n_stats_aggregated)
    ),
    files = character(0)
  )
})

dir_create(REPORTS_DIR, recurse = TRUE)

qmd_path <- generate_report_qmd(task$audience, metadata, results, task$qmd_path)

cat(toJSON(list(ok = TRUE, qmd_path = qmd_path), auto_unbox = TRUE))

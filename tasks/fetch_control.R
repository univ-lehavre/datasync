#!/usr/bin/env Rscript
# Récupère les champs de contrôle d'un instrument, supporte dateRangeBegin pour détecter les modifications.
# Usage: Rscript tasks/fetch_control.R --task /path/to/task.json
# Sortie stdout JSON: { "has_changes": true/false, "changed_record_ids": [...], "n_total": 45 }

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

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

control_fields <- c(task$id_field, task$id_level_field, task$audience_field)

# Requête complète pour compter le total
all_records <- get_records_with_fields_raw(task$api_url, task$token, control_fields)
n_total <- if (is.null(all_records)) 0L else nrow(all_records)

# Si dateRangeBegin fourni : requête ciblée pour détecter les changements
if (!is.null(task$date_range_begin) && nchar(task$date_range_begin) > 0L) {
  extra <- list(
    rawOrLabel = "label",
    fields = paste(control_fields, collapse = ","),
    dateRangeBegin = task$date_range_begin
  )
  body <- redcap_request(task$api_url, task$token, "record", extra)
  changed <- fromJSON(body, simplifyDataFrame = TRUE)

  if (is.null(changed) || (is.data.frame(changed) && nrow(changed) == 0L)) {
    cat(toJSON(list(
      has_changes = FALSE,
      changed_record_ids = character(0),
      n_total = n_total
    ), auto_unbox = TRUE))
  } else {
    cat(toJSON(list(
      has_changes = TRUE,
      changed_record_ids = as.character(changed[[task$id_field]]),
      n_total = n_total
    ), auto_unbox = TRUE))
  }
} else {
  # Pas de dateRangeBegin : toujours considéré comme changé (premier run)
  cat(toJSON(list(
    has_changes = TRUE,
    changed_record_ids = character(0),
    n_total = n_total
  ), auto_unbox = TRUE))
}

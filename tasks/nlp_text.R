#!/usr/bin/env Rscript
# Analyse NLP d'un champ textuel REDCap (détection langue, lemmatisation, TF-IDF, LDA).
# Usage: Rscript tasks/nlp_text.R --task /path/to/task.json
# Sortie stdout JSON: {"ok":true,"langues":{"fr":12,"en":8},"lda_k":{"fr":3}}

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
source(file.path(script_dir, "nlp.R"))
check_nlp_packages()

suppressPackageStartupMessages({
  library(cld3)
  library(udpipe)
  library(tidytext)
  library(stopwords)
  library(topicmodels)
  library(tm)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
task_idx <- which(args == "--task")
if (length(task_idx) == 0L) stop("Argument --task manquant")
task <- fromJSON(args[task_idx + 1L])

required_fields <- c("csv_path", "id_field", "output_dir")
missing_fields <- required_fields[!required_fields %in% names(task)]
if (length(missing_fields) > 0L) {
  stop(paste("Champs task manquants :", paste(missing_fields, collapse = ", ")))
}
if (is.null(task$field) && is.null(task$fields)) {
  stop("Champs task manquants : field ou fields")
}

# Accepte soit "field" (scalaire) soit "fields" (liste)
field_arg <- if (!is.null(task$fields)) as.character(task$fields) else task$field

dir_create(task$output_dir, recurse = TRUE)

result <- run_nlp_pipeline(
  csv_path                   = task$csv_path,
  field                      = field_arg,
  id_field                   = task$id_field,
  output_dir                 = task$output_dir,
  field_identifiables_path   = task$field_identifiables_path,
  profile_identifiables_path = task$profile_identifiables_path,
  id_level_field             = task$id_level_field
)

cat(toJSON(result, auto_unbox = TRUE))

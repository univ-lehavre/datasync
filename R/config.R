# config.R — Imports, constantes, utilitaires console, chargement .env

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(digest)
  library(readr)
  library(dplyr)
  library(stringr)
  library(fs)
})

# ---------------------------------------------------------------------------
# Constantes
# ---------------------------------------------------------------------------
DOWNLOADS_DIR <- "downloads"
METADATA_DIR <- file.path(DOWNLOADS_DIR, "metadata")
DATA_DIR <- file.path(DOWNLOADS_DIR, "data")
REPORTS_DIR <- "reports"
RAPPORT_QMD <- "rapport-analyse-premiere-vague.qmd"

# ---------------------------------------------------------------------------
# Utilitaires console
# ---------------------------------------------------------------------------
cat_green <- function(...) cat(paste0("\033[32m", ..., "\033[0m"))
cat_cyan <- function(...) cat(paste0("\033[36m", ..., "\033[0m"))
cat_yellow <- function(...) cat(paste0("\033[33m", ..., "\033[0m"))
cat_bold <- function(...) cat(paste0("\033[1m", ..., "\033[0m"))
cat_red <- function(...) cat(paste0("\033[31m", ..., "\033[0m"))
cat_gray <- function(...) cat(paste0("\033[90m", ..., "\033[0m"))

str_green <- function(x) paste0("\033[32m", x, "\033[0m")
str_cyan <- function(x) paste0("\033[36m", x, "\033[0m")
str_yellow <- function(x) paste0("\033[33m", x, "\033[0m")
str_bold <- function(x) paste0("\033[1m", x, "\033[0m")
str_red <- function(x) paste0("\033[31m", x, "\033[0m")
str_gray <- function(x) paste0("\033[90m", x, "\033[0m")

# ---------------------------------------------------------------------------
# Chargement du .env
# ---------------------------------------------------------------------------
load_env <- function() {
  if (!file.exists(".env")) {
    return(invisible(NULL))
  }
  lines <- readLines(".env", warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "#")) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      key <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      value <- gsub('^["\']|["\']$', "", value)
      do.call(Sys.setenv, setNames(list(value), key))
    }
  }
}

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
get_config <- function() {
  api_url <- Sys.getenv("REDCAP_API_URL")
  token <- Sys.getenv("REDCAP_TOKEN")

  if (nchar(token) == 0) {
    stop("REDCAP_TOKEN non défini. Créez un fichier .env avec REDCAP_TOKEN=votre_token")
  }
  if (nchar(api_url) == 0) {
    stop("REDCAP_API_URL non défini, créez un fichier .env avec REDCAP_API_URL=https://")
  }
  list(api_url = api_url, token = token)
}

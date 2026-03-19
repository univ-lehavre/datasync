# extract_text.R — Extraction de texte depuis des fichiers binaires (PDF, DOCX, TXT, HTML)
# Dépendances R : pdftools, officer, rvest
# Dépendance Python : refextract (uv add refextract)

check_extract_packages <- function() {
  required <- c("pdftools", "officer", "rvest")
  missing_pkgs <- required[
    !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0L) {
    stop(paste0(
      "Packages R manquants pour l'extraction de texte. Installez avec :\n",
      "  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))\n"
    ))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Extraction de texte brut selon le type de fichier
# ---------------------------------------------------------------------------

extract_text_from_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  tryCatch(
    switch(ext,
      pdf  = extract_pdf(path),
      docx = extract_docx(path),
      txt  = extract_txt(path),
      html = extract_html(path),
      htm  = extract_html(path),
      NULL
    ),
    error = function(e) {
      message(sprintf("  [extract] Erreur sur %s : %s", basename(path), conditionMessage(e)))
      NULL
    }
  )
}

extract_pdf <- function(path) {
  pages <- pdftools::pdf_text(path)
  paste(pages, collapse = "\n")
}

extract_docx <- function(path) {
  doc <- officer::read_docx(path)
  summary_df <- officer::docx_summary(doc)
  paragraphs <- summary_df$text[summary_df$content_type == "paragraph"]
  paragraphs <- paragraphs[!is.na(paragraphs) & nchar(trimws(paragraphs)) > 0L]
  paste(paragraphs, collapse = "\n")
}

extract_txt <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(lines, collapse = "\n")
}

extract_html <- function(path) {
  html <- rvest::read_html(path)
  rvest::html_text(html, trim = TRUE)
}

# ---------------------------------------------------------------------------
# Détection et extraction de la section bibliographie
# ---------------------------------------------------------------------------

BIBLIO_HEADER_PATTERN <- "^\\s*(r[eé]f[eé]rences?|bibliograph|works?\\s+cited|sources?|ouvrages?\\s+cit[eé]s?)\\s*$"

extract_biblio_section <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  # Cherche le dernier en-tête de bibliographie
  biblio_idx <- grep(BIBLIO_HEADER_PATTERN, lines, ignore.case = TRUE)
  if (length(biblio_idx) == 0L) {
    return(character(0L))
  }
  start <- biblio_idx[length(biblio_idx)] + 1L
  if (start > length(lines)) {
    return(character(0L))
  }
  lines[start:length(lines)]
}

# ---------------------------------------------------------------------------
# Extraction des titres dans les lignes de bibliographie
#
# Patterns supportés :
#   (1) Auteur, P. (2020). Titre de la publication. Journal...
#       → le titre est entre le premier "). " et le prochain ". "
#   (2) [1] Auteur P. "Titre entre guillemets". Journal...
#   (3) [1] ou 1. suivi d'une ligne de texte (entrée numérotée)
#   (4) Ligne autonome ressemblant à un titre (capitalisée, sans ". ")
# ---------------------------------------------------------------------------

parse_biblio_titles <- function(biblio_lines) {
  if (length(biblio_lines) == 0L) {
    return(data.frame(titre_cite = character(0L), stringsAsFactors = FALSE))
  }

  lines <- trimws(biblio_lines)
  lines <- lines[nchar(lines) > 5L]
  titles <- character(0L)

  for (line in lines) {
    # Pattern 1 : Auteur (Année). Titre. Suite...
    m1 <- regmatches(line, regexpr("\\(\\d{4}[a-z]?\\)\\.\\s+(.+?)\\.", line, perl = TRUE))
    if (length(m1) > 0L) {
      title <- sub("\\(\\d{4}[a-z]?\\)\\.\\s+", "", m1)
      title <- sub("\\.$", "", title)
      titles <- c(titles, trimws(title))
      next
    }
    # Pattern 2 : "Titre entre guillemets"
    m2 <- regmatches(line, regexpr('"([^"]{10,})"', line, perl = TRUE))
    if (length(m2) > 0L) {
      title <- gsub('^"|"$', "", m2)
      titles <- c(titles, trimws(title))
      next
    }
    # Pattern 3 : entrée numérotée [1] ou 1. → texte
    m3 <- sub("^(\\[\\d+\\]|\\d+\\.)\\s+", "", line)
    if (m3 != line && nchar(m3) > 10L) {
      # Prend uniquement la première phrase comme titre approximatif
      first_sentence <- sub("\\.\\s+.*$", "", m3)
      titles <- c(titles, trimws(first_sentence))
    }
  }

  titles <- unique(titles[nchar(titles) > 5L])
  data.frame(titre_cite = titles, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Pipeline complet pour un répertoire de fichiers
# ---------------------------------------------------------------------------

run_extract_pipeline <- function(files_dir, output_dir, field) {
  files <- list.files(files_dir, full.names = TRUE)
  pattern <- paste0("_", field, "\\.[^.]+$")
  files <- files[grepl(pattern, basename(files))]

  texts_dir <- file.path(output_dir, "texts")
  dir.create(texts_dir, recursive = TRUE, showWarnings = FALSE)

  texts_rows <- list()
  n_skipped <- 0L

  for (f in files) {
    hashed_id <- sub(paste0("_", field, "\\.[^.]+$"), "", basename(f))
    text <- extract_text_from_file(f)
    if (is.null(text) || nchar(trimws(text)) == 0L) {
      n_skipped <- n_skipped + 1L
      next
    }
    writeLines(text, file.path(texts_dir, paste0(hashed_id, ".txt")), useBytes = FALSE)
    texts_rows[[length(texts_rows) + 1L]] <- data.frame(
      hashed_id = hashed_id,
      text = text,
      stringsAsFactors = FALSE
    )
  }

  texts_df <- if (length(texts_rows) > 0L) {
    do.call(rbind, texts_rows)
  } else {
    data.frame(hashed_id = character(), text = character(), stringsAsFactors = FALSE)
  }

  write_csv(texts_df, file.path(output_dir, "texts.csv"))

  list(ok = TRUE, n_extracted = nrow(texts_df), n_skipped = n_skipped)
}

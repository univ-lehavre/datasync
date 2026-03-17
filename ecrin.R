#!/usr/bin/env Rscript
# CLI ECRIN - analyse première vague
#
# Usage:
#   Rscript ecrin.R              # Lance le menu interactif
#   Rscript ecrin.R metadata     # Récupère instruments + métadonnées + dictionnaire.csv
#   Rscript ecrin.R export       # Exporte les données en CSV
#   Rscript ecrin.R diffusion    # Récupère les paramètres de diffusion
#   Rscript ecrin.R rapport profils  # Génère le rapport des profils
#   Rscript ecrin.R clean        # Supprime les fichiers générés

script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(args, regexpr("(?<=--file=).*", args, perl = TRUE))
  if (length(m) == 1L && nchar(m) > 0L) dirname(normalizePath(m, mustWork = FALSE)) else "."
})

source(file.path(script_dir, "R/config.R"))
source(file.path(script_dir, "R/api_redcap.R"))
source(file.path(script_dir, "R/instruments.R"))
source(file.path(script_dir, "R/download.R"))
source(file.path(script_dir, "R/rapport.R"))
source(file.path(script_dir, "R/display.R"))

# ---------------------------------------------------------------------------
# Commandes CLI
# ---------------------------------------------------------------------------

cmd_export <- function() {
  cfg <- get_config()
  cat(sprintf("\n%s\n\n", str_bold("Export des données REDCap")))
  dir_create(DATA_DIR, recurse = TRUE)

  cat("  Récupération des métadonnées...\n")
  metadata <- get_metadata(cfg$api_url, cfg$token)
  cat(str_green("  \u2713 "), "Métadonnées récupérées\n", sep = "")
  id_field <- metadata$field_name[1]

  cat("  Récupération des enregistrements...\n")
  records <- get_records(cfg$api_url, cfg$token, id_field)
  cat(str_green("  \u2713 "), "Enregistrements récupérés\n", sep = "")

  if (is.null(records) || nrow(records) == 0) {
    cat(sprintf("\n%s Aucune donnée à exporter.\n\n", str_yellow("\u26a0")))
    return(invisible(NULL))
  }

  cat("  Export CSV...\n")
  keys <- sort(names(records))
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")
  out_path <- file.path(DATA_DIR, sprintf("data_export_%s.csv", timestamp))
  write_csv(records[, keys], out_path)
  cat(str_green("  \u2713 "), "Export CSV terminé\n", sep = "")

  cat(sprintf(
    "\n%s %s enregistrements exportés\n",
    str_green("\u2713"), str_bold(as.character(nrow(records)))
  ))
  cat(sprintf("  \u2192 %s\n\n", str_cyan(out_path)))

  cat(sprintf("  %s\n\n", str_bold("Aperçu des données:")))
  preview_cols <- keys[seq_len(min(6L, length(keys)))]
  print_records_table(records, preview_cols, max_rows = 5)
}

cmd_metadata <- function() {
  cfg <- get_config()
  cat(sprintf("\n%s\n", str_bold("Récupération des métadonnées REDCap")))
  cat(sprintf("  URL: %s\n\n", str_cyan(cfg$api_url)))
  dir_create(DATA_DIR, recurse = TRUE)

  cat("  Récupération des instruments...\n")
  instruments <- get_instruments(cfg$api_url, cfg$token)
  write(
    toJSON(instruments, pretty = TRUE, auto_unbox = TRUE),
    file.path(DATA_DIR, "instruments.json")
  )
  cat(str_green("  \u2713 "), "Instruments récupérés\n", sep = "")

  cat("  Récupération du dictionnaire de données...\n")
  metadata <- get_metadata(cfg$api_url, cfg$token)
  write(
    toJSON(metadata, pretty = TRUE, auto_unbox = TRUE),
    file.path(DATA_DIR, "metadata.json")
  )
  cat(str_green("  \u2713 "), "Métadonnées récupérées\n", sep = "")

  cat("  Génération du dictionnaire CSV...\n")
  generate_variables_csv(metadata, file.path(DATA_DIR, "dictionnaire.csv"))
  cat(str_green("  \u2713 "), "Dictionnaire généré\n", sep = "")

  cat(sprintf("\n%s Terminé!\n\n", str_green("\u2713")))
  cat(sprintf("  %s %d instruments, %d variables\n\n", str_bold("REDCap:"), nrow(instruments), nrow(metadata)))
  cat(sprintf("    \u2192 %s\n", str_cyan(file.path(DATA_DIR, "instruments.json"))))
  cat(sprintf("    \u2192 %s\n", str_cyan(file.path(DATA_DIR, "metadata.json"))))
  cat(sprintf("    \u2192 %s\n\n", str_cyan(file.path(DATA_DIR, "dictionnaire.csv"))))
}

cmd_diffusion <- function() {
  cfg <- get_config()
  cat(sprintf("\n%s\n\n", str_bold("Récupération des paramètres de diffusion REDCap")))
  dir_create(DATA_DIR, recurse = TRUE)

  cat("  Récupération des métadonnées...\n")
  metadata <- get_metadata(cfg$api_url, cfg$token)
  cat(str_green("  \u2713 "), "Métadonnées récupérées\n", sep = "")

  cat("  Identification des champs de diffusion...\n")
  diffusion_fields <- find_diffusion_fields(metadata)
  if (nrow(diffusion_fields) == 0) {
    cat(sprintf(
      "\n%s Aucun champ de diffusion trouvé (patterns: *_identification_level, *_audience)\n\n",
      str_yellow("\u26a0")
    ))
    return(invisible(NULL))
  }
  cat(str_green("  \u2713 "), sprintf("%d champs trouvés\n", nrow(diffusion_fields)), sep = "")

  cat("  Récupération des valeurs...\n")
  record_id_field <- metadata$field_name[1]
  field_names <- c(record_id_field, diffusion_fields$field_name)
  records <- get_records_with_fields(cfg$api_url, cfg$token, field_names)
  cat(str_green("  \u2713 "), "Valeurs récupérées\n", sep = "")

  cat("  Export CSV...\n")
  out_path <- file.path(DATA_DIR, "diffusion.csv")
  if (!is.null(records) && nrow(records) > 0) {
    cols_present <- field_names[field_names %in% names(records)]
    write_csv(records[, cols_present, drop = FALSE], out_path)
  }
  cat(str_green("  \u2713 "), "Export CSV terminé\n", sep = "")

  cat(sprintf("\n%s Terminé!\n\n", str_green("\u2713")))
  cat(sprintf("  %s %d champs de diffusion trouvés:\n", str_bold("Diffusion:"), nrow(diffusion_fields)))
  for (i in seq_len(nrow(diffusion_fields))) {
    cat(sprintf(
      "    \u2022 %s (%s)\n",
      str_cyan(diffusion_fields$field_name[i]),
      diffusion_fields$form_name[i]
    ))
  }
  cat(sprintf(
    "\n  %s %d enregistrements\n", str_bold("Participants:"),
    if (!is.null(records)) nrow(records) else 0L
  ))
  cat(sprintf("  %s %s\n", str_bold("Fichier:"), str_cyan(out_path)))

  cat(sprintf("\n  %s\n\n", str_bold("Aperçu des données:")))
  print_records_table(records, field_names, max_rows = 5)

  stat_fields <- field_names[-1]
  print_records_stats(records, stat_fields)
  cat("\n")
}

# ---------------------------------------------------------------------------
# Rapport Quarto : compilation
# ---------------------------------------------------------------------------
cmd_rapport_pdf <- function() {
  cat("\n  Compilation du rapport en PDF...\n")
  tryCatch(
    {
      run_quarto("render", RAPPORT_QMD, "--to", "pdf")
      cat(str_green("  \u2713 "), "Rapport PDF compilé\n", sep = "")
    },
    error = function(e) stop(sprintf("Erreur Quarto: %s", conditionMessage(e)))
  )
  cat("\n")
}

cmd_rapport_html <- function() {
  cat("\n  Compilation du rapport en HTML...\n")
  tryCatch(
    {
      run_quarto("render", RAPPORT_QMD, "--to", "html")
      cat(str_green("  \u2713 "), "Rapport HTML compilé\n", sep = "")
    },
    error = function(e) stop(sprintf("Erreur Quarto: %s", conditionMessage(e)))
  )
  cat("\n")
}

cmd_rapport_preview <- function() {
  cat(sprintf("\n%s\n", str_bold("Lancement du preview Quarto")))
  cat("  Appuyez sur Ctrl+C pour arrêter\n\n")
  system2("quarto", c("preview", RAPPORT_QMD))
}

# ---------------------------------------------------------------------------
# Sélection de l'audience (interactive)
# ---------------------------------------------------------------------------
select_audience <- function() {
  items <- c(
    "Public - Rapport destiné au grand public",
    "Chercheurs - Rapport restreint aux chercheurs autorisés"
  )
  cat("\nPour quelle audience souhaitez-vous générer le rapport ?\n")
  for (i in seq_along(items)) cat(sprintf("  %d. %s\n", i, items[i]))
  cat("Votre choix (1 ou 2) : ")
  choice <- readLines(con = "stdin", n = 1)
  choice <- suppressWarnings(as.integer(trimws(choice)))
  if (is.na(choice) || !choice %in% 1:2) stop("Choix invalide.")
  c("public", "chercheurs")[choice]
}

# ---------------------------------------------------------------------------
# Commande rapport profils (pipeline complet)
# ---------------------------------------------------------------------------
cmd_rapport_profils <- function() {
  cfg <- get_config()
  cat(sprintf("\n%s\n\n", str_bold("Génération du rapport ECRIN")))

  audience <- select_audience()
  cat(sprintf("\n  Audience sélectionnée: %s\n\n", str_cyan(audience)))

  dir_create(DATA_DIR, recurse = TRUE)
  dir_create(REPORTS_DIR, recurse = TRUE)

  # 0. Nettoyage
  cat("  Nettoyage des données précédentes...\n")
  clean_output_dir()
  cat(str_green("  \u2713 "), "Nettoyage terminé\n", sep = "")

  # 1. Métadonnées et instruments
  cat("  Récupération des métadonnées...\n")
  instruments <- get_instruments(cfg$api_url, cfg$token)
  metadata <- get_metadata(cfg$api_url, cfg$token)
  id_field <- metadata$field_name[1]
  cat(str_green("  \u2713 "), "Métadonnées récupérées\n", sep = "")

  # 2. Détecter les instruments avec diffusion
  cat("  Détection des instruments...\n")
  configs <- build_instrument_configs(instruments, metadata)
  if (length(configs) == 0) {
    cat(sprintf("\n%s Aucun instrument avec diffusion trouvé.\n\n", str_yellow("\u26a0")))
    return(invisible(NULL))
  }
  for (cfg_item in configs) {
    extra <- character(0)
    if (cfg_item$has_identifiers) extra <- c(extra, "avec champs identifiants")
    if (length(cfg_item$file_fields) > 0) {
      extra <- c(extra, sprintf("fichiers: %s", paste(cfg_item$file_fields, collapse = ", ")))
    }
    extra_str <- if (length(extra) > 0) sprintf(" (%s)", paste(extra, collapse = ", ")) else ""
    cat(str_green("  \u2713 "), cfg_item$label, extra_str, "\n", sep = "")
  }

  # 3. Télécharger les données par instrument
  cat("  Téléchargement des données par instrument...\n")
  results <- list()
  for (cfg_item in configs) {
    result <- download_instrument_data(cfg$api_url, cfg$token, metadata, id_field, audience, cfg_item)
    n_total <-
      (if (!is.null(result$ident_records)) nrow(result$ident_records) else 0L) +
      (if (!is.null(result$pseudo_records)) nrow(result$pseudo_records) else 0L) +
      (if (!is.null(result$anon_records)) nrow(result$anon_records) else 0L) +
      result$stats$no_response + result$stats$audience_filtered + result$stats$aggregated
    n_stats <- result$stats$no_response + result$stats$audience_filtered + result$stats$aggregated
    stats_str <- if (n_stats > 0) sprintf(" (dont %d en statistiques seules)", n_stats) else ""
    cat(str_green("  \u2713 "), cfg_item$label, ": ", n_total, " enregistrements", stats_str, "\n", sep = "")
    results[[length(results) + 1]] <- result
  }

  # 4. Télécharger les fichiers pour les identifiables
  cat("  Téléchargement des fichiers...\n")
  for (i in seq_along(results)) {
    r <- results[[i]]
    if (length(r$config$file_fields) > 0 && !is.null(r$ident_records) && nrow(r$ident_records) > 0) {
      files <- download_instrument_files(cfg$api_url, cfg$token, r$ident_records, id_field, r$config)
      results[[i]]$files <- files
      cat(str_green("  \u2713 "), r$config$label, ": ", length(files), " fichiers téléchargés\n", sep = "")
    }
  }

  # 5. Générer le QMD
  cat("  Génération du template Quarto...\n")
  qmd_path <- generate_report_qmd(audience, metadata, results)
  cat(str_green("  \u2713 "), "Template QMD généré : ", qmd_path, "\n", sep = "")

  # 6. Compiler en PDF
  cat("  Compilation du rapport en PDF...\n")
  tryCatch(
    {
      run_quarto("render", qmd_path, "--to", "pdf")
      cat(str_green("  \u2713 "), "Rapport compilé\n", sep = "")
    },
    error = function(e) stop(sprintf("Erreur Quarto: %s", conditionMessage(e)))
  )

  pdf_path <- sub("\\.qmd$", ".pdf", qmd_path)
  cat(sprintf("\n%s Rapport généré!\n", str_green("\u2713")))
  cat(sprintf("  \u2192 %s\n\n", str_cyan(pdf_path)))
}

# ---------------------------------------------------------------------------
# Commande clean
# ---------------------------------------------------------------------------
cmd_clean <- function() {
  cat(sprintf("\n%s\n\n", str_bold("Nettoyage des fichiers générés")))

  patterns <- list(
    list(path = DATA_DIR, is_dir = TRUE),
    list(path = REPORTS_DIR, is_dir = TRUE)
  )

  # Suppression des répertoires principaux
  for (p in patterns) {
    if (dir.exists(p$path)) {
      unlink(p$path, recursive = TRUE)
      cat(str_red("  \u2717 "), p$path, "\n", sep = "")
    }
  }

  # Patterns supplémentaires dans le répertoire courant
  extra_patterns <- c("*.pdf", "*.html", "*_files", ".quarto", "*_cache")
  for (pat in extra_patterns) {
    matches <- Sys.glob(pat)
    for (m in matches) {
      if (dir.exists(m)) {
        unlink(m, recursive = TRUE)
      } else {
        file.remove(m)
      }
      cat(str_red("  \u2717 "), m, "\n", sep = "")
    }
  }

  cat(sprintf("\n%s Nettoyage terminé\n\n", str_green("\u2713")))
}

# ---------------------------------------------------------------------------
# Menu interactif
# ---------------------------------------------------------------------------
run_interactive_menu <- function() {
  items <- list(
    list(name = "Métadonnées", desc = "Récupère instruments + métadonnées + dictionnaire.csv", action = cmd_metadata),
    list(name = "Export", desc = "Exporte les données en CSV", action = cmd_export),
    list(name = "Diffusion", desc = "Récupère les paramètres de diffusion", action = cmd_diffusion),
    list(
      name = "Rapport Profils", desc = "Génère le rapport des profils chercheurs (PDF)",
      action = cmd_rapport_profils
    ),
    list(name = "Rapport PDF", desc = "Compile le rapport Quarto en PDF", action = cmd_rapport_pdf),
    list(name = "Rapport HTML", desc = "Compile le rapport Quarto en HTML", action = cmd_rapport_html),
    list(name = "Preview", desc = "Lance le preview Quarto", action = cmd_rapport_preview),
    list(name = "Nettoyage", desc = "Supprime les fichiers générés", action = cmd_clean),
    list(name = "Quitter", desc = "Ferme le programme", action = NULL)
  )

  repeat {
    cat(sprintf("\n%s\n", str_bold("=======================================")))

    cat(sprintf("  %s - Menu principal\n", str_bold("ECRIN")))
    cat(sprintf("%s\n\n", str_bold("=======================================")))

    for (i in seq_along(items)) {
      cat(sprintf("  %2d. %s - %s\n", i, str_cyan(items[[i]]$name), items[[i]]$desc))
    }

    cat("\nChoisissez une action (numéro) : ")
    choice <- tryCatch(
      suppressWarnings(as.integer(trimws(readLines(con = "stdin", n = 1)))),
      error = function(e) NA_integer_
    )

    if (is.na(choice) || choice < 1 || choice > length(items)) {
      cat("Choix invalide.\n")
      next
    }

    selected <- items[[choice]]

    if (is.null(selected$action)) {
      cat("\nAu revoir!\n")
      break
    }

    tryCatch(
      selected$action(),
      error = function(e) cat(str_red(sprintf("\nErreur: %s\n", conditionMessage(e))))
    )

    cat("\nAppuyez sur Entrée pour continuer...")
    readLines(con = "stdin", n = 1)

    system("clear")
  }
}

# ---------------------------------------------------------------------------
# Point d'entrée principal
# ---------------------------------------------------------------------------
main <- function() {
  load_env()

  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    run_interactive_menu()
    return(invisible(NULL))
  }

  cmd <- tolower(args[1])

  tryCatch(
    {
      switch(cmd,
        "metadata" = ,
        "m" = cmd_metadata(),
        "export" = ,
        "e" = cmd_export(),
        "diffusion" = ,
        "d" = cmd_diffusion(),
        "rapport" = ,
        "r" = {
          sub_cmd <- if (length(args) >= 2) tolower(args[2]) else "pdf"
          switch(sub_cmd,
            "profils" = cmd_rapport_profils(),
            "pdf" = cmd_rapport_pdf(),
            "html" = cmd_rapport_html(),
            "preview" = cmd_rapport_preview(),
            {
              cat(sprintf("Sous-commande rapport inconnue: %s\n", sub_cmd))
              cat("Sous-commandes disponibles: profils, pdf, html, preview\n")
            }
          )
        },
        "clean" = cmd_clean(),
        {
          cat(sprintf("Commande inconnue: %s\n", cmd))
          cat("Commandes: metadata, export, diffusion, rapport [profils|pdf|html|preview], clean\n")
          cat("Sans argument : lance le menu interactif\n")
          quit(status = 1)
        }
      )
    },
    error = function(e) {
      cat(str_red(sprintf("\nErreur: %s\n\n", conditionMessage(e))), file = stderr())
      quit(status = 1)
    }
  )
}

main()

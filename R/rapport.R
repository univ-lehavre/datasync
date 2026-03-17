# rapport.R — Génération de rapports (CSV, texte, Quarto QMD), nettoyage, compilation

# ---------------------------------------------------------------------------
# Génération du CSV des variables
# ---------------------------------------------------------------------------
generate_variables_csv <- function(metadata, filepath) {
  df <- data.frame(
    instrument = metadata$form_name,
    variable = metadata$field_name,
    label = metadata$field_label,
    type = metadata$field_type,
    choices = metadata$select_choices_or_calculations,
    required = metadata$required_field,
    branching_logic = metadata$branching_logic,
    stringsAsFactors = FALSE
  )
  write_csv(df, filepath)
}

# ---------------------------------------------------------------------------
# Génération du rapport texte
# ---------------------------------------------------------------------------
generate_report <- function(instruments, metadata, filepath) {
  lines <- character(0)
  add <- function(...) {
    lines <<- c(lines, paste0(...))
  }

  add(strrep("=", 60))
  add("DICTIONNAIRE DES DONNEES REDCAP")
  add("Généré le ", format(Sys.time(), "%Y-%m-%d %H:%M"))
  add(strrep("=", 60))
  add("")
  add("Nombre d'instruments : ", nrow(instruments))
  add("Nombre total de variables : ", nrow(metadata))
  add("")

  for (i in seq_len(nrow(instruments))) {
    inst <- instruments[i, ]
    vars <- metadata[metadata$form_name == inst$instrument_name, ]

    add(strrep("-", 60))
    add("INSTRUMENT : ", inst$instrument_label)
    add("Nom technique : ", inst$instrument_name)
    add("Variables : ", nrow(vars))
    add(strrep("-", 60))
    add("")

    for (j in seq_len(nrow(vars))) {
      v <- vars[j, ]
      required <- if (!is.na(v$required_field) && v$required_field == "y") "★ " else "  "
      add(required, "• ", v$field_name, " [", v$field_type, "]")
      add("    Label: ", v$field_label)

      has_choices <- !is.na(v$select_choices_or_calculations) &&
        nchar(v$select_choices_or_calculations) > 0 &&
        v$field_type %in% c("dropdown", "radio", "checkbox")
      if (has_choices) {
        add("    Choix: ", v$select_choices_or_calculations)
      }

      if (!is.na(v$branching_logic) && nchar(v$branching_logic) > 0) {
        add("    Condition: ", v$branching_logic)
      }
      add("")
    }
    add("")
  }

  writeLines(lines, filepath)
}

# ---------------------------------------------------------------------------
# Traduction des types de champs REDCap
# ---------------------------------------------------------------------------
translate_field_type <- function(field_type) {
  switch(field_type,
    text       = "Texte",
    radio      = "Choix unique",
    dropdown   = "Liste déroulante",
    checkbox   = "Cases à cocher",
    yesno      = "Oui/Non",
    file       = "Fichier",
    notes      = "Zone de texte",
    sql        = "Liste dynamique",
    field_type # défaut : valeur brute
  )
}

# ---------------------------------------------------------------------------
# Tableau markdown des variables d'un instrument
# ---------------------------------------------------------------------------
generate_instrument_variables_table <- function(metadata, form_name, has_identifiers) {
  sub <- metadata[metadata$form_name == form_name & metadata$field_type != "descriptive", ]
  lines <- character(0)

  if (has_identifiers) {
    lines <- c(lines, "| Variable | Type | Identifiante |", "|----------|------|-------------|")
  } else {
    lines <- c(lines, "| Variable | Type |", "|----------|------|")
  }

  for (i in seq_len(nrow(sub))) {
    v <- sub[i, ]
    type_fr <- translate_field_type(v$field_type)
    if (has_identifiers) {
      ident_mark <- if (!is.na(v$identifier) && v$identifier == "y") "**Oui**" else "Non"
      lines <- c(lines, sprintf("| `%s` | %s | %s |", v$field_name, type_fr, ident_mark))
    } else {
      lines <- c(lines, sprintf("| `%s` | %s |", v$field_name, type_fr))
    }
  }

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Section QMD d'un instrument
# ---------------------------------------------------------------------------
generate_instrument_section <- function(result, metadata, section_index) {
  name <- result$config$name
  label <- result$config$label
  prefix <- sprintf("%s_%d", name, section_index)

  n_ident <- if (!is.null(result$ident_records)) nrow(result$ident_records) else 0L
  n_pseudo <- if (!is.null(result$pseudo_records)) nrow(result$pseudo_records) else 0L
  n_anon <- if (!is.null(result$anon_records)) nrow(result$anon_records) else 0L
  n_stats <- result$stats$no_response + result$stats$audience_filtered + result$stats$aggregated
  n_total <- n_ident + n_pseudo + n_anon + n_stats

  if (n_total == 0) {
    return(sprintf("\n## %s\n\nAucune donnée pour cet instrument.\n\n", label))
  }

  b <- character(0)
  add <- function(...) {
    b <<- c(b, paste0(...))
  }

  add("\n## ", label, "\n")

  # Tableau des variables
  var_table <- generate_instrument_variables_table(metadata, name, result$config$has_identifiers)
  add("### Variables de l'instrument\n")
  add(var_table)
  add("\n")

  # Statistiques via blocs R
  add("### Statistiques\n")
  add("```{r}")
  add("#| label: stats-", prefix)
  add("#| results: asis")
  add("")
  tpl_csv <- "show_col_types = FALSE), error = function(e) tibble())"
  add("ident_", prefix, " <- tryCatch(read_csv(\"../data/vague2_", name, "_identifiables.csv\", ", tpl_csv)
  add("pseudo_", prefix, " <- tryCatch(read_csv(\"../data/vague3_", name, "_pseudonymises.csv\", ", tpl_csv)
  add("anon_", prefix, " <- tryCatch(read_csv(\"../data/vague4_", name, "_anonymises.csv\", ", tpl_csv)
  tpl_csv_stats <- "show_col_types = FALSE), error = function(e) tibble(categorie = character(), nombre = integer()))"
  add("stats_", prefix, " <- tryCatch(read_csv(\"../data/vague5_", name, "_statistiques.csv\", ", tpl_csv_stats)
  add("")
  add(sprintf("n_ident <- nrow(ident_%s)", prefix))
  add(sprintf("n_pseudo <- nrow(pseudo_%s)", prefix))
  add(sprintf("n_anon <- nrow(anon_%s)", prefix))
  add("")
  add("# Lire le détail des statistiques depuis le CSV")
  add("get_stat <- function(df, cat) {")
  add("  row <- df[df$categorie == cat, ]")
  add("  if (nrow(row) > 0) row$nombre[1] else 0L")
  add("}")
  add(sprintf("n_sans_reponse <- get_stat(stats_%s, \"sans_reponse\")", prefix))
  add(sprintf("n_filtre_audience <- get_stat(stats_%s, \"filtre_audience\")", prefix))
  add(sprintf("n_agreges <- get_stat(stats_%s, \"agreges\")", prefix))
  add("n_stats <- n_sans_reponse + n_filtre_audience + n_agreges")
  add("n_total <- n_ident + n_pseudo + n_anon + n_stats")
  add("")
  add('cat(paste0("- **Total** : ", n_total, " enregistrement(s)\\n"))')
  add('cat(paste0("- **Identifiables** : ", n_ident, "\\n"))')
  add('cat(paste0("- **Pseudonymisés** : ", n_pseudo, "\\n"))')
  add('cat(paste0("- **Anonymisés** : ", n_anon, "\\n"))')
  add("if (n_stats > 0) {")
  add('  cat(paste0("- **Statistiques seules** : ", n_stats, "\\n"))')
  add('  if (n_sans_reponse > 0) cat(paste0("  - Sans réponse de diffusion : ", n_sans_reponse, "\\n"))')
  add('  if (n_filtre_audience > 0) cat(paste0("  - Filtrés par audience : ", n_filtre_audience, "\\n"))')
  add('  if (n_agreges > 0) cat(paste0("  - Agrégés uniquement : ", n_agreges, "\\n"))')
  add("}")
  add("```\n")

  # Données identifiables
  if (n_ident > 0) {
    add("### Données identifiables\n")
    add("```{r}")
    add("#| label: ident-", prefix)
    add("#| results: asis")
    add("")
    add(sprintf("if (nrow(ident_%s) > 0) {", prefix))
    add(sprintf("  display_cols <- names(ident_%s)", prefix))
    add("  # Limiter à 6 colonnes pour la lisibilité")
    add("  if (length(display_cols) > 6) display_cols <- display_cols[1:6]")
    add(sprintf(
      '  kable(ident_%s[, display_cols], caption = paste0("Identifiables (", nrow(ident_%s), ")")) %%>%%',
      prefix, prefix
    ))
    add('    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%')
    add("    print()")
    add("}")
    add("```\n")
  }

  # Données pseudonymisées
  if (n_pseudo > 0) {
    add("### Données pseudonymisées\n")
    add("```{r}")
    add("#| label: pseudo-", prefix)
    add("#| results: asis")
    add("")
    add(sprintf("if (nrow(pseudo_%s) > 0) {", prefix))
    add(sprintf("  display_cols <- names(pseudo_%s)", prefix))
    add("  if (length(display_cols) > 6) display_cols <- display_cols[1:6]")
    add(sprintf(
      '  kable(pseudo_%s[, display_cols], caption = paste0("Pseudonymisés (", nrow(pseudo_%s), ")")) %%>%%',
      prefix, prefix
    ))
    add('    kable_styling(latex_options = c("striped", "hold_position", "scale_down")) %>%')
    add("    print()")
    add("}")
    add("```\n")
  }

  # Données anonymisées
  if (n_anon > 0) {
    add("### Données anonymisées\n")
    add("```{r}")
    add("#| label: anon-", prefix)
    add("#| results: asis")
    add("")
    add(sprintf('cat(paste0("**", nrow(anon_%s), " enregistrement(s) anonymisé(s)**\\n\\n"))', prefix))
    add("```\n")
  }

  paste(b, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Génération du rapport Quarto QMD
# ---------------------------------------------------------------------------
generate_report_qmd <- function(audience, metadata, results) {
  watermark <- ""
  if (audience == "chercheurs") {
    watermark <- paste0(
      "\nheader-includes:\n",
      "  - \\usepackage{draftwatermark}\n",
      "  - \\SetWatermarkText{Restricted - Authorized researchers only}\n",
      "  - \\SetWatermarkScale{0.25}\n",
      "  - \\SetWatermarkColor[gray]{0.9}"
    )
  }

  audience_label <- if (audience == "chercheurs") "Chercheurs autorisés" else "Grand public"

  b <- character(0)
  add <- function(...) {
    b <<- c(b, paste0(...))
  }

  # En-tête YAML
  add("---")
  add('title: "Rapport ECRIN - Première vague"')
  add(sprintf('subtitle: "Audience : %s"', audience_label))
  add('author: "Plateforme ECRIN"')
  add(sprintf('date: "%s"', format(Sys.Date(), "%Y-%m-%d")))
  add("lang: fr")
  add("format:")
  add("  pdf:")
  add("    toc: true")
  add("    toc-depth: 2")
  add("    documentclass: article")
  add("    papersize: a4")
  add("    geometry:")
  add("      - margin=2.5cm")
  add("    colorlinks: true", watermark)
  add("execute:")
  add("  echo: false")
  add("  warning: false")
  add("  message: false")
  add("---")
  add("")

  # Setup R
  add("```{r}")
  add("#| label: setup")
  add("#| include: false")
  add("")
  add("library(tidyverse)")
  add("library(knitr)")
  add("library(kableExtra)")
  add("```")
  add("")

  # Introduction
  add(sprintf("Ce rapport présente les données de la première vague ECRIN pour l'audience **%s**.\n", audience_label))

  # Résumé global
  add("## Résumé\n")
  add("| Instrument | Identifiables | Pseudonymisés | Anonymisés | Statistiques seules | Total |")
  add("|------------|:---:|:---:|:---:|:---:|:---:|")

  for (r in results) {
    n_i <- if (!is.null(r$ident_records)) nrow(r$ident_records) else 0L
    n_p <- if (!is.null(r$pseudo_records)) nrow(r$pseudo_records) else 0L
    n_a <- if (!is.null(r$anon_records)) nrow(r$anon_records) else 0L
    n_st <- r$stats$no_response + r$stats$audience_filtered + r$stats$aggregated
    n_t <- n_i + n_p + n_a + n_st
    add(sprintf("| %s | %d | %d | %d | %d | **%d** |", r$config$label, n_i, n_p, n_a, n_st, n_t))
  }
  add("")

  # Sections par instrument
  for (i in seq_along(results)) {
    section <- generate_instrument_section(results[[i]], metadata, i)
    add(section)
  }

  # Pied de page
  add(sprintf("\n---\n\n*Rapport généré le %s*", format(Sys.time(), "%Y-%m-%d %H:%M")))

  qmd_path <- file.path(REPORTS_DIR, "rapport-ecrin.qmd")
  writeLines(b, qmd_path)

  qmd_path
}

# ---------------------------------------------------------------------------
# Nettoyage des sorties précédentes (avant rapport)
# ---------------------------------------------------------------------------
clean_output_dir <- function() {
  # vague*_*.csv dans data/
  csv_files <- Sys.glob(file.path(DATA_DIR, "vague*_*.csv"))
  for (f in csv_files) file.remove(f)

  # dossier fichiers/
  fichiers_dir <- file.path(DATA_DIR, "fichiers")
  if (dir.exists(fichiers_dir)) unlink(fichiers_dir, recursive = TRUE)

  # rapport-ecrin.* dans reports/
  rapport_files <- Sys.glob(file.path(REPORTS_DIR, "rapport-ecrin.*"))
  for (f in rapport_files) file.remove(f)

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Compilation Quarto
# ---------------------------------------------------------------------------
run_quarto <- function(...) {
  args <- c(...)
  ret <- system2("quarto", args)
  if (ret != 0) stop(sprintf("Erreur Quarto (code %d)", ret))
  invisible(ret)
}

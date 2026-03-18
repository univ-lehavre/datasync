# stats.R — Statistiques par variable d'un instrument REDCap

# Retourne un data.frame (variable, statistique, valeur)
# records : data.frame des enregistrements (périmètre anonymisés)
# dict    : data.frame du dictionnaire natif REDCap (18 colonnes)
# form_name : nom de l'instrument
# id_field  : nom du champ identifiant (exclu des stats)
compute_instrument_stats <- function(records, dict, form_name, id_field) {
  form_dict <- dict[dict$form_name == form_name & dict$field_type != "descriptive", ]
  if (nrow(form_dict) == 0 || is.null(records) || nrow(records) == 0) {
    return(data.frame(
      variable = character(), statistique = character(), valeur = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()

  for (i in seq_len(nrow(form_dict))) {
    field <- form_dict$field_name[i]
    ftype <- form_dict$field_type[i]
    choices <- if ("select_choices_or_calculations" %in% names(form_dict)) {
      form_dict$select_choices_or_calculations[i]
    } else {
      ""
    }
    valid_type <- if ("text_validation_type_or_show_slider_number" %in% names(form_dict)) {
      form_dict$text_validation_type_or_show_slider_number[i]
    } else {
      ""
    }
    if (is.na(valid_type)) valid_type <- ""

    branching <- if ("branching_logic" %in% names(form_dict)) {
      form_dict$branching_logic[i]
    } else {
      ""
    }
    if (is.na(branching)) branching <- ""
    # Filtrer les enregistrements applicables selon la branching_logic
    applicable_records <- resolve_branching_filter(records, branching, dict)
    conditional <- !is.null(applicable_records)
    active_records <- if (conditional) applicable_records else records
    missing_label <- "n_manquants"

    if (ftype == "checkbox") {
      # Colonnes checkbox : field___1, field___2, ...
      checkbox_cols <- names(records)[startsWith(names(records), paste0(field, "___"))]
      if (length(checkbox_cols) == 0) next

      # Parse options pour labels
      option_labels <- parse_choices(choices)

      # Codes bruts "1"/"0" ou labels "Checked"/"Unchecked" selon export REDCap
      checked_vals <- c("1", "Checked", "Oui", "Yes")
      n_obf_total <- 0L
      for (col in checkbox_cols) {
        suffix <- sub(paste0("^", field, "___"), "", col)
        label <- if (!is.null(option_labels) && suffix %in% names(option_labels)) {
          option_labels[[suffix]]
        } else {
          suffix
        }
        cvals <- as.character(active_records[[col]])
        obf_c <- cvals[cvals == "***"]
        n_obf_total <- max(n_obf_total, length(obf_c))
        non_obf_c <- cvals[cvals != "***"]
        filled_c <- non_obf_c[!is.na(non_obf_c) & non_obf_c != "" & non_obf_c != "NA"]
        n_coches <- sum(filled_c %in% checked_vals)
        rows <- c(rows, list(
          data.frame(
            variable = field, statistique = label,
            valeur = as.character(n_coches), stringsAsFactors = FALSE
          )
        ))
      }
      rows <- c(rows, list(
        data.frame(
          variable = field, statistique = missing_label,
          valeur = as.character(n_obf_total), stringsAsFactors = FALSE
        )
      ))
      next
    }

    if (!(field %in% names(active_records))) next
    vals <- as.character(active_records[[field]])

    # Pour les champs conditionnels, émettre n_applicable en tête
    if (conditional) {
      rows <- c(rows, list(data.frame(
        variable = field, statistique = "n_applicable",
        valeur = as.character(nrow(active_records)), stringsAsFactors = FALSE
      )))
    }

    if (ftype == "file") {
      non_empty <- vals[!is.na(vals) & vals != "" & vals != "NA" & vals != "***"]
      exts <- unique(tolower(tools::file_ext(non_empty)))
      exts <- exts[nchar(exts) > 0]
      rows <- c(rows, list(
        data.frame(
          variable = field, statistique = "n_fichiers",
          valeur = as.character(length(non_empty)), stringsAsFactors = FALSE
        ),
        data.frame(
          variable = field, statistique = "extensions",
          valeur = paste(exts, collapse = ", "), stringsAsFactors = FALSE
        )
      ))
      next
    }

    if (ftype %in% c("yesno", "radio", "dropdown", "sql")) {
      option_labels <- if (ftype %in% c("radio", "dropdown")) parse_choices(choices) else NULL
      n_obf <- sum(vals == "***")
      non_obf <- vals[vals != "***"]
      n_missing <- sum(is.na(non_obf) | non_obf == "" | non_obf == "NA")
      filled <- non_obf[!is.na(non_obf) & non_obf != "" & non_obf != "NA"]
      freq <- sort(table(filled), decreasing = TRUE)
      for (k in names(freq)) {
        lbl <- if (!is.null(option_labels) && k %in% names(option_labels)) {
          option_labels[[k]]
        } else {
          k
        }
        rows <- c(rows, list(
          data.frame(
            variable = field, statistique = lbl,
            valeur = as.character(freq[[k]]), stringsAsFactors = FALSE
          )
        ))
      }
      if (n_obf > 0) {
        rows <- c(rows, list(data.frame(
          variable = field, statistique = "n_obfusques",
          valeur = as.character(n_obf), stringsAsFactors = FALSE
        )))
      }
      rows <- c(rows, list(
        data.frame(
          variable = field, statistique = missing_label,
          valeur = as.character(n_missing), stringsAsFactors = FALSE
        )
      ))
      next
    }

    # text, notes, calc, slider, et autres
    n_obf <- sum(vals == "***")
    n_rens <- sum(!is.na(vals) & vals != "" & vals != "NA" & vals != "***")
    n_missing <- sum(is.na(vals) | vals == "" | vals == "NA")
    rows <- c(rows, list(
      data.frame(
        variable = field, statistique = "n_renseignes",
        valeur = as.character(n_rens), stringsAsFactors = FALSE
      )
    ))
    if (n_obf > 0) {
      rows <- c(rows, list(data.frame(
        variable = field, statistique = "n_obfusques",
        valeur = as.character(n_obf), stringsAsFactors = FALSE
      )))
    }
    rows <- c(rows, list(
      data.frame(
        variable = field, statistique = missing_label,
        valeur = as.character(n_missing), stringsAsFactors = FALSE
      )
    ))
  }

  if (length(rows) == 0) {
    return(data.frame(
      variable = character(), statistique = character(), valeur = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

# Retourne le sous-ensemble de records applicables si branching_logic est de la forme
# "[field] = 'code' [or [field] = 'code2' ...]", NULL sinon (logique non parseable).
# Les codes sont résolus en labels via le dictionnaire (export REDCap = labels).
resolve_branching_filter <- function(records, branching, dict) {
  if (nchar(trimws(branching)) == 0) {
    return(NULL)
  }
  # Ne traiter que les conditions "= 'code'" sans <>, AND, etc.
  if (grepl("<>|AND|and", branching)) {
    return(NULL)
  }
  # Extraire toutes les paires [field] = 'code'
  m <- gregexpr("\\[([^\\]]+)\\]\\s*=\\s*'([^']*)'", branching, perl = TRUE)
  matches <- regmatches(branching, m)[[1]]
  if (length(matches) == 0) {
    return(NULL)
  }
  # Construire un filtre OR : au moins une condition vraie
  keep <- rep(FALSE, nrow(records))
  for (match in matches) {
    cap <- regmatches(branching, regexec(
      "\\[([^\\]]+)\\]\\s*=\\s*'([^']*)'",
      match,
      perl = TRUE
    ))[[1]]
    parent_field <- cap[2]
    code <- cap[3]
    if (!(parent_field %in% names(records))) next
    parent_vals <- as.character(records[[parent_field]])
    # Résoudre code → label via le dictionnaire du champ parent
    parent_row <- dict[dict$field_name == parent_field, ]
    target_label <- if (nrow(parent_row) > 0) {
      ptype <- parent_row$field_type[1]
      if (ptype == "yesno") {
        if (code == "1") "Oui" else if (code == "0") "Non" else code
      } else {
        pchoices <- if ("select_choices_or_calculations" %in% names(parent_row)) {
          parent_row$select_choices_or_calculations[1]
        } else {
          ""
        }
        lmap <- parse_choices(pchoices)
        if (!is.null(lmap) && code %in% names(lmap)) lmap[[code]] else code
      }
    } else {
      code
    }
    keep <- keep | (parent_vals == target_label & !is.na(parent_vals))
  }
  records[keep, , drop = FALSE]
}

# Parse "1, Label un | 2, Label deux | ..." → list("1"="Label un", "2"="Label deux")
parse_choices <- function(choices_str) {
  if (is.null(choices_str) || is.na(choices_str) || nchar(trimws(choices_str)) == 0) {
    return(NULL)
  }
  parts <- strsplit(choices_str, "\\|")[[1]]
  result <- list()
  for (p in parts) {
    p <- trimws(p)
    m <- regexpr("^([^,]+),\\s*(.*)", p, perl = TRUE)
    if (m == -1) next
    starts <- attr(m, "capture.start")
    lens <- attr(m, "capture.length")
    key <- trimws(substr(p, starts[1], starts[1] + lens[1] - 1))
    label <- trimws(substr(p, starts[2], starts[2] + lens[2] - 1))
    result[[key]] <- label
  }
  if (length(result) == 0) NULL else result
}

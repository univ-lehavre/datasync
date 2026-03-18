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
    conditional <- !is.na(branching) && nchar(trimws(branching)) > 0
    missing_label <- if (conditional) "n_applicable" else "n_manquants"

    if (ftype == "checkbox") {
      # Colonnes checkbox : field___1, field___2, ...
      checkbox_cols <- names(records)[startsWith(names(records), paste0(field, "___"))]
      if (length(checkbox_cols) == 0) next

      # Parse options pour labels
      option_labels <- parse_choices(choices)

      for (col in checkbox_cols) {
        suffix <- sub(paste0("^", field, "___"), "", col)
        label <- if (!is.null(option_labels) && suffix %in% names(option_labels)) {
          option_labels[[suffix]]
        } else {
          suffix
        }
        cvals <- as.character(records[[col]])
        non_obf_c <- cvals[cvals != "***"]
        filled_c <- non_obf_c[!is.na(non_obf_c) & non_obf_c != "" & non_obf_c != "NA"]
        n_empty_c <- length(non_obf_c) - length(filled_c)
        # Codes bruts "1"/"0" ou labels "Checked"/"Unchecked" selon export REDCap
        checked_vals <- c("1", "Checked", "Oui", "Yes")
        n_coches <- sum(filled_c %in% checked_vals)
        rows <- c(rows, list(
          data.frame(
            variable = field, statistique = label,
            valeur = as.character(n_coches), stringsAsFactors = FALSE
          ),
          data.frame(
            variable = field, statistique = missing_label,
            valeur = as.character(n_empty_c), stringsAsFactors = FALSE
          )
        ))
      }
      next
    }

    if (!(field %in% names(records))) next
    vals <- as.character(records[[field]])

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
      option_labels <- if (ftype != "yesno") parse_choices(choices) else NULL
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
      rows <- c(rows, list(
        data.frame(
          variable = field, statistique = missing_label,
          valeur = as.character(n_missing), stringsAsFactors = FALSE
        )
      ))
      next
    }

    # text, notes, calc, slider, et autres
    n_rens <- sum(!is.na(vals) & vals != "" & vals != "NA" & vals != "***")
    n_missing <- sum(is.na(vals) | vals == "" | vals == "NA" | vals == "***")
    rows <- c(rows, list(
      data.frame(
        variable = field, statistique = "n_renseignes",
        valeur = as.character(n_rens), stringsAsFactors = FALSE
      ),
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

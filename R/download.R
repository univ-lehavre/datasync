# download.R — Téléchargement REDCap, écriture CSV, pipeline par niveaux d'identification

# ---------------------------------------------------------------------------
# Téléchargement de fichier REDCap
# ---------------------------------------------------------------------------
download_file_redcap <- function(api_url, token, record_id, field_name, dest_path) {
  body_params <- list(
    token   = token,
    content = "file",
    action  = "export",
    record  = record_id,
    field   = field_name
  )
  resp <- tryCatch(
    request(api_url) |>
      req_method("POST") |>
      req_body_form(!!!body_params) |>
      req_perform(),
    error = function(e) {
      message(sprintf("Erreur téléchargement fichier %s/%s: %s", record_id, field_name, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(resp)) {
    return(invisible(FALSE))
  }
  if (resp_status(resp) != 200L) {
    message(sprintf("Erreur API fichier (status %d)", resp_status(resp)))
    return(invisible(FALSE))
  }
  writeBin(resp_body_raw(resp), dest_path)
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Normalisation DPI via magick CLI (si disponible)
# ---------------------------------------------------------------------------
normalize_image_dpi <- function(image_path) {
  if (nchar(Sys.which("magick")) > 0) {
    system2("magick", c("mogrify", "-density", "72", image_path),
      stdout = FALSE, stderr = FALSE
    )
  }
}

# ---------------------------------------------------------------------------
# Écriture CSV d'un instrument
# ---------------------------------------------------------------------------
write_instrument_csv <- function(path, records, fields, id_field, include_hashed_id = FALSE) {
  if (is.null(records) || nrow(records) == 0) {
    return(invisible(NULL))
  }

  rows <- lapply(seq_len(nrow(records)), function(i) {
    row <- character(length(fields))
    for (j in seq_along(fields)) {
      f <- fields[j]
      if (f == "hashed_id" && include_hashed_id) {
        row[j] <- if (id_field %in% names(records)) hash_id(as.character(records[[id_field]][i])) else ""
      } else {
        row[j] <- if (f %in% names(records)) as.character(records[[f]][i]) else ""
      }
    }
    row
  })

  df_out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df_out) <- fields
  write_csv(df_out, path)
}

# ---------------------------------------------------------------------------
# Téléchargement des fichiers d'un instrument
# ---------------------------------------------------------------------------
download_instrument_files <- function(api_url, token, records, id_field, config) {
  if (length(config$file_fields) == 0 || is.null(records) || nrow(records) == 0) {
    return(character(0))
  }

  files_dir <- file.path(DATA_DIR, "fichiers", config$name)
  dir_create(files_dir, recurse = TRUE)

  downloaded <- character(0)

  for (i in seq_len(nrow(records))) {
    record_id <- as.character(records[[id_field]][i])

    for (field_name in config$file_fields) {
      val <- if (field_name %in% names(records)) as.character(records[[field_name]][i]) else ""
      if (nchar(val) == 0 || val == "NA" || val == "<nil>") next

      # Cas spécial : portrait avec portrait_choice
      if (field_name == "portrait") {
        choice <- if ("portrait_choice" %in% names(records)) as.character(records[["portrait_choice"]][i]) else ""
        if (!grepl("picture|2", choice)) next
      }

      ext <- tools::file_ext(val)
      if (nchar(ext) == 0) ext <- "bin"
      hashed_id <- hash_id(record_id)
      dest_path <- file.path(files_dir, sprintf("%s_%s.%s", hashed_id, field_name, ext))

      ok <- download_file_redcap(api_url, token, record_id, field_name, dest_path)
      if (!isTRUE(ok)) next

      # Normaliser le DPI pour les images
      if (tolower(ext) %in% c("jpg", "jpeg", "png")) {
        normalize_image_dpi(dest_path)
      }

      downloaded <- c(downloaded, dest_path)
    }
  }

  downloaded
}

# ---------------------------------------------------------------------------
# Téléchargement complet d'un instrument (par niveaux)
# ---------------------------------------------------------------------------
download_instrument_data <- function(api_url, token, metadata, id_field, audience, config) {
  result <- list(
    config = config,
    ident_records = NULL,
    pseudo_records = NULL,
    anon_records = NULL,
    stats = list(no_response = 0L, audience_filtered = 0L, aggregated = 0L),
    files = character(0)
  )

  split_res <- split_fields_by_identifier(metadata, config$name)
  identifiers <- split_res$identifiers
  non_identifiers <- split_res$non_identifiers
  all_form_fields <- get_form_fields(metadata, config$name)

  data_fields <- if (config$has_identifiers) non_identifiers else all_form_fields
  name_fields <- c("last_name", "first_name", "middle_name")

  # 1. Champs de contrôle pour classifier les participants
  control_fields <- c(id_field, config$id_level_field, config$audience_field)
  control_records <- get_records_with_fields_raw(api_url, token, control_fields)

  if (is.null(control_records) || nrow(control_records) == 0) {
    return(result)
  }

  ident_ids <- character(0)
  pseudo_ids <- character(0)
  anon_ids <- character(0)
  aggregated_ids <- character(0)

  for (i in seq_len(nrow(control_records))) {
    id <- as.character(control_records[[id_field]][i])
    id_level <- if (config$id_level_field %in% names(control_records)) {
      as.character(control_records[[config$id_level_field]][i])
    } else {
      ""
    }
    data_audience <- if (config$audience_field %in% names(control_records)) {
      as.character(control_records[[config$audience_field]][i])
    } else {
      ""
    }

    if (is.na(id_level) || nchar(id_level) == 0) {
      result$stats$no_response <- result$stats$no_response + 1L
      next
    }

    if (audience == "public" && (is.na(data_audience) || !grepl("General public", data_audience, fixed = TRUE))) {
      result$stats$audience_filtered <- result$stats$audience_filtered + 1L
      next
    }

    if (grepl("Identifiable", id_level, fixed = TRUE)) {
      ident_ids <- c(ident_ids, id)
    } else if (grepl("Pseudonymised", id_level, fixed = TRUE)) {
      pseudo_ids <- c(pseudo_ids, id)
    } else if (grepl("Anonymised", id_level, fixed = TRUE)) {
      anon_ids <- c(anon_ids, id)
    } else if (grepl("Aggregated", id_level, fixed = TRUE)) {
      aggregated_ids <- c(aggregated_ids, id)
    }
  }

  # 2. Identifiables
  if (length(ident_ids) > 0) {
    fields_to_download <- if (config$has_identifiers) {
      c(id_field, identifiers, non_identifiers)
    } else {
      c(id_field, name_fields, all_form_fields)
    }

    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, ident_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération identifiables: %s", config$name, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(records) && nrow(records) > 0) {
      csv_fields <- if (config$has_identifiers) c(identifiers, non_identifiers) else c(name_fields, all_form_fields)
      csv_path <- file.path(DATA_DIR, sprintf("vague2_%s_identifiables.csv", config$name))
      write_instrument_csv(csv_path, records, csv_fields, id_field, include_hashed_id = FALSE)
      result$ident_records <- records
    }
  }

  # 3. Pseudonymisés
  if (length(pseudo_ids) > 0) {
    fields_to_download <- c(id_field, data_fields)

    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, pseudo_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération pseudonymisés: %s", config$name, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(records) && nrow(records) > 0) {
      csv_fields <- c("hashed_id", data_fields)
      csv_path <- file.path(DATA_DIR, sprintf("vague3_%s_pseudonymises.csv", config$name))
      write_instrument_csv(csv_path, records, csv_fields, id_field, include_hashed_id = TRUE)
      result$pseudo_records <- records
    }
  }

  # 4. Anonymisés
  if (length(anon_ids) > 0) {
    fields_to_download <- c(id_field, data_fields)

    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, anon_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération anonymisés: %s", config$name, conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(records) && nrow(records) > 0) {
      csv_path <- file.path(DATA_DIR, sprintf("vague4_%s_anonymises.csv", config$name))
      write_instrument_csv(csv_path, records, data_fields, id_field, include_hashed_id = FALSE)
      result$anon_records <- records
    }
  }

  # 5. Agrégés
  if (length(aggregated_ids) > 0) {
    result$stats$aggregated <- result$stats$aggregated + length(aggregated_ids)
  }

  # 6. Export CSV statistiques
  n_stats <- result$stats$no_response + result$stats$audience_filtered + result$stats$aggregated
  if (n_stats > 0) {
    csv_path <- file.path(DATA_DIR, sprintf("vague5_%s_statistiques.csv", config$name))
    stats_df <- data.frame(
      categorie = c("sans_reponse", "filtre_audience", "agreges", "total"),
      nombre = c(
        result$stats$no_response,
        result$stats$audience_filtered,
        result$stats$aggregated,
        n_stats
      ),
      stringsAsFactors = FALSE
    )
    write_csv(stats_df, csv_path)
  }

  result
}

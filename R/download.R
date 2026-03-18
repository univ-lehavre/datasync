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
  raw_body <- tryCatch(resp_body_raw(resp), error = function(e) raw(0))
  if (length(raw_body) == 0L) {
    return(invisible(FALSE))
  }
  writeBin(raw_body, dest_path)
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
write_instrument_csv <- function(path, records, fields, id_field,
                                 include_hashed_id = FALSE,
                                 obfuscate_fields = character(0)) {
  if (is.null(records) || nrow(records) == 0) {
    return(invisible(NULL))
  }

  rows <- lapply(seq_len(nrow(records)), function(i) {
    row <- character(length(fields))
    for (j in seq_along(fields)) {
      f <- fields[j]
      if (f %in% obfuscate_fields) {
        row[j] <- "***"
      } else if (f == "hashed_id" && include_hashed_id) {
        row[j] <- if (id_field %in% names(records)) hash_id(as.character(records[[id_field]][i])) else ""
      } else {
        val <- if (f %in% names(records)) as.character(records[[f]][i]) else ""
        row[j] <- gsub("[\r\n]+", " ", val)
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
download_instrument_files <- function(
    api_url, token, records, id_field, config,
    files_dir = file.path(DATA_DIR, "files")) {
  if (length(config$file_fields) == 0 || is.null(records) || nrow(records) == 0) {
    return(character(0))
  }

  dir_create(files_dir, recurse = TRUE)

  downloaded <- character(0)

  for (i in seq_len(nrow(records))) {
    record_id <- as.character(records[[id_field]][i])

    for (field_name in config$file_fields) {
      val <- if (field_name %in% names(records)) as.character(records[[field_name]][i]) else ""
      if (is.na(val) || nchar(val) == 0 || val == "NA" || val == "<nil>") next

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
download_instrument_data <- function(
    api_url, token, metadata, id_field, audience, config,
    data_dir = DATA_DIR, dict = NULL) {
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

  # Colonnes communes à tous les CSV (ordre du formulaire REDCap)
  all_csv_fields <- c(id_field, "hashed_id", all_form_fields)
  # Champs nominatifs obfusqués dans pseudo/anon
  nominative_fields <- c(id_field, if (config$has_identifiers) identifiers else character(0))
  # Champs obfusqués dans anonymisés (nominatifs + hashed_id)
  anon_obfuscate <- c(nominative_fields, "hashed_id")

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
  stats_only_ids <- character(0) # audience-filtered + aggregated : stats uniquement

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

    not_public_audience <- is.na(data_audience) || !grepl("General public", data_audience, fixed = TRUE)
    audience_filtered <- audience == "public" && not_public_audience

    if (audience_filtered) {
      result$stats$audience_filtered <- result$stats$audience_filtered + 1L
    }

    # Classement pour les CSV individuels (respecte le filtre audience)
    if (!audience_filtered) {
      if (audience == "admin") {
        ident_ids <- c(ident_ids, id)
      } else if (grepl("Identifiable", id_level, fixed = TRUE)) {
        ident_ids <- c(ident_ids, id)
      } else if (grepl("Pseudonymised", id_level, fixed = TRUE)) {
        pseudo_ids <- c(pseudo_ids, id)
      } else if (grepl("Anonymised", id_level, fixed = TRUE)) {
        anon_ids <- c(anon_ids, id)
      } else if (grepl("Aggregated", id_level, fixed = TRUE)) {
        aggregated_ids <- c(aggregated_ids, id)
      }
    }

    # Classement pour les statistiques : tous les participants
    # sauf ceux déjà inclus via ident/pseudo/anon (qui ont leurs données complètes)
    if (audience_filtered || grepl("Aggregated", id_level, fixed = TRUE)) {
      stats_only_ids <- c(stats_only_ids, id)
    }
  }

  # Champs complets à télécharger (nominatifs + données)
  fields_to_download <- unique(c(id_field, setdiff(all_csv_fields, "hashed_id")))

  # 2. Identifiables
  if (length(ident_ids) > 0) {
    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, ident_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération identifiables: %s", config$name, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(records) && nrow(records) > 0) {
      result$ident_records <- records
    }
  }

  # 3. Pseudonymisés
  if (length(pseudo_ids) > 0) {
    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, pseudo_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération pseudonymisés: %s", config$name, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(records) && nrow(records) > 0) {
      result$pseudo_records <- records
    }
  }

  # 4. Anonymisés
  if (length(anon_ids) > 0) {
    records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, anon_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération anonymisés: %s", config$name, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(records) && nrow(records) > 0) {
      result$anon_records <- records
    }
  }

  # Écriture identifiables : toutes les colonnes, aucune obfuscation
  if (!is.null(result$ident_records) && nrow(result$ident_records) > 0) {
    csv_path <- file.path(data_dir, "identifiables.csv")
    write_instrument_csv(
      csv_path, result$ident_records, all_csv_fields, id_field,
      include_hashed_id = TRUE
    )
  }

  # Écriture pseudonymisés : pseudo + identifiables, nominatifs obfusqués
  pseudo_rows <- list()
  for (r in list(result$pseudo_records, result$ident_records)) {
    if (!is.null(r) && nrow(r) > 0) pseudo_rows <- c(pseudo_rows, list(r))
  }
  if (length(pseudo_rows) > 0) {
    csv_path <- file.path(data_dir, "pseudonymises.csv")
    write_instrument_csv(
      csv_path, do.call(rbind, pseudo_rows), all_csv_fields, id_field,
      include_hashed_id = TRUE, obfuscate_fields = nominative_fields
    )
  }

  # Écriture anonymisés : anon + pseudo + identifiables, nominatifs + hashed_id obfusqués
  anon_rows <- list()
  for (r in list(result$anon_records, result$pseudo_records, result$ident_records)) {
    if (!is.null(r) && nrow(r) > 0) anon_rows <- c(anon_rows, list(r))
  }
  if (length(anon_rows) > 0) {
    csv_path <- file.path(data_dir, "anonymises.csv")
    write_instrument_csv(
      csv_path, do.call(rbind, anon_rows), all_csv_fields, id_field,
      include_hashed_id = TRUE, obfuscate_fields = anon_obfuscate
    )
  }

  # 5. Agrégés
  if (length(aggregated_ids) > 0) {
    result$stats$aggregated <- result$stats$aggregated + length(aggregated_ids)
  }

  # 6. Enregistrements stats-only (audience-filtered + agrégés) : tous les champs
  # Les champs identifiants sont obfusqués à "***" pour protéger l'identité,
  # mais leur présence/absence reste comptabilisée dans les statistiques.
  stats_only_records <- NULL
  if (length(stats_only_ids) > 0) {
    stats_only_records <- tryCatch(
      get_records_with_fields_and_ids(api_url, token, fields_to_download, stats_only_ids),
      error = function(e) {
        warning(sprintf("[%s] erreur récupération stats-only: %s", config$name, conditionMessage(e)))
        NULL
      }
    )
    # Obfusquer tous les champs identifiants (id + identifiers)
    if (!is.null(stats_only_records)) {
      for (col in nominative_fields) {
        if (col %in% names(stats_only_records)) {
          stats_only_records[[col]] <- "***"
        }
      }
    }
  }

  # 7. Export CSV statistiques par variable
  all_levels <- list(result$anon_records, result$pseudo_records, result$ident_records, stats_only_records)
  anon_rows_all <- Filter(Negate(is.null), all_levels)
  anon_all <- if (length(anon_rows_all) > 0) do.call(rbind, anon_rows_all) else NULL
  if (!is.null(dict) && !is.null(anon_all) && nrow(anon_all) > 0) {
    csv_path <- file.path(data_dir, "statistiques.csv")
    stats_df <- compute_instrument_stats(anon_all, dict, config$name, id_field)
    if (!is.null(stats_df) && nrow(stats_df) > 0) {
      write_csv(stats_df, csv_path)
    }
  }

  result
}

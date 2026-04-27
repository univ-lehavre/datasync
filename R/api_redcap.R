# api_redcap.R — Requêtes API REDCap, hashing, récupération des enregistrements

# ---------------------------------------------------------------------------
# Requête API REDCap générique
# ---------------------------------------------------------------------------
redcap_request <- function(api_url, token, content, extra = list()) {
  body <- c(
    list(token = token, content = content, format = "json"),
    extra
  )

  resp <- tryCatch(
    request(api_url) |>
      req_method("POST") |>
      req_body_form(!!!body) |>
      req_perform(),
    error = function(e) stop(paste("Erreur requête:", conditionMessage(e)))
  )

  if (resp_status(resp) != 200L) {
    stop(sprintf("Erreur API (status %d): %s", resp_status(resp), resp_body_string(resp)))
  }

  resp_body_string(resp)
}

# ---------------------------------------------------------------------------
# Instruments
# ---------------------------------------------------------------------------
get_instruments <- function(api_url, token) {
  body <- redcap_request(api_url, token, "instrument")
  fromJSON(body, simplifyDataFrame = TRUE)
}

# ---------------------------------------------------------------------------
# Métadonnées
# ---------------------------------------------------------------------------
get_metadata <- function(api_url, token) {
  body <- redcap_request(api_url, token, "metadata")
  df <- fromJSON(body, simplifyDataFrame = TRUE)
  # Normaliser les colonnes attendues
  needed <- c(
    "form_name", "field_name", "field_label", "field_type",
    "select_choices_or_calculations", "required_field",
    "branching_logic", "identifier"
  )
  for (col in needed) {
    if (!col %in% names(df)) df[[col]] <- ""
  }
  df[needed]
}

# ---------------------------------------------------------------------------
# Hash SHA256 tronqué à 16 caractères hex
# ---------------------------------------------------------------------------
hash_id <- function(id) {
  substr(digest::digest(id, algo = "sha256", serialize = FALSE), 1, 16)
}

hash_record_ids <- function(records, id_field) {
  if (is.null(records) || nrow(records) == 0) {
    return(records)
  }
  records[["_original_id"]] <- as.character(records[[id_field]])
  records[[id_field]] <- vapply(records[[id_field]], function(v) hash_id(as.character(v)), character(1))
  records
}

# ---------------------------------------------------------------------------
# Récupération des enregistrements
# ---------------------------------------------------------------------------
get_records <- function(api_url, token, id_field) {
  body <- redcap_request(api_url, token, "record", list(rawOrLabel = "label"))
  records <- fromJSON(body, simplifyDataFrame = TRUE)
  if (is.null(records) || (is.data.frame(records) && nrow(records) == 0)) {
    return(records)
  }
  hash_record_ids(records, id_field)
}

get_records_with_fields_opts <- function(api_url, token, fields, hash_ids = TRUE) {
  extra <- list(rawOrLabel = "label", fields = paste(fields, collapse = ","))
  body <- redcap_request(api_url, token, "record", extra)
  records <- fromJSON(body, simplifyDataFrame = TRUE)
  if (is.null(records) || (is.data.frame(records) && nrow(records) == 0)) {
    return(records)
  }
  if (hash_ids && length(fields) > 0) {
    records <- hash_record_ids(records, fields[1])
  }
  records
}

get_records_with_fields <- function(api_url, token, fields) {
  get_records_with_fields_opts(api_url, token, fields, hash_ids = TRUE)
}

get_records_with_fields_raw <- function(api_url, token, fields) {
  get_records_with_fields_opts(api_url, token, fields, hash_ids = FALSE)
}

get_records_with_fields_and_ids <- function(api_url, token, fields, record_ids) {
  body_params <- list(
    token      = token,
    content    = "record",
    format     = "json",
    type       = "flat",
    rawOrLabel = "label",
    fields     = paste(fields, collapse = ","),
    records    = paste(record_ids, collapse = ",")
  )
  resp <- tryCatch(
    request(api_url) |>
      req_method("POST") |>
      req_body_form(!!!body_params) |>
      req_perform(),
    error = function(e) stop(paste("Erreur requête:", conditionMessage(e)))
  )
  if (resp_status(resp) != 200L) {
    stop(sprintf("Erreur API (status %d): %s", resp_status(resp), resp_body_string(resp)))
  }
  fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
}

# instruments.R — Logique de modélisation des instruments et champs REDCap

# ---------------------------------------------------------------------------
# Champs de diffusion
# ---------------------------------------------------------------------------
find_diffusion_fields <- function(metadata) {
  metadata[
    str_ends(metadata$field_name, "_identification_level") |
      str_ends(metadata$field_name, "_audience"), ,
    drop = FALSE
  ]
}

# ---------------------------------------------------------------------------
# InstrumentConfig : détection automatique
# ---------------------------------------------------------------------------
build_instrument_configs <- function(instruments, metadata) {
  # Instruments ayant un champ _identification_level
  with_diffusion <- unique(
    metadata$form_name[str_ends(metadata$field_name, "_identification_level")]
  )

  configs <- list()
  for (i in seq_len(nrow(instruments))) {
    inst <- instruments[i, ]
    if (!inst$instrument_name %in% with_diffusion) next

    form_meta <- metadata[metadata$form_name == inst$instrument_name, ]

    has_identifiers <- any(form_meta$identifier == "y", na.rm = TRUE)
    file_fields <- form_meta$field_name[form_meta$field_type == "file"]

    configs[[length(configs) + 1]] <- list(
      name = inst$instrument_name,
      label = inst$instrument_label,
      id_level_field = paste0(inst$instrument_name, "_identification_level"),
      audience_field = paste0(inst$instrument_name, "_data_audience"),
      has_identifiers = has_identifiers,
      file_fields = file_fields
    )
  }
  configs
}

# ---------------------------------------------------------------------------
# Séparation des champs identifiants / non-identifiants
# ---------------------------------------------------------------------------
split_fields_by_identifier <- function(metadata, form_name) {
  sub <- metadata[
    metadata$form_name == form_name &
      metadata$field_type != "descriptive" &
      !str_ends(metadata$field_name, "_identification_level") &
      !str_ends(metadata$field_name, "_data_audience"),
  ]
  list(
    identifiers     = sub$field_name[sub$identifier == "y"],
    non_identifiers = sub$field_name[sub$identifier != "y"]
  )
}

get_form_fields <- function(metadata, form_name) {
  sub <- metadata[
    metadata$form_name == form_name &
      metadata$field_type != "descriptive" &
      !str_ends(metadata$field_name, "_identification_level") &
      !str_ends(metadata$field_name, "_data_audience"),
  ]
  sub$field_name
}

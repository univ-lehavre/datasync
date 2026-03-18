# nlp.R — Analyse NLP d'un champ textuel REDCap
# Dépendances : cld3, udpipe, tidytext, stopwords, topicmodels, tm

UDPIPE_MODELS_DIR <- file.path(normalizePath("~"), ".ecrin", "udpipe-models")

check_nlp_packages <- function() {
  required <- c("cld3", "udpipe", "tidytext", "stopwords", "topicmodels", "tm")
  missing_pkgs <- required[
    !vapply(required, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(missing_pkgs) > 0L) {
    stop(paste0(
      "Packages R manquants pour l'analyse NLP. Installez avec :\n",
      "  install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "),
      "))\n",
      "  # cld3 requiert libprotobuf-dev (Debian/Ubuntu) ou protobuf (Homebrew)\n"
    ))
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Modèles udpipe
# ---------------------------------------------------------------------------

udpipe_lang_name <- function(lang_code) {
  switch(lang_code,
    fr = "french",
    en = "english",
    NULL
  )
}

load_udpipe_model <- function(lang_code) {
  lang_name <- udpipe_lang_name(lang_code)
  if (is.null(lang_name)) {
    return(NULL)
  }

  dir.create(UDPIPE_MODELS_DIR, recursive = TRUE, showWarnings = FALSE)
  model_files <- list.files(
    UDPIPE_MODELS_DIR,
    pattern = paste0("^", lang_name, ".*\\.udpipe$"),
    full.names = TRUE
  )
  if (length(model_files) > 0L) {
    return(udpipe::udpipe_load_model(model_files[1]))
  }

  message(sprintf("Téléchargement du modèle udpipe '%s'...", lang_name))
  dl <- udpipe::udpipe_download_model(
    language  = lang_name,
    model_dir = UDPIPE_MODELS_DIR
  )
  udpipe::udpipe_load_model(dl$file_model)
}

# ---------------------------------------------------------------------------
# Chargement et filtrage
# ---------------------------------------------------------------------------

load_and_filter_texts <- function(csv_path, field, id_field) {
  df <- as.data.frame(read_csv(csv_path, show_col_types = FALSE))
  if (!(field %in% names(df))) {
    stop(sprintf("Champ '%s' absent de %s", field, csv_path))
  }
  if (!(id_field %in% names(df))) {
    stop(sprintf("Champ id '%s' absent de %s", id_field, csv_path))
  }
  vals <- as.character(df[[field]])
  keep <- !is.na(vals) & vals != "" & vals != "NA" & vals != "***"
  df <- df[keep, , drop = FALSE]
  # Utilise hashed_id si disponible et non obfusqué, sinon id_field
  id_col <- if ("hashed_id" %in% names(df)) {
    hids <- as.character(df[["hashed_id"]])
    if (all(hids == "***" | is.na(hids))) id_field else "hashed_id"
  } else {
    id_field
  }
  data.frame(
    id = as.character(df[[id_col]]),
    text = as.character(df[[field]]),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Détection de langue
# ---------------------------------------------------------------------------

detect_languages <- function(df) {
  langs <- cld3::detect_language(df$text)
  n_tokens <- lengths(strsplit(trimws(df$text), "\\s+"))
  short <- n_tokens < 30L
  langs[is.na(langs) | short] <- "unknown"
  df$langue <- langs
  df$n_tokens <- n_tokens
  df
}

# ---------------------------------------------------------------------------
# Tokenisation avec lemmatisation (udpipe pour fr/en, tidytext sinon)
# ---------------------------------------------------------------------------

tokenize_group <- function(df_lang, lang_code, output_dir) {
  model <- load_udpipe_model(lang_code)
  use_udpipe <- !is.null(model)

  # --- Étape 1 : annotation complète ---
  if (use_udpipe) {
    ann <- udpipe::udpipe_annotate(model, x = df_lang$text, doc_id = df_lang$id)
    ann_df <- as.data.frame(ann, detailed = FALSE)
    # Toutes les annotations avec indicateur de rétention UPOS
    upos_keep <- c("NOUN", "VERB", "ADJ", "ADV", "PROPN")
    ann_full <- data.frame(
      id = ann_df$doc_id,
      token = tolower(ann_df$token),
      lemma = tolower(ann_df$lemma),
      upos = ann_df$upos,
      upos_retenu = ann_df$upos %in% upos_keep,
      stringsAsFactors = FALSE
    )
    write_csv(ann_full, file.path(output_dir, "01_annotation.csv"))

    # Transformations token → lemme uniquement
    lemmatises <- ann_full[
      ann_full$upos_retenu & ann_full$token != ann_full$lemma,
      c("id", "token", "lemma", "upos"),
      drop = FALSE
    ]
    write_csv(lemmatises, file.path(output_dir, "01b_lemmatisation.csv"))

    tokens_raw <- ann_full[ann_full$upos_retenu, c("id", "token", "lemma", "upos"), drop = FALSE]
  } else {
    tokens_raw <- tidytext::unnest_tokens(df_lang, token, text, token = "words")
    tokens_raw$lemma <- tokens_raw$token
    tokens_raw$upos <- NA_character_
    write_csv(tokens_raw[, c("id", "token", "lemma", "upos")], file.path(output_dir, "01_annotation.csv"))
  }

  # --- Étape 2 : suppression stop-words (sur le lemme) ---
  if (lang_code %in% c("fr", "en")) {
    sw <- stopwords::stopwords(lang_code, source = "snowball")
    mask_sw <- tokens_raw$lemma %in% sw
    write_csv(tokens_raw[mask_sw, , drop = FALSE], file.path(output_dir, "02_stopwords_supprimes.csv"))
    tokens_sw <- tokens_raw[!mask_sw, , drop = FALSE]
  } else {
    write_csv(
      data.frame(id = character(), token = character(), lemma = character(), upos = character()),
      file.path(output_dir, "02_stopwords_supprimes.csv")
    )
    tokens_sw <- tokens_raw
  }
  write_csv(tokens_sw, file.path(output_dir, "02_apres_stopwords.csv"))

  # --- Étape 3 : longueur minimale (sur le lemme) ---
  mask_len <- nchar(tokens_sw$lemma) < 3L
  write_csv(tokens_sw[mask_len, , drop = FALSE], file.path(output_dir, "03_min_chars_supprimes.csv"))
  tokens_len <- tokens_sw[!mask_len, , drop = FALSE]
  write_csv(tokens_len, file.path(output_dir, "03_apres_min_chars.csv"))

  if (nrow(tokens_len) == 0L) {
    empty <- data.frame(id = character(), token = character(), stringsAsFactors = FALSE)
    write_csv(
      data.frame(id = character(), token = character()),
      file.path(output_dir, "04_hapax_supprimes.csv")
    )
    write_csv(empty, file.path(output_dir, "04_lemmes_finaux.csv"))
    return(empty)
  }

  # --- Étape 4 : réduction aux lemmes ---
  tokens_lemmes <- data.frame(
    id = tokens_len$id,
    token = tokens_len$lemma,
    stringsAsFactors = FALSE
  )

  # --- Étape 5 : suppression hapax ---
  doc_freq <- tapply(tokens_lemmes$id, tokens_lemmes$token, function(x) length(unique(x)))
  shared <- names(doc_freq)[doc_freq > 1L]
  hapax <- names(doc_freq)[doc_freq == 1L]
  write_csv(
    tokens_lemmes[tokens_lemmes$token %in% hapax, , drop = FALSE],
    file.path(output_dir, "04_hapax_supprimes.csv")
  )
  if (length(shared) > 0L) {
    tokens_lemmes <- tokens_lemmes[tokens_lemmes$token %in% shared, , drop = FALSE]
  }
  write_csv(tokens_lemmes, file.path(output_dir, "04_lemmes_finaux.csv"))

  tokens_lemmes[, c("id", "token"), drop = FALSE]
}

# ---------------------------------------------------------------------------
# TF-IDF
# ---------------------------------------------------------------------------

compute_tfidf <- function(tokens_df, lang_code, output_dir = NULL) {
  if (nrow(tokens_df) == 0L) {
    return(data.frame(
      langue = character(), token = character(),
      tf_idf_moyen = numeric(), n_docs = integer(),
      stringsAsFactors = FALSE
    ))
  }
  counts <- dplyr::count(tokens_df, id, token, name = "n")
  tfidf <- tidytext::bind_tf_idf(counts, token, id, n)

  # Export détail par document si output_dir fourni
  if (!is.null(output_dir)) {
    detail <- tfidf[, c("id", "token", "n", "tf", "idf", "tf_idf")]
    detail <- detail[order(detail$id, -detail$tf_idf), ]
    write_csv(detail, file.path(output_dir, "05_tfidf_detail.csv"))
  }

  summary <- dplyr::summarise(
    dplyr::group_by(tfidf, token),
    tf_idf_moyen = mean(tf_idf),
    n_docs       = dplyr::n_distinct(id),
    .groups      = "drop"
  )
  summary$langue <- lang_code
  summary <- summary[order(-summary$tf_idf_moyen), ]
  summary[, c("langue", "token", "tf_idf_moyen", "n_docs")]
}

# ---------------------------------------------------------------------------
# LDA
# ---------------------------------------------------------------------------

run_lda <- function(tokens_df, lang_code, n_docs) {
  if (n_docs < 5L || lang_code == "unknown") {
    return(NULL)
  }

  k_max <- min(5L, floor(n_docs / 2L))
  if (k_max < 2L) {
    return(NULL)
  }

  n_distinct_tokens <- length(unique(tokens_df$token))
  if (n_distinct_tokens < 10L) {
    return(NULL)
  }

  counts <- dplyr::count(tokens_df, id, token, name = "n")
  dtm <- tidytext::cast_dtm(counts, id, token, n)

  best_k <- 2L
  best_perp <- Inf
  models <- list()

  for (k in seq(2L, k_max)) {
    lda_ctrl <- list(seed = 42L)
    m <- topicmodels::LDA(dtm, k = k, method = "VEM", control = lda_ctrl)
    p <- topicmodels::perplexity(m)
    models[[as.character(k)]] <- m
    if (p < best_perp) {
      best_perp <- p
      best_k <- k
    }
  }

  model <- models[[as.character(best_k)]]
  list(
    k            = best_k,
    topics_df    = extract_lda_topics(model, lang_code, best_k),
    individus_df = extract_lda_individus(model, lang_code, best_k, tokens_df)
  )
}

extract_lda_topics <- function(model, lang_code, k) {
  beta <- tidytext::tidy(model, matrix = "beta")
  top <- dplyr::slice_max(
    dplyr::group_by(beta, topic),
    order_by = beta, n = 10L, with_ties = FALSE
  )
  top <- dplyr::arrange(top, topic, dplyr::desc(beta))
  top$rang <- sequence(tabulate(top$topic))
  top$langue <- lang_code
  top$k <- k
  cols <- c("langue", "k", "topic", "rang", "term", "beta")
  as.data.frame(top[, cols, drop = FALSE])
}

extract_lda_individus <- function(model, lang_code, k, tokens_df) {
  gamma <- tidytext::tidy(model, matrix = "gamma")

  dominant <- dplyr::summarise(
    dplyr::group_by(gamma, document),
    topic_dominant = topic[which.max(gamma)],
    .groups        = "drop"
  )

  wide <- tidyr::pivot_wider(gamma,
    names_from   = topic,
    values_from  = gamma,
    names_prefix = "gamma_"
  )

  result <- merge(dominant, wide, by = "document")
  result$langue <- lang_code
  names(result)[names(result) == "document"] <- "userid"
  ind_cols <- c("userid", "langue", "topic_dominant", paste0("gamma_", seq_len(k)))
  result[, ind_cols, drop = FALSE]
}

# ---------------------------------------------------------------------------
# Pipeline principal
# ---------------------------------------------------------------------------

run_nlp_pipeline <- function(csv_path, field, id_field, output_dir) {
  df <- load_and_filter_texts(csv_path, field, id_field)
  if (nrow(df) == 0L) {
    write_nlp_empty(output_dir)
    return(list(ok = TRUE, langues = list(), lda_k = list()))
  }

  df <- detect_languages(df)

  # Export 00 : source avec langue et n_tokens (avant split par langue)
  write_csv(
    df[, c("id", "langue", "n_tokens", "text"), drop = FALSE],
    file.path(output_dir, "00_source.csv")
  )

  lang_groups <- split(df, df$langue)

  all_tfidf <- list()
  all_topics <- list()
  all_individus <- list()
  langues_count <- list()
  lda_k <- list()

  for (lang_code in names(lang_groups)) {
    grp <- lang_groups[[lang_code]]
    n_docs <- nrow(grp)
    langues_count[[lang_code]] <- n_docs

    lang_debug_dir <- file.path(output_dir, paste0("debug-", lang_code))
    dir.create(lang_debug_dir, recursive = TRUE, showWarnings = FALSE)

    tokens <- tokenize_group(grp, lang_code, lang_debug_dir)
    if (nrow(tokens) == 0L) next

    all_tfidf[[lang_code]] <- compute_tfidf(tokens, lang_code, output_dir = lang_debug_dir)

    lda_res <- run_lda(tokens, lang_code, n_docs)
    if (!is.null(lda_res)) {
      lda_k[[lang_code]] <- lda_res$k
      all_topics[[lang_code]] <- lda_res$topics_df
      all_individus[[lang_code]] <- lda_res$individus_df
    }
  }

  empty_tfidf <- data.frame(
    langue = character(), token = character(),
    tf_idf_moyen = numeric(), n_docs = integer(),
    stringsAsFactors = FALSE
  )
  empty_topics <- data.frame(
    langue = character(), k = integer(), topic = integer(),
    rang = integer(), term = character(), beta = numeric(),
    stringsAsFactors = FALSE
  )
  empty_individus <- data.frame(
    userid = character(), langue = character(), topic_dominant = integer(),
    stringsAsFactors = FALSE
  )

  tfidf_df <- if (length(all_tfidf) > 0L) do.call(rbind, all_tfidf) else empty_tfidf
  topics_df <- if (length(all_topics) > 0L) do.call(rbind, all_topics) else empty_topics
  individus_df <- if (length(all_individus) > 0L) do.call(rbind, all_individus) else empty_individus

  write_csv(tfidf_df, file.path(output_dir, "tfidf.csv"))
  write_csv(topics_df, file.path(output_dir, "lda_topics.csv"))
  write_csv(individus_df, file.path(output_dir, "lda_individus.csv"))

  list(ok = TRUE, langues = langues_count, lda_k = lda_k)
}

write_nlp_empty <- function(output_dir) {
  empty_tfidf <- data.frame(
    langue = character(), token = character(),
    tf_idf_moyen = numeric(), n_docs = integer()
  )
  empty_topics <- data.frame(
    langue = character(), k = integer(), topic = integer(),
    rang = integer(), term = character(), beta = numeric()
  )
  empty_individus <- data.frame(
    userid = character(), langue = character(), topic_dominant = integer()
  )
  write_csv(empty_tfidf, file.path(output_dir, "tfidf.csv"))
  write_csv(empty_topics, file.path(output_dir, "lda_topics.csv"))
  write_csv(empty_individus, file.path(output_dir, "lda_individus.csv"))
}

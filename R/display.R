# display.R — Affichage console des enregistrements (tableaux, statistiques)

# ---------------------------------------------------------------------------
# Troncature de chaîne
# ---------------------------------------------------------------------------
truncate_string <- function(s, max_len) {
  if (nchar(s) <= max_len) return(s)
  paste0(substr(s, 1, max_len - 1), "\u2026")
}

# ---------------------------------------------------------------------------
# Tableau de records en console
# ---------------------------------------------------------------------------
print_records_table <- function(records, columns, max_rows = 5) {
  if (is.null(records) || nrow(records) == 0 || length(columns) == 0) return(invisible(NULL))

  max_col_width <- 25L
  col_widths    <- nchar(columns)

  row_count <- min(nrow(records), max_rows)

  for (i in seq_len(row_count)) {
    for (j in seq_along(columns)) {
      col <- columns[j]
      val_str <- if (col %in% names(records)) as.character(records[[col]][i]) else ""
      if (is.na(val_str)) val_str <- ""
      if (nchar(val_str) > col_widths[j]) col_widths[j] <- nchar(val_str)
    }
  }
  col_widths <- pmin(col_widths, max_col_width)

  # En-tête
  header <- paste(mapply(function(col, w) formatC(truncate_string(col, w), width = -w), columns, col_widths), collapse = " ")
  cat("  ", header, "\n", sep = "")
  sep <- paste(vapply(col_widths, function(w) strrep("\u2500", w), character(1)), collapse = " ")
  cat("  ", sep, "\n", sep = "")

  for (i in seq_len(row_count)) {
    row <- paste(mapply(function(col, w) {
      val <- if (col %in% names(records)) as.character(records[[col]][i]) else ""
      if (is.na(val)) val <- ""
      formatC(truncate_string(val, w), width = -w)
    }, columns, col_widths), collapse = " ")
    cat("  ", row, "\n", sep = "")
  }

  if (nrow(records) > max_rows) {
    cat(str_gray(sprintf("  ... et %d autres lignes\n", nrow(records) - max_rows)))
  }
}

# ---------------------------------------------------------------------------
# Statistiques de records en console
# ---------------------------------------------------------------------------
print_records_stats <- function(records, columns) {
  if (is.null(records) || nrow(records) == 0) return(invisible(NULL))
  cat(sprintf("\n  %s\n", str_bold("Statistiques:")))

  for (col in columns) {
    cat(sprintf("\n    %s:\n", str_cyan(col)))
    vals <- if (col %in% names(records)) as.character(records[[col]]) else rep("", nrow(records))
    vals[is.na(vals)] <- ""
    empty_count  <- sum(vals == "" | vals == "NA")
    non_empty    <- vals[vals != "" & vals != "NA"]

    if (empty_count > 0) {
      pct <- empty_count * 100 / nrow(records)
      cat(str_gray(sprintf("      Vide: %d (%.0f%%)\n", empty_count, pct)))
    }

    freq <- sort(table(non_empty), decreasing = TRUE)
    max_display <- min(5L, length(freq))
    for (i in seq_len(max_display)) {
      pct <- freq[i] * 100 / nrow(records)
      cat(sprintf("      %s: %d (%.0f%%)\n", truncate_string(names(freq)[i], 30), freq[i], pct))
    }
    if (length(freq) > 5L) {
      cat(str_gray(sprintf("      ... et %d autres valeurs\n", length(freq) - 5L)))
    }
  }
}

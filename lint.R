#!/usr/bin/env Rscript
# lint.R — Formatting et linting du projet
#
# Usage:
#   Rscript lint.R        # vérifie uniquement (lint sans fix)
#   Rscript lint.R fix    # formate avec styler puis vérifie avec lintr

args <- commandArgs(trailingOnly = TRUE)
fix_mode <- length(args) > 0 && tolower(args[1]) == "fix"

r_files <- c("ecrin.R", list.files("R", pattern = "\\.R$", full.names = TRUE))

# ---------------------------------------------------------------------------
# styler
# ---------------------------------------------------------------------------
if (fix_mode) {
  cat("--- styler ---\n")
  for (f in r_files) {
    result <- styler::style_file(f)
    if (any(result$changed)) {
      cat(sprintf("  reformatté : %s\n", f))
    }
  }
  cat("  OK\n\n")
}

# ---------------------------------------------------------------------------
# lintr
# ---------------------------------------------------------------------------
cat("--- lintr ---\n")
lint_fail <- FALSE

for (f in r_files) {
  lints <- lintr::lint(f)
  if (length(lints) > 0) {
    print(lints)
    lint_fail <- TRUE
  }
}

if (lint_fail) {
  cat("\nDes problèmes ont été trouvés.\n")
  quit(status = 1)
} else {
  cat("  OK\n")
}

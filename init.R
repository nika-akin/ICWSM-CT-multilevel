#!/usr/bin/env Rscript
# init.R — One-click project setup
# Run: Rscript init.R

cat("=== Voat CT Analysis Setup ===\n\n")

# 1. R Version Check ----------------------------------------------------------
r_min <- "4.2.0"
if (getRversion() < r_min) {
  stop(sprintf("R >= %s required. You have %s", r_min, getRversion()))
}
cat(sprintf("[OK] R version: %s\n", getRversion()))

# 2. Install renv if missing --------------------------------------------------
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("[INFO] Installing renv...\n")
  install.packages("renv", repos = "https://cloud.r-project.org")
}

# 3. Restore packages from renv.lock ------------------------------------------
if (file.exists("renv.lock")) {
  cat("[INFO] Restoring renv environment...\n")
  renv::restore(prompt = FALSE)
} else {
  cat("[WARN] renv.lock not found. Initializing renv...\n")
  renv::init(bare = TRUE)
}

# 4. Core pipeline packages ---------------------------------------------------
core_pkgs <- c(
  "targets", "tarchetypes", "tidyverse", "lubridate", "lme4", "survival",
  "survminer", "mgcv", "gratia", "slider", "data.table", "broom", "ggpubr",
  "patchwork", "ggeffects", "sjPlot", "forcats", "tidyr", "irr", "pROC",
  "caret", "psych", "hexbin", "gridExtra", "readr", "jtools", "report",
  "ggplot2", "quarto", "rsconnect", "shiny", "shinydashboard", "DT",
  "withr", "here"
)

missing <- core_pkgs[!vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  cat(sprintf("[INFO] Installing missing packages: %s\n", paste(missing, collapse = ", ")))
  renv::install(missing)
  renv::snapshot(prompt = FALSE)
} else {
  cat("[OK] All core packages installed.\n")
}

# 5. Directory structure ------------------------------------------------------
dirs <- c("data", "output", "figures", "reports", "shiny", "docs", "R")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, showWarnings = FALSE)
    cat(sprintf("[OK] Created directory: %s/\n", d))
  }
}

# 6. Data file check ----------------------------------------------------------
required_files <- c(
  "data/seperated_annotation.csv",
  "data/user_info.csv",
  "data/subverse_scores_wa.csv",
  "data/subverse.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  cat("[ERROR] Missing required data files:\n")
  for (f in missing_files) cat(sprintf("  - %s\n", f))
  cat("\nPlease place the data files in the data/ directory and re-run.\n")
} else {
  cat("[OK] All required data files present.\n")
}

# 7. Git hooks (optional) -----------------------------------------------------
if (dir.exists(".git")) {
  if (!file.exists(".git/hooks/pre-commit")) {
    writeLines("#!/bin/sh\nmake pipeline\n", ".git/hooks/pre-commit")
    Sys.chmod(".git/hooks/pre-commit", mode = "0755")
    cat("[OK] Added pre-commit hook to run pipeline.\n")
  }
}

cat("\n=== Setup Complete ===\n")
cat("Next steps:\n")
cat("  1. make pipeline   # Run the full targets pipeline\n")
cat("  2. make reports    # Render Quarto reports\n")
cat("  3. make shiny      # Launch the dashboard\n")

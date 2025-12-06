############################################################
# GET_STARTED.R
# ---------------------------------------------------------
# Purpose:
# - Make it easy for someone to start from a fresh machine
# - Install all required packages (if missing)
# - Clarify main files and how to run the analyses
#
# How to use:
# 1) Open this project in RStudio (File -> Open Project…).
# 2) Open this file and click "Source" (or run: source("GET_STARTED.R")).
# 3) Then open and knit: "Wave 1+2 Analyses FINAL FOR MANUSCRIPT.Rmd".
############################################################

#### 1. Global options ####
options(
  stringsAsFactors = FALSE,
  max.print = 20000,
  scipen = 1000
)

cat("Global options set.\n\n")

#### 2. Packages to install ####

required_pkgs <- c(
  # Core tidy tools
  "tidyverse",   # ggplot2, dplyr, tidyr, lubridate, etc.
  
  # Time series / forecasting
  "forecast",
  "tsibble",
  "statcomp",
  
  # Psychometrics / stats
  "psych",
  "irr",
  "lme4",
  "emmeans",
  "car",
  "Hmisc",
  "moments",
  "partR2",
  
  # Graphics / plotting helpers
  "ggsci",
  "CGPfunctions",
  "ggpubr",
  "ggdist",
  "tidyquant",
  "jtools",
  
  # Bayesian / Bayes factors
  "rstanarm",
  "bayestestR",
  
  # For knitting / reports
  "rmarkdown",
  "knitr"
)

installed <- rownames(installed.packages())
new_pkgs <- required_pkgs[!required_pkgs %in% installed]

if (length(new_pkgs) > 0) {
  cat("Installing missing packages:\n")
  print(new_pkgs)
  install.packages(new_pkgs)
} else {
  cat("All required packages are already installed.\n")
}

cat("\nPackage check complete.\n\n")

#### 3. Project file → purpose map ####

cat("Project file map:\n")
cat("  - GET_STARTED.R\n")
cat("      * This setup script: installs packages, documents project structure.\n\n")

cat("  - Wave 1+2 Analyses FINAL FOR MANUSCRIPT.Rmd\n")
cat("      * Main analysis document: all manuscript analyses, figures, and tables.\n\n")

cat("  - (Optional) data/ directory (adjust as needed):\n")
cat("      * data/raw/      - raw input data files (e.g., Wave 1 / Wave 2 CSVs).\n")
cat("      * data/derived/  - cleaned / merged datasets used by the Rmd.\n\n")

cat("  - (Optional) scripts/ directory (if present):\n")
cat("      * scripts/01_clean_data.R       - cleaning + merging data.\n")
cat("      * scripts/02_descriptives.R     - descriptive stats and plots.\n\n")

cat("  - (Optional) outputs/ directory:\n")
cat("      * outputs/tables/   - tables for the manuscript.\n")
cat("      * outputs/figures/  - figures for the manuscript.\n\n")

cat("Adjust the file map above to match your actual structure.\n\n")

#### 4. Optional helpers (commented out by default) ####
# If you want a one-liner to run everything, you can uncomment:

# cat('Running data cleaning script...\n')
# source("scripts/01_clean_data.R")

# cat('Rendering main manuscript Rmd...\n')
# rmarkdown::render("Wave 1+2 Analyses FINAL FOR MANUSCRIPT.Rmd")

cat("Setup complete. You can now open and knit:\n")
cat('  "Wave 1+2 Analyses FINAL FOR MANUSCRIPT.Rmd"\n')
cat("from within RStudio.\n")
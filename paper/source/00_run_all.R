library(here)

scripts <- c(
  "01_coarsen_analysis_hbef.R",
  "02_coarsen_figure_hbef.R",
  "03_coarsen_example_figure.R",
  "04_coarsen_analysis_plynlimon.R",
  "05_coarsen_figure_plynlimon.R",
  "06_coarsen_analysis_neon.R",
  "07_coarsen_figure_neon.R",
  "08_macrosheds_compare.R",
  "09_macrosheds_descriptive.R",
  "10_ca_correlation.R",
  "11_misc_figures.R",
  "12_hbef_method_comparison.R"
)

required_packages <- c(
  "tidyverse", "here", "feather", "lfstat", "lubridate",
  "RiverLoad", "zoo", "patchwork", "EGRET", "macrosheds", "imputeTS"
)

required_data <- c(
  "w3_sensor_wdisch.feather",
  "data/hbef/HBEFdata_All_2022-11-17.csv",
  "data/plynlimon/PlynlimonHighFrequencyHydrochemistry.csv",
  "data/macrosheds/sites.csv",
  "data/neon/ms_stream_order.csv",
  "data/load_annual.csv",
  "data/hbef_published_flux/ws3_stream_monthly_flux_gHa.csv"
)

# --- Check prerequisites ---

cat("Checking required packages...\n")
missing_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "),
       "\nInstall with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))")
}
cat("  All packages found.\n")

cat("Checking required data files...\n")
missing_data <- required_data[!file.exists(here(required_data))]
if (length(missing_data) > 0) {
  warning("Missing data files:\n  ", paste(missing_data, collapse = "\n  "),
          "\n  Some scripts may fail. See data/README.md for instructions.")
}
cat("  Data check complete.\n\n")

# --- Parse start_from argument ---

args <- commandArgs(trailingOnly = TRUE)
start_from <- 1L
if (length(args) > 0) {
  start_from <- as.integer(args[1])
  if (is.na(start_from) || start_from < 1 || start_from > length(scripts)) {
    stop("start_from must be between 1 and ", length(scripts))
  }
  cat("Starting from script", start_from, "\n\n")
}

# --- Run scripts ---

dir.create(here("data", "coarsen_hbef"), showWarnings = FALSE)
dir.create(here("data", "coarsen_plynlimon"), showWarnings = FALSE)
dir.create(here("data", "coarsen_neon"), showWarnings = FALSE)
dir.create(here("paper", "figures"), showWarnings = FALSE)

total_start <- proc.time()

for (i in seq(start_from, length(scripts))) {
  script <- scripts[i]
  cat(sprintf("[%02d/%02d] Running %s ...\n", i, length(scripts), script))
  t0 <- proc.time()

  tryCatch(
    source(here("paper", "source", script), local = new.env(parent = globalenv())),
    error = function(e) {
      cat(sprintf("  FAILED: %s\n", conditionMessage(e)))
    }
  )

  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("  Done in %.1f sec\n\n", elapsed))
}

total_elapsed <- (proc.time() - total_start)["elapsed"]
cat(sprintf("All scripts complete. Total time: %.1f sec (%.1f min)\n",
            total_elapsed, total_elapsed / 60))

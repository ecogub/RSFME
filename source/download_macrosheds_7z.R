library(macrosheds)
library(here)

ms_root <- here('data', 'macrosheds')

sevenz <- normalizePath('C:/Program Files/7-Zip/7z.exe', mustWork = FALSE)
if (!file.exists(sevenz)) stop("7-Zip not found at: ", sevenz)

# Override unzip to use 7-Zip (R's internal unzip fails on Windows)
original_unzip <- utils::unzip
my_unzip <- function(zipfile, files = NULL, list = FALSE, overwrite = TRUE,
                     junkpaths = FALSE, exdir = '.', unzip = 'internal',
                     setTimes = FALSE) {
  cat('  [7z] extracting', basename(zipfile), '... ')
  res <- system2(sevenz, args = c('x', paste0('-o', exdir), '-y', zipfile),
                 stdout = FALSE, stderr = FALSE)
  if (res == 0) {
    cat('OK\n')
    return(invisible(list.files(exdir, recursive = TRUE, full.names = TRUE)))
  } else {
    warning(paste('7z extraction failed with exit code', res))
    return(invisible(character(0)))
  }
}
ns <- getNamespace('utils')
unlockBinding('unzip', ns)
assign('unzip', my_unzip, envir = ns)
lockBinding('unzip', ns)

cat("Downloading MacroSheds core data to:", ms_root, "\n")
cat("Using 7-Zip for zip extraction\n\n")

ms_download_core_data(
  macrosheds_root = ms_root,
  domains = 'all',
  skip_existing = TRUE,
  quiet = FALSE
)

# Restore original unzip
unlockBinding('unzip', ns)
assign('unzip', original_unzip, envir = ns)
lockBinding('unzip', ns)

n_files <- length(list.files(ms_root, recursive = TRUE, pattern = '\\.feather$'))
cat(sprintf("\nDone. %d feather files in %s\n", n_files, ms_root))

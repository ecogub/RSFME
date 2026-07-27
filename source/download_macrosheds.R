library(here)

ms_dir <- here('data', 'macrosheds')
if (!dir.exists(ms_dir)) dir.create(ms_dir, recursive = TRUE)

edi_url <- 'https://portal.edirepository.org/nis/dataviewer?packageid=edi.1262.2'
zip_path <- here('edi.1262.2.zip')

if (!file.exists(zip_path)) {
    cat("Downloading MacroSheds v2 from EDI...\n")
    download.file(edi_url, zip_path, mode = 'wb')
}

cat("Extracting to:", ms_dir, "\n")

sevenz <- normalizePath('C:/Program Files/7-Zip/7z.exe', mustWork = FALSE)
if (file.exists(sevenz)) {
    system2(sevenz, args = c('x', paste0('-o', ms_dir), '-y', zip_path),
            stdout = FALSE, stderr = FALSE)
} else {
    unzip(zip_path, exdir = ms_dir)
}

n_csv <- length(list.files(ms_dir, pattern = 'timeseries_.*\\.csv$'))
cat(sprintf("Done. %d domain timeseries CSVs in %s\n", n_csv, ms_dir))

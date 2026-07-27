library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(EGRET)
library(parallel)

source(here('source/helper_functions.R'))
source(here('source/egret_overwrites.R'))
source(here('ms_overwrites.R'))
source(here('source/flux_methods.R'))
source(here('source/usgs_helpers.R'))

# site metadata from EDI download
site_info <- read_csv(here('data', 'macrosheds', 'sites.csv'), show_col_types = FALSE)

# flux-convertible variables: those that appear in the stream_flux category
flux_vars <- read_csv(here('data', 'macrosheds', 'variables_timeseries.csv'),
                      show_col_types = FALSE) %>%
    filter(chem_category == 'stream_flux') %>%
    pull(variable_code) %>%
    unique()

# all domains available in the EDI download
domain_files <- list.files(here('data', 'macrosheds'), pattern = '^timeseries_.*\\.csv$')
domains <- str_replace(domain_files, 'timeseries_(.*)\\.csv', '\\1')

# --- per-site-solute-year flux computation ---
compute_site_solute_fluxes <- function(sc, target_solute, raw_data_q, domain_chem,
                                        raw_data_con_in, area, lat, long, site_info, nwk) {
    # flux_methods.R functions reference 'area' as a free variable from .GlobalEnv
    assign('area', area, envir = .GlobalEnv)

    results <- list()
    diags <- list()

    raw_data_con <- domain_chem %>%
        filter(site_code == sc, ms_interp == 0, val > 0, var == target_solute) %>%
        select(datetime, val) %>%
        na.omit()

    if(nrow(raw_data_con) == 0) return(list(results = NULL, diags = NULL))

    # find acceptable years — guard against empty Q data
    q_filtered <- raw_data_q %>%
        mutate(date = date(datetime)) %>%
        filter(ms_interp == 0, !is.na(val)) %>%
        distinct(date, .keep_all = TRUE)

    if(nrow(q_filtered) == 0) return(list(results = NULL, diags = NULL))

    q_check <- q_filtered %>%
        mutate(water_year = water_year(datetime, origin = "usgs")) %>%
        group_by(water_year) %>%
        summarise(n = n(), .groups = 'drop') %>%
        filter(n >= 311)

    conc_filtered <- raw_data_con %>%
        mutate(date = date(datetime)) %>%
        filter(!is.na(val)) %>%
        distinct(date, .keep_all = TRUE)

    if(nrow(conc_filtered) == 0) return(list(results = NULL, diags = NULL))

    conc_check <- conc_filtered %>%
        mutate(water_year = water_year(date, origin = "usgs"),
               quart = quarter(date)) %>%
        group_by(water_year) %>%
        summarise(count = n_distinct(quart), n = n(), .groups = 'drop') %>%
        filter(n >= 4, count > 3)

    if(nrow(q_check) == 0 || nrow(conc_check) == 0) return(list(results = NULL, diags = NULL))

    good_years <- q_check$water_year[q_check$water_year %in% conc_check$water_year]
    if(length(good_years) == 0) return(list(results = NULL, diags = NULL))

    daily_data_con <- raw_data_con %>%
        mutate(date = date(datetime)) %>%
        group_by(date) %>%
        summarize(val = mean_or_x(val), .groups = 'drop') %>%
        mutate(site_code = sc, var = 'con') %>%
        select(site_code, datetime = date, var, val)

    daily_data_q <- raw_data_q %>%
        mutate(date = date(datetime)) %>%
        group_by(date) %>%
        summarize(val = mean_or_x(val), .groups = 'drop') %>%
        mutate(site_code = sc, var = 'q_lps') %>%
        select(site_code, datetime = date, var, val)

    q_df <- daily_data_q %>%
        pivot_wider(names_from = var, values_from = val)

    raw_data_full <- bind_rows(daily_data_con, daily_data_q) %>%
        pivot_wider(names_from = var, values_from = val, id_cols = c(site_code, datetime)) %>%
        mutate(wy = water_year(datetime, origin = 'usgs')) %>%
        filter(wy %in% good_years)

    con_full <- raw_data_full %>%
        mutate(wy = as.numeric(as.character(wy))) %>%
        select(site_code, datetime, con, wy) %>%
        na.omit()

    # WRTDS
    flux_annual_wrtds <- NA
    tryCatch({
        flux_annual_wrtds <- calculate_wrtds(
            chem_df = con_full, q_df = q_df, ws_size = area,
            lat = lat, long = long, datecol = 'datetime',
            agg = 'annual', minNumObs = 100, minNumUncen = 50)
    }, error = function(e) {})

    for(k in 1:length(good_years)){
        target_year <- as.numeric(as.character(good_years[k]))

        raw_data_target_year <- raw_data_full %>%
            mutate(wy = as.numeric(as.character(wy))) %>%
            filter(wy == target_year)

        q_target_year <- raw_data_target_year %>%
            select(site_code, datetime, q_lps, wy) %>% na.omit()
        con_target_year <- raw_data_target_year %>%
            select(site_code, datetime, con, wy) %>% na.omit()

        if(nrow(q_target_year) < 2 || nrow(con_target_year) < 2) next

        chem_df <- tryCatch(errors::drop_errors(con_target_year),
                            error = function(e) con_target_year)
        q_df_yr <- tryCatch(errors::drop_errors(q_target_year),
                            error = function(e) q_target_year)

        flux_annual_average <- raw_data_target_year %>%
            group_by(wy) %>%
            summarize(q_lps = mean(q_lps, na.rm = TRUE),
                      con = mean(con, na.rm = TRUE), .groups = 'drop') %>%
            mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
            pull(flux)

        flux_annual_pw <- calculate_pw(chem_df, q_df_yr, datecol = 'datetime')
        flux_annual_beale <- calculate_beale(chem_df, q_df_yr, datecol = 'datetime')
        flux_annual_rating <- calculate_rating(chem_df, q_df_yr, datecol = 'datetime')

        rating_filled_df <- generate_residual_corrected_con(
            chem_df = chem_df, q_df = q_df_yr,
            datecol = 'datetime', sitecol = 'site_code')
        flux_annual_comp <- calculate_composite_from_rating_filled_df(rating_filled_df)

        # recommended method selection (Aulenbach et al 2016)
        paired_df <- q_df_yr %>%
            full_join(chem_df, by = c('datetime', 'site_code', 'wy')) %>%
            na.omit() %>%
            filter(q_lps > 0, is.finite(q_lps))

        if(nrow(paired_df) < 3) {
            ideal_method <- NA
            r_squared <- NA; resid_acf <- NA; con_acf <- NA
        } else {
            q_log <- log10(paired_df$q_lps)
            c_log <- log10(paired_df$con)
            model_data <- tibble(c_log, q_log) %>%
                filter(is.finite(c_log), is.finite(q_log)) %>% na.omit()

            if(nrow(model_data) < 3) {
                ideal_method <- NA
                r_squared <- NA; resid_acf <- NA; con_acf <- NA
            } else {
                rating_summary <- summary(lm(model_data$c_log ~ model_data$q_log, singular.ok = TRUE))
                r_squared <- rating_summary$r.squared
                resid_acf <- abs(acf(rating_summary$residuals, lag.max = 1, plot = FALSE)$acf[2])
                con_acf <- abs(acf(paired_df$con, lag.max = 1, plot = FALSE)$acf[2])

                if(!is.nan(r_squared)) {
                    ideal_method <- if(r_squared > 0.3) {
                        ifelse(resid_acf > 0.2, 'composite', 'rating')
                    } else {
                        ifelse(con_acf > 0.20, 'pw', 'average')
                    }
                } else {
                    ideal_method <- NA
                }
            }
        }

        year_out <- tibble(
            wy = target_year,
            val = c(flux_annual_average, flux_annual_pw, flux_annual_beale,
                    flux_annual_rating, flux_annual_comp$flux[1]),
            site_code = sc, var = target_solute,
            method = c('average', 'pw', 'beale', 'rating', 'composite')) %>%
            mutate(ms_recommended = ifelse(method == ideal_method, 1, 0))
        results[[length(results) + 1]] <- year_out

        diags[[length(diags) + 1]] <- tibble(
            network = site_info$network[site_info$site_code == sc][1],
            domain = nwk, site_code = sc, var = target_solute,
            water_year = target_year, cq_rsquared = r_squared,
            cq_resid_acf = resid_acf, c_acf = con_acf,
            n_c_obs = nrow(con_target_year), n_q_obs = nrow(q_target_year),
            n_paired_cq_obs = nrow(paired_df))
    } # end year loop

    # WRTDS
    if(is.data.frame(flux_annual_wrtds)) {
        wrtds_out <- flux_annual_wrtds %>%
            filter(wy %in% good_years) %>%
            rename(val = flux) %>%
            mutate(site_code = sc, var = target_solute,
                   method = 'wrtds', ms_recommended = 0)
        results[[length(results) + 1]] <- wrtds_out
    }

    list(results = bind_rows(results), diags = bind_rows(diags))
}

# --- per-domain processing function ---
process_domain <- function(nwk, site_info, flux_vars) {
    cat(sprintf("\n starting domain: %s\n", nwk))

    ts_path <- here('data', 'macrosheds', glue('timeseries_{nwk}.csv'))
    domain_data <- read_csv(ts_path, show_col_types = FALSE)

    domain_q <- domain_data %>%
        filter(var_category == 'discharge') %>%
        rename(datetime = date)
    domain_chem <- domain_data %>%
        filter(var_category == 'stream_chemistry') %>%
        rename(datetime = date)
    rm(domain_data); gc()

    domain_sites <- unique(domain_chem$site_code)
    domain_results <- list()

    for(sc in domain_sites) {
        area <- site_info %>% filter(site_code == sc) %>% pull(ws_area_ha)
        lat <- site_info %>% filter(site_code == sc) %>% pull(latitude)
        long <- site_info %>% filter(site_code == sc) %>% pull(longitude)

        if(length(area) == 0 || length(lat) == 0) next

        raw_data_q <- domain_q %>% filter(site_code == sc)
        raw_data_con_in <- domain_chem %>%
            filter(site_code == sc, ms_interp == 0, var %in% flux_vars)

        solutes <- unique(raw_data_con_in$var)
        if(length(solutes) == 0) next

        cat(sprintf("  FLUX CALCS: %s (%d solutes)\n", sc, length(solutes)))

        for(target_solute in solutes) {
            res <- tryCatch(
                compute_site_solute_fluxes(sc, target_solute, raw_data_q, domain_chem,
                                           raw_data_con_in, area, lat, long, site_info, nwk),
                error = function(e) {
                    cat(sprintf("    ERROR %s/%s: %s\n", sc, target_solute, e$message))
                    list(results = NULL, diags = NULL)
                }
            )
            if(!is.null(res$results)) domain_results[[length(domain_results) + 1]] <- res
        }
    }

    if(length(domain_results) == 0) return(list(results = NULL, diags = NULL))

    list(
        results = bind_rows(lapply(domain_results, `[[`, 'results')),
        diags = bind_rows(lapply(domain_results, `[[`, 'diags'))
    )
}

# --- run all domains sequentially (swap lapply -> parLapply for parallelism) ---
cat(sprintf("Processing %d domains...\n", length(domains)))

all_domain_results <- lapply(domains, process_domain,
                              site_info = site_info, flux_vars = flux_vars)

# combine
out_frame <- bind_rows(Filter(Negate(is.null), lapply(all_domain_results, `[[`, 'results')))
diag_frame <- bind_rows(Filter(Negate(is.null), lapply(all_domain_results, `[[`, 'diags')))

if(nrow(out_frame) == 0) {
    cat("\nNo results produced. Check error messages above.\n")
    quit(status = 1)
}

# write combined output as CSV
out_csv <- out_frame %>%
    mutate(network = site_info$network[match(site_code, site_info$site_code)],
           domain = site_info$domain[match(site_code, site_info$site_code)]) %>%
    rename(water_year = wy, load = val) %>%
    select(network, domain, site_code, var, water_year, load, method, ms_recommended) %>%
    arrange(domain, site_code, var, water_year, method)

write_csv(out_csv, here('data', 'load_annual.csv'))
write_csv(diag_frame, here('data', 'load_annual_diagnostics.csv'))

cat(sprintf("\nDone. %d load estimates across %d sites written to data/load_annual.csv\n",
            nrow(out_csv), length(unique(out_csv$site_code))))

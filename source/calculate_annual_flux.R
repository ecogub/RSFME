library(tidyverse)
library(here)
library(glue)
library(lubridate)
library(EGRET)

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

# output accumulator
out_frame <- tibble(wy = as.integer(),
                    site_code = as.character(),
                    var = as.character(),
                    val = as.numeric(),
                    method = as.character(),
                    ms_recommended = as.integer())

# diagnostics accumulator
diag_frame <- tibble(network = as.character(),
                     domain = as.character(),
                     site_code = as.character(),
                     var = as.character(),
                     water_year = as.integer(),
                     cq_rsquared = as.numeric(),
                     cq_resid_acf = as.numeric(),
                     c_acf = as.numeric(),
                     n_c_obs = as.integer(),
                     n_q_obs = as.integer(),
                     n_paired_cq_obs = as.integer())

for(nwk in domains){

  writeLines(paste("\n\n starting annual flux calculations for domain:", nwk, "\n\n"))

  ts_path <- here('data', 'macrosheds', glue('timeseries_{nwk}.csv'))
  domain_data <- read_csv(ts_path, show_col_types = FALSE)

  # get discharge data for this domain (rename date -> datetime for flux methods compatibility)
  domain_q <- domain_data %>%
      filter(var_category == 'discharge') %>%
      rename(datetime = date)

  # get chemistry data for this domain
  domain_chem <- domain_data %>%
      filter(var_category == 'stream_chemistry') %>%
      rename(datetime = date)

  rm(domain_data)
  gc()

  domain_sites <- unique(domain_chem$site_code)

  for(sc in domain_sites){

    area <- site_info %>%
        filter(site_code == sc) %>%
        distinct() %>%
        pull(ws_area_ha)

    lat <- site_info %>%
        filter(site_code == sc) %>%
        distinct() %>%
        pull(latitude)

    long <- site_info %>%
        filter(site_code == sc) %>%
        distinct() %>%
        pull(longitude)

    if(length(area) == 0 || length(lat) == 0) {
        writeLines(glue("  skipping {sc}: no site metadata"))
        next
    }

    raw_data_con_in <- domain_chem %>%
        filter(site_code == sc,
               ms_interp == 0,
               var %in% flux_vars)

    raw_data_q <- domain_q %>%
        filter(site_code == sc)

    solutes <- unique(raw_data_con_in$var)

    if(length(solutes) == 0) {
      writeLines(glue("  no flux-convertible solutes for {sc}, skipping"))
      next
    }

    writeLines(paste("FLUX CALCS:", sc))

    for(j in 1:length(solutes)){
      writeLines(paste("  site:", sc, "var:", solutes[j]))

      target_solute <- solutes[j]

      raw_data_con <- domain_chem %>%
          filter(site_code == sc,
                 ms_interp == 0,
                 val > 0,
                 var == target_solute) %>%
          select(datetime, val) %>%
          na.omit()

      if(nrow(raw_data_con) == 0) next

      # find acceptable years
      q_check <- raw_data_q %>%
          mutate(date = date(datetime)) %>%
          filter(ms_interp == 0, !is.na(val)) %>%
          distinct(date, .keep_all = TRUE) %>%
          mutate(water_year = water_year(datetime, origin = "usgs")) %>%
          group_by(water_year) %>%
          summarise(n = n()) %>%
          filter(n >= 311)

      conc_check <- raw_data_con %>%
          mutate(date = date(datetime)) %>%
          filter(!is.na(val)) %>%
          distinct(date, .keep_all = TRUE) %>%
          mutate(water_year = water_year(date, origin = "usgs"),
                 quart = quarter(date)) %>%
          group_by(water_year) %>%
          summarise(count = n_distinct(quart),
                    n = n()) %>%
          filter(n >= 4,
                 count > 3)

      q_good_years <- q_check$water_year
      conc_good_years <- conc_check$water_year
      good_years <- q_good_years[q_good_years %in% conc_good_years]

      if(nrow(conc_check) < 1) {
        writeLines(glue("  {sc} concentration data insufficient, skipping"))
        next
      } else if(nrow(q_check) < 1) {
        writeLines(glue("  {sc} discharge data insufficient, skipping"))
        next
      } else if(length(good_years) == 0) {
        writeLines(glue("  no overlapping good years for {sc}, skipping"))
        next
      }

      daily_data_con <- raw_data_con %>%
          mutate(date = date(datetime)) %>%
          group_by(date) %>%
          summarize(val = mean_or_x(val)) %>%
          mutate(site_code = sc, var = 'con') %>%
          select(site_code, datetime = date, var, val)

      daily_data_q <- raw_data_q %>%
          mutate(date = date(datetime)) %>%
          group_by(date) %>%
          summarize(val = mean_or_x(val)) %>%
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

      #### calculate WRTDS ######
      flux_annual_wrtds <- NA
      tryCatch(
        expr = {
          flux_annual_wrtds <- calculate_wrtds(
            chem_df = con_full,
            q_df = q_df,
            ws_size = area,
            lat = lat,
            long = long,
            datecol = 'datetime',
            agg = 'annual',
            minNumObs = 100,
            minNumUncen = 50
           )
        },
        error = function(e) {
          writeLines(paste('  WRTDS failed for', sc, target_solute))
        }
      )

      for(k in 1:length(good_years)){

        writeLines(paste("  site:", sc, 'year:', good_years[k]))

          target_year <- as.numeric(as.character(good_years[k]))

          flag_df <- carry_flags(raw_q_df = raw_data_q,
                                 raw_con_df = raw_data_con_in,
                                 target_year = target_year,
                                 target_solute = target_solute,
                                 period = 'annual')

          raw_data_target_year <- raw_data_full %>%
              mutate(wy = as.numeric(as.character(wy))) %>%
              filter(wy == target_year)

          q_target_year <- raw_data_target_year %>%
              select(site_code, datetime, q_lps, wy) %>%
              na.omit()

          con_target_year <- raw_data_target_year %>%
              select(site_code, datetime, con, wy) %>%
              na.omit()

          chem_df <- tryCatch(errors::drop_errors(con_target_year),
                              error = function(e) con_target_year)
          q_df_yr <- tryCatch(errors::drop_errors(q_target_year),
                              error = function(e) q_target_year)

          #### calculate average ####
          flux_annual_average <- raw_data_target_year %>%
              group_by(wy) %>%
              summarize(q_lps = mean(q_lps, na.rm = TRUE),
                        con = mean(con, na.rm = TRUE)) %>%
              mutate(flux = con*q_lps*3.154e+7*(1/area)*1e-6) %>%
              pull(flux)

          #### calculate period weighted #####
          flux_annual_pw <- calculate_pw(chem_df, q_df_yr, datecol = 'datetime')

          #### calculate beale ######
          flux_annual_beale <- calculate_beale(chem_df, q_df_yr, datecol = 'datetime')

          #### calculate rating #####
          flux_annual_rating <- calculate_rating(chem_df, q_df_yr, datecol = 'datetime')

          #### calculate composite ######
          rating_filled_df <- generate_residual_corrected_con(chem_df = chem_df,
                                                              q_df = q_df_yr,
                                                              datecol = 'datetime',
                                                              sitecol = 'site_code')

          flux_annual_comp <- calculate_composite_from_rating_filled_df(rating_filled_df)

          #### select recommended method (Aulenbach et al 2016) ####
          paired_df <- q_df_yr %>%
              full_join(chem_df, by = c('datetime', 'site_code', 'wy')) %>%
              na.omit() %>%
              filter(q_lps > 0,
                     is.finite(q_lps))

          q_log <- log10(paired_df$q_lps)
          c_log <- log10(paired_df$con)
          model_data <- tibble(c_log, q_log) %>%
              filter(is.finite(c_log),
                     is.finite(q_log)) %>%
              na.omit()

          rating_summary <- summary(lm(model_data$c_log ~ model_data$q_log, singular.ok = TRUE))
          r_squared <- rating_summary$r.squared
          resid_acf <- abs(acf(rating_summary$residuals, lag.max = 1, plot = FALSE)$acf[2])
          con_acf <- abs(acf(paired_df$con, lag.max = 1, plot = FALSE)$acf[2])

          if(!is.nan(r_squared)) {
            if(r_squared > 0.3){
                ideal_method <- ifelse(resid_acf > 0.2, 'composite', 'rating')
            } else {
                ideal_method <- ifelse(con_acf > 0.20, 'pw', 'average')
            }
          } else {
            writeLines("  ideal method error: r_squared was NaN, set to NA")
            ideal_method <- NA
          }

          #### congeal fluxes ####
          target_year_out <- tibble(wy = target_year,
                                    val = c(flux_annual_average,
                                            flux_annual_pw,
                                            flux_annual_beale,
                                            flux_annual_rating,
                                            flux_annual_comp$flux[1]),
                              site_code = sc,
                              var = target_solute,
                              method = c('average', 'pw', 'beale', 'rating', 'composite')) %>%
              mutate(ms_recommended = ifelse(method == ideal_method, 1, 0))
          out_frame <- bind_rows(out_frame, target_year_out)

          # diagnostics
          diag_row <- tibble(
              network = site_info$network[site_info$site_code == sc][1],
              domain = nwk,
              site_code = sc,
              var = target_solute,
              water_year = target_year,
              cq_rsquared = r_squared,
              cq_resid_acf = resid_acf,
              c_acf = con_acf,
              n_c_obs = nrow(con_target_year),
              n_q_obs = nrow(q_target_year),
              n_paired_cq_obs = nrow(paired_df))
          diag_frame <- bind_rows(diag_frame, diag_row)

      } # end year loop

      # add WRTDS results if available
      if(is.data.frame(flux_annual_wrtds)) {
          wrtds_out <- flux_annual_wrtds %>%
              filter(wy %in% good_years) %>%
              rename(val = flux) %>%
              mutate(site_code = sc,
                     var = solutes[j],
                     method = 'wrtds',
                     ms_recommended = 0)
          out_frame <- bind_rows(out_frame, wrtds_out)
      }

    } # end solute loop
  } # end site loop
} # end domain loop

# write combined output as CSV
out_csv <- out_frame %>%
    mutate(network = site_info$network[match(site_code, site_info$site_code)],
           domain = site_info$domain[match(site_code, site_info$site_code)]) %>%
    rename(water_year = wy, load = val) %>%
    select(network, domain, site_code, var, water_year, load, method, ms_recommended) %>%
    arrange(domain, site_code, var, water_year, method)

write_csv(out_csv, here('paper', 'macrosheds_application', 'load_annual.csv'))
write_csv(diag_frame, here('paper', 'macrosheds_application', 'load_annual_diagnostics.csv'))

cat(sprintf("\nDone. %d load estimates across %d sites written to paper/macrosheds_application/load_annual.csv\n",
            nrow(out_csv), length(unique(out_csv$site_code))))

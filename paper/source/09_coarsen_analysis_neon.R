library(tidyverse)
library(here)
library(lfstat)
library(lubridate)
library(RiverLoad)

ms_sites <- read_csv(here('data', 'macrosheds', 'sites.csv'), show_col_types = FALSE)

small_sites <- read_csv(here('data', 'neon', 'ms_stream_order.csv'),
                        show_col_types = FALSE) %>%
    left_join(ms_sites, by = 'site_code') %>%
    filter(domain == 'neon', stream_order == 1)

set.seed(53045)

source(here('source/flux_methods.R'))
source(here('paper','source','coarsen_helpers.R'))

neon_ts <- read_csv(here('data', 'macrosheds', 'timeseries_neon.csv'), show_col_types = FALSE)
neon_site_codes <- unique(small_sites$site_code)
q_dat <- neon_ts %>% filter(var_category == 'discharge', site_code %in% neon_site_codes)
c_dat <- neon_ts %>% filter(var_category == 'stream_chemistry', site_code %in% neon_site_codes)

loop_vec <- c(seq(from = 1, to = 13, by = 1),
              seq(from = 14, to = 28, by = 7),
              30, 60, 90)

for (target_site in unique(c_dat$site_code)) {
    area <- ms_sites %>% filter(site_code == target_site) %>% pull(ws_area_ha)

    for (target_solute in c('turbid_FNU')) {
        gc()
        q_data_in <- q_dat %>%
            filter(site_code == target_site) %>%
            select(date, site_code, q_lps = val)

        c_data_in <- c_dat %>%
            filter(site_code == target_site, var == target_solute, grab_sample == 0) %>%
            select(date, site_code, con = val)

        com_dat_raw <- q_data_in %>%
            full_join(c_data_in, by = c('date', 'site_code')) %>%
            mutate(wy = as.integer(as.character(water_year(date, origin = 'usgs'))))

        good_years <- com_dat_raw %>% na.omit() %>% count(wy) %>% filter(n > 364)
        if (nrow(good_years) == 0) next

        com_dat <- com_dat_raw %>% filter(wy %in% good_years$wy)

        for (target_wy in unique(com_dat$wy)) {
            com_dat_wy <- com_dat %>% filter(wy == target_wy)
            if (nrow(na.omit(com_dat_wy)) <= 300) next

            ts_df <- com_dat_wy %>% select(date, con, q_lps)

            out_tbl <- run_coarsening_experiment(
                ts_df = ts_df, site_code = target_site, target_wy = target_wy,
                area = area, loop_vec = loop_vec, reps = 100, daily_agg = FALSE)

            if (target_solute == 'spCond') {
                save(out_tbl, file = here('data','coarsen_neon',
                    paste0('TEST100reps_annual_spCond_', target_wy, '_', target_site, '.RData')))
            }
            if (target_solute == 'turbid_FNU') {
                save(out_tbl, file = here('data','coarsen_neon',
                    paste0('TEST100reps_annual_turb', target_wy, '_', target_site, '.RData')))
            }
        }
    }
}

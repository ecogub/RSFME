library(tidyverse)
library(feather)
library(here)
library(lfstat)
library(lubridate)
library(RiverLoad)

set.seed(53045)

source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('paper','source','coarsen_helpers.R'))

area <- HBEF_AREA
site_code <- HBEF_SITE_CODE

loop_vec <- c(seq(from = 1, to = 92, by = 4),
              seq(from = 96, to = 672, by = 96),
              3592, 6512)

d <- read_feather(here('w3_sensor_wdisch.feather')) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))

target_wy <- HBEF_TARGET_WY

for (solute_var in HBEF_SOLUTES) {
    dn <- d %>%
        filter(wy == target_wy) %>%
        select(date, con = all_of(solute_var), q_lps = IS_discharge)

    if (solute_var == 'IS_spCond') dn$con <- dn$con * CA_SPCOND_SLOPE + CA_SPCOND_INTERCEPT

    out_tbl <- run_coarsening_experiment(
        ts_df = dn, site_code = site_code, target_wy = target_wy,
        area = area, loop_vec = loop_vec, reps = COARSEN_REPS, daily_agg = TRUE)

    if (solute_var == 'IS_spCond') {
        save(out_tbl, file = here('data','coarsen_hbef', '100reps_annual_Ca.RData'))
    } else {
        save(out_tbl, file = here('data','coarsen_hbef', '100reps_annual_NO3.RData'))
    }
}

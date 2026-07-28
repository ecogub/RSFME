library(tidyverse)
library(here)
library(lfstat)
library(lubridate)
library(RiverLoad)

set.seed(53045)

source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('paper','source','coarsen_helpers.R'))

area <- PLYN_AREA
site_code <- PLYN_SITE_CODE

loop_vec <- c(seq(from = 1, to = 3, by = 1),
              seq(from = 4, to = 48, by = 3),
              96, 192)

d <- read_csv(here('data','plynlimon','PlynlimonHighFrequencyHydrochemistry.csv'),
              show_col_types = FALSE) %>%
    filter(Site == site_code) %>%
    select(date_time, `NO3-N mg/l`, `Ca mg/l`, `water flux mm/hr`) %>%
    mutate(wy = water_year(date_time, origin = 'usgs'),
           q_lps = `water flux mm/hr` * area * (1000/1) * (1/10000) * (1/3600) * (1000/1))

target_wy <- PLYN_TARGET_WY

for (solute_var in c('NO3-N mg/l', 'Ca mg/l')) {
    dn <- d %>%
        filter(wy == target_wy) %>%
        select(date = date_time, con = all_of(solute_var), q_lps)

    out_tbl <- run_coarsening_experiment(
        ts_df = dn, site_code = site_code, target_wy = target_wy,
        area = area, loop_vec = loop_vec, reps = COARSEN_REPS, daily_agg = TRUE)

    if (solute_var == 'Ca mg/l') {
        write_csv(out_tbl, file = here('data','coarsen_plynlimon', '100reps_annual_Ca.csv'))
    } else {
        write_csv(out_tbl, file = here('data','coarsen_plynlimon', '100reps_annual_NO3.csv'))
    }
}

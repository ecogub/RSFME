library(tidyverse)
library(here)
library(lfstat)
library(lubridate)
library(RiverLoad)

set.seed(53045)

source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))

# read in shared data ####
area <- PLYN_AREA
site_code <- PLYN_SITE_CODE
target_wy <- PLYN_TARGET_WY

d <- read_csv(here('data','plynlimon','PlynlimonHighFrequencyHydrochemistry.csv')) %>%
    filter(Site == site_code) %>%
    select(date_time, `NO3-N mg/l`, `Ca mg/l`, `water flux mm/hr`) %>%
    mutate(wy = water_year(date_time, origin = 'usgs'),
           q_lps = `water flux mm/hr`*area*(1/1000)*(10000/1)*(1/3600)*(1000/1))



# create calcium figure #####
# set watershed attributes #####
target_solute = 'Ca mg/l'

dn <- d %>%
    filter(wy == target_wy) %>%
    select(date = date_time, con = all_of(target_solute), q_lps)

out_tbl <- read_csv(file = here('data','coarsen_plynlimon', '100reps_annual_Ca.csv'))

## calculate 'truth' ####
chem_df <- dn %>%
    group_by(lubridate::yday(date)) %>%
    summarize(date = date(date),
              con = mean(con)) %>%
    ungroup() %>%
    unique() %>%
    select(date, con) %>%
    mutate(site_code = site_code, wy = target_wy)

q_df <- dn %>%
    select(date, q_lps)%>%
    group_by(lubridate::yday(date)) %>%
    summarize(date = date(date),
              q_lps = mean(q_lps)) %>%
    ungroup() %>%
    unique() %>%
    mutate(site_code = site_code, wy = target_wy)

out_val <- generate_residual_corrected_con(chem_df = chem_df, q_df = q_df, sitecol = 'site_code') %>%
    rename(datetime = date) %>%
    calculate_composite_from_rating_filled_df() %>%
    pull(flux)
truth <- tibble(method = 'truth', estimate = out_val)

## calculate error from truth #####
plot_tbl <- out_tbl %>%
    unique() %>%
    mutate(error = ((estimate-truth$estimate[1])/truth$estimate[1])*100,
           error_abs = abs(error),
           method = factor(method, levels = c('pw', 'beale', 'rating', 'composite')),
           percent_coverage = (nrow(dn)/n)/nrow(dn),
           hours = n*7)

## set breaks #####
breaks <- c(24,96,192,384,768)
x_labels <- c('Daily', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

## generate Ca plot ####
plot_tbl %>%
    group_by(method, hours) %>%
    mutate(min = min(error), max = max(error), median = median(error)) %>%
    filter(hours <= 899) %>%
    ggplot(., aes(x = hours, y = median))+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = error_band_colors['band_5pct'], alpha = .15)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -20, ymax = -5, fill = error_band_colors['band_20pct'], alpha = .15)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = 5, ymax = 20, fill = error_band_colors['band_20pct'], alpha = .15)+
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .25)+
    geom_line(linewidth = 1.5)+
    geom_line(aes(y = max), linewidth = .75)+
    geom_line(aes(y = min), linewidth = .75)+
    facet_wrap(vars(method), ncol = 2, labeller = as_labeller(method_labels))+
    labs(x = 'Frequency', y = 'Error (%)', title = '(a) Calcium Load Accuracy')+
    theme_rsfme()+
    scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_cartesian(ylim = c(-25,25))
ggsave_hess(filename = here('paper','figures', 'fig09_plynlimon_ca_coarsening.png'))

# create nitrate figure #####
target_solute = 'NO3-N mg/l'

## subset data #####
dn <- d %>%
    filter(wy == target_wy) %>%
    select(date = date_time, con = all_of(target_solute), q_lps)

out_tbl <- read_csv(file = here('data','coarsen_plynlimon', '100reps_annual_NO3.csv'))

## calculate 'truth' ####
chem_df <- dn %>%
    group_by(lubridate::yday(date)) %>%
    summarize(date = date(date),
              con = mean(con)) %>%
    ungroup() %>%
    unique() %>%
    select(date, con) %>%
    mutate(site_code = site_code, wy = target_wy)

q_df <- dn %>%
    select(date, q_lps)%>%
    group_by(lubridate::yday(date)) %>%
    summarize(date = date(date),
              q_lps = mean(q_lps)) %>%
    ungroup() %>%
    unique() %>%
    mutate(site_code = site_code, wy = target_wy)

out_val <- generate_residual_corrected_con(chem_df = chem_df, q_df = q_df, sitecol = 'site_code') %>%
    rename(datetime = date) %>%
    calculate_composite_from_rating_filled_df() %>%
    pull(flux)
truth <- tibble(method = 'truth', estimate = out_val)

## calculate error from truth #####
plot_tbl <- out_tbl %>%
    unique() %>%
    mutate(error = ((estimate-truth$estimate[1])/truth$estimate[1])*100,
           error_abs = abs(error),
           method = factor(method, levels = c('pw', 'beale', 'rating', 'composite')),
           percent_coverage = (nrow(dn)/n)/nrow(dn),
           hours = n*7)

## generate NO3 plot ####
plot_tbl %>%
    group_by(method, hours) %>%
    mutate(min = min(error), max = max(error), median = median(error)) %>%
    filter(hours <= 899) %>%
    ggplot(., aes(x = hours, y = median))+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = error_band_colors['band_5pct'], alpha = .15)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -20, ymax = -5, fill = error_band_colors['band_20pct'], alpha = .15)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = 5, ymax = 20, fill = error_band_colors['band_20pct'], alpha = .15)+
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .25)+
    geom_line(linewidth = 1.5)+
    geom_line(aes(y = max), linewidth = .75)+
    geom_line(aes(y = min), linewidth = .75)+
    facet_wrap(vars(method), ncol = 2, labeller = as_labeller(method_labels))+
    labs(x = 'Frequency', y = 'Error (%)', title = '(b) Nitrate Load Accuracy')+
    theme_rsfme()+
    scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    coord_cartesian(ylim = c(-25,25))
ggsave_hess(filename = here('paper','figures', 'fig10_plynlimon_no3_coarsening.png'))
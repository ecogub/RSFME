library(tidyverse)
library(feather)
library(here)
library(lfstat)
library(lubridate)
library(RiverLoad)

set.seed(53045)

source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))

# create calcium figure #####
## set watershed attributes ####
area <- HBEF_AREA
site_code <- HBEF_SITE_CODE
target_solute = 'IS_spCond'


## read in and prep data ####
d <- read_feather(here('w3_sensor_wdisch.feather')) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))

#### subset to 2016 wy ####
target_wy <- HBEF_TARGET_WY
dn <- d %>%
    filter(wy == target_wy) %>%
    select(date, all_of(target_solute), IS_discharge)
colnames(dn)[2] <- 'con'
#### convert from specific conductivity to calcium ####
dn$con <- dn$con * CA_SPCOND_SLOPE + CA_SPCOND_INTERCEPT

load(file = here('data','coarsen_hbef', '100reps_annual_Ca.RData'))

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
    select(date, q_lps = IS_discharge)%>%
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
           hours = n/4)

## set breaks #####
breaks <- c(1,24,96,192,384,768)
x_labels <- c('Hourly', 'Daily', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')
## generate plot with legend ####
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
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.spacing.y = unit(1.2, 'lines'))+
    coord_cartesian(ylim = c(-25, 25))
ggsave_hess(filename = here('paper','figures', 'fig07_hbef_ca_coarsening.png'))

# create nitrate figure #####
## set watershed attributes ####
area <- HBEF_AREA
site_code <- HBEF_SITE_CODE
target_solute = 'IS_NO3'

## read in and prep data ####
load(file = here('data','coarsen_hbef', '100reps_annual_NO3.RData'))
#### subset to 2016 wy ####
target_wy <- HBEF_TARGET_WY
dn <- d %>%
    filter(wy == target_wy) %>%
    select(date, all_of(target_solute), IS_discharge)
colnames(dn)[2] <- 'con'

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
    select(date, q_lps = IS_discharge)%>%
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
           hours = n/4)

## generate nitrate plot ####
breaks <- c(1,24,96,192,384,768)
x_labels <- c('Hourly', 'Daily', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

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
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          panel.spacing.y = unit(1.2, 'lines'))+
    coord_cartesian(ylim = c(-50, 50))

ggsave_hess(filename = here('paper','figures', 'fig08_hbef_no3_coarsening.png'))


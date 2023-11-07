library(tidyverse)
library(forecast)
library(feather)
library(xts)
library(imputeTS)
library(here)
library(lfstat)
library(lubridate)
library(ggpubr)
library(patchwork)

set.seed(53045)


source(here('source/flux_methods.R'))
source(here('paper/hbef_corr_exploration/Ca_correlation_investigation.R'))

area <- 42.4
site_code = 'w3'

# read in HBEF data ####
d <- read_feather('C:/Users/gubbi/desktop/w3_sensor_wdisch.feather') %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))
#slice(1:ts_len)

# subset to 2016 wy
target_wy <- 2016

# clean HBEF data and convert conductivity to Ca ####
dn <- d %>%
    filter(wy == target_wy) %>%
    mutate(IS_discharge = na.approx(IS_discharge),
           IS_NO3 = na.approx(IS_NO3),
           IS_spCond = na.approx(IS_spCond)*0.06284158,
           season = 'Summer') %>%
    select(datetime, IS_spCond, IS_NO3, IS_discharge, season)
dn$season[month(dn$datetime) %in% c(12,1,2)] <- 'Winter'
dn$season[month(dn$datetime) %in% c(3,4,5)] <- 'Spring'
dn$season[month(dn$datetime) %in% c(9,10,11)] <- 'Fall'

# generate supplemental HBEF figures #####
##  HBEF Calcium C:Q relationship ####
q_breaks = c(1e-2, 1, 1e2)
q_labels = c('.01', '1', '100')

dn %>%
    ggplot(aes(x = IS_discharge, y = IS_spCond))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_classic() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Calcium at HBEF',
         color = 'Season')+
    theme(text = element_text(size = 20)) +
    scale_color_manual(values = c('#882255','#117733','#DDCC77', '#332288'))

ggsave(filename = here('paper','misc_figure_creation', 'ca_cq.png'), width = 6, height = 6)

##  HBEF nitrate C:Q relationship ####
dn %>%
    ggplot(aes(x = IS_discharge, y = IS_NO3))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_classic() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Nitrate-N at HBEF')+
    theme(text = element_text(size = 20),
          legend.position = 'none')+
    scale_color_manual(values = c('#882255','#117733','#DDCC77', '#332288'))
ggsave(filename = here('paper','misc_figure_creation', 'nitrate_cq.png'), width = 6, height = 6)

## HBEF chemistry time series ####
chem_ts <- dn %>%
    rename(`Nitrate (as N)` = IS_NO3,
           Calcium = IS_spCond) %>%
    pivot_longer(cols = c(`Nitrate (as N)`, Calcium),
                 names_to = 'var',
                 values_to = 'val') %>%
    ggplot(aes(x = datetime, y = val)) +
    geom_line()+
    facet_wrap(~var, ncol = 1, scales = 'free')+
    theme_classic()+
    theme(text = element_text(size = 20))+
    labs(x = '',
         y = 'C (mg/L)',
         title = 'HBEF Watershed 3 - 2016 Water Year')

chem_ts + inset_element(ca_sc_plot + theme(text = element_text(size = 10)), 0.1, 0.75, 0.3, .99, align_to = 'plot')

ggsave(filename = here('paper','misc_figure_creation', 'rawchem.png'), width = 12, height = 6)

## HBEF Streamflow timeseries ####
dn %>%
    ggplot(aes(x = datetime, y = IS_discharge, color = flag)) +
    geom_line()+
    theme_classic()+
    theme(text = element_text(size = 20),
          legend.position = 'none')+
    scale_y_log10(breaks = q_breaks,
                  labels = q_labels)+
    scale_color_manual(values = c('black', 'red'))+
    labs(x = '',
         y = 'Q (Lps)',
         title = 'Streamflow at HBEF Watershed 3 - 2016 Water Year')


ggsave(filename = here('paper','misc_figure_creation', 'rawQ.png'), width = 12, height = 6)

# read in Plynlimon data ####
area <- 122
site_code <- 'UHF'
target_wy <- 2008

d <- read_csv(here('paper/plynlimon_discussion/PlynlimonHighFrequencyHydrochemistry.csv')) %>%
    filter(Site == site_code) %>%
    mutate(datetime = ymd_hm(date_time)) %>%
    select(datetime,
           `Nitrate (as N)` = `NO3-N mg/l`,
           Calcium = `Ca mg/l`, `water flux mm/hr`) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'),
           q_lps = `water flux mm/hr`*area*(1000/1)*(1/10000)*(1/3600)*(1000/1)) %>%
    filter(wy == target_wy) %>%
    mutate(season = 'Summer') %>%
    select(datetime, Calcium, `Nitrate (as N)`, q_lps, season)
d$season[month(d$datetime) %in% c(12,1,2)] <- 'Winter'
d$season[month(d$datetime) %in% c(3,4,5)] <- 'Spring'
d$season[month(d$datetime) %in% c(9,10,11)] <- 'Fall'

##  PLY calcium C:Q relationship ####

q_breaks = c(1e-2, 1, 1e2)
q_labels = c('.01', '1', '100')

d %>%
    ggplot(aes(x = q_lps, y = Calcium))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_classic() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Calcium at Plynlimon',
         color = 'Season')+
    theme(text = element_text(size = 20)) +
    scale_color_manual(values = c('#882255','#117733','#DDCC77', '#332288'))

ggsave(filename = here('paper','misc_figure_creation', 'ca_cq_ply.png'), width = 6, height = 6)

##  PLY nitrate C:Q relationship ####
d %>%
    ggplot(aes(x = q_lps, y = `Nitrate (as N)`))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_classic() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Nitrate-N at Plynlimon')+
    theme(text = element_text(size = 20),
          legend.position = 'none')+
    scale_color_manual(values = c('#882255','#117733','#DDCC77', '#332288'))
ggsave(filename = here('paper','misc_figure_creation', 'nitrate_cq_ply.png'), width = 6, height = 6)

## PLY chemistry time series ####
d %>%
    pivot_longer(cols = c(`Nitrate (as N)`, Calcium),
                 names_to = 'var',
                 values_to = 'val') %>%
    ggplot(aes(x = as_datetime(datetime), y = val)) +
    geom_line()+
    facet_wrap(~var, ncol = 1, scales = 'free')+
    theme_classic()+
    theme(text = element_text(size = 20))+
    labs(x = '',
         y = 'C (mg/L)',
         title = 'Plynlimon Upper Hafren - 2008 Water Year')

ggsave(filename = here('paper','misc_figure_creation', 'rawchem_ply.png'), width = 12, height = 6)

## PLY Streamflow timeseries ####
d %>%
    ggplot(aes(x = as_datetime(datetime), y = q_lps)) +
    geom_line()+
    theme_classic()+
    theme(text = element_text(size = 20),
          legend.position = 'none')+
    scale_y_log10(#breaks = q_breaks,
                  #labels = q_labels
        )+
    scale_color_manual(values = c('black', 'red'))+
    labs(x = '',
         y = 'Q (Lps)',
         title = 'Streamflow at Plynlimon Upper Hafren - 2008 Water Year')


ggsave(filename = here('paper','misc_figure_creation', 'rawQ_ply.png'), width = 12, height = 6)


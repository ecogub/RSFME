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
library(RiverLoad)
library(cowplot)
library(zoo)

set.seed(53045)

source(here('source/flux_methods.R'))
# read in data ######
d <- read_feather('C:/Users/gubbi/desktop/w3_sensor_wdisch.feather') %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))

## Subset to 2016 wy
target_wy <- 2016
dn <- d %>%
    filter(wy == target_wy)
## read in output from 1_ts_simulation_analysis.R####
weekly <- read_csv(here('paper','ts_simulation', 'weeklyFreq_100Reps20221221.csv')) %>%
    mutate(freq = 'Weekly')
biweekly <- read_csv(here('paper','ts_simulation', 'biweeklyFreq_100Reps20221221.csv')) %>%
    mutate(freq = 'Biweekly')
monthly <- read_csv(here('paper','ts_simulation', 'monthlyFreq_100Reps20221221.csv')) %>%
    mutate(freq = 'Monthly')
loop_out <- rbind(weekly, biweekly, monthly) %>%
    mutate(freq = factor(freq, levels = c('Weekly', 'Biweekly', 'Monthly')))
#loop_out$cq[loop_out$cq == 'broken_dilution'] = 'dilution'


### make hydrologic regime plots #####
side_ymin <- 0.01
side_ymax <- 1000
side_breaks <- c(1e-1, 1e1, 1e3)
side_labels <- c('0.1', '10', '1,000')

tibble(date = dn$date,
            `Input Data` = dn$IS_discharge,
            Unaltered = simulated_series[[1]],
            `Stormflow Enchanced` = simulated_series[[2]],
            `Baseflow Enchanced` = simulated_series[[3]]) %>%
    pivot_longer(., cols = -date, names_to = 'Treatment', values_to = 'val') %>%
    mutate(Treatment = factor(Treatment, levels = c('Input Data', 'Unaltered', 'Stormflow Enchanced', 'Baseflow Enchanced'))) %>%
    ggplot(aes(x = date))+
    geom_line(aes(y = val)) +
    theme_classic()+
    theme(axis.title.x=element_blank(),
          text = element_text(size = 15),
          axis.text.y = element_text(size = 15))+
    labs(y = 'Q (Lps)')+
    scale_y_log10(limits = c(side_ymin,side_ymax),
                  breaks = side_breaks,
                  labels = side_labels)+
    scale_x_continuous(breaks = c(as_date('2015-10-01'), as_date('2016-04-01'), as_date('2016-10-01')),
                       labels = c('10/2015', '4/2016', '10/2016')) +
    facet_wrap(~Treatment, ncol = 1)

ggsave(filename = here('paper','ts_simulation', 'hydro_regime.png'), width = 6, height = 6)

### make cq plots ####
top_row_breaks <- c(1e-2, 1, 1e2)
top_row_labels <- c('0.01', '1', '100')
top_row_ymax <- 1e2
top_row_ymin <- 1e-2

tibble(date = dn$date,
       Q = simulated_series[[1]],
       Chemostatic = simulated_series[[4]],
       `No Pattern` = simulated_series[[5]],
       Enriching = simulated_series[[6]],
       Diluting = simulated_series[[7]]) %>%
    pivot_longer(., cols = -c(date, Q), names_to = 'cq', values_to = 'val') %>%
    mutate(cq = factor(cq, levels = c('Chemostatic', 'No Pattern', 'Enriching', 'Diluting'))) %>%
    ggplot(aes(x = Q))+
    geom_point(aes(y = val)) +
    theme_classic()+
    theme(text = element_text(size = 15),
          axis.text.y = element_text(size = 15))+
    labs(x = 'Q (Lps)',
         y = 'C (mg/L)')+
    scale_x_log10(breaks = side_breaks,
                  labels = side_labels) +
    scale_y_log10(limits = c(top_row_ymin, top_row_ymax),
                  breaks = top_row_breaks,
                  labels = top_row_labels)+
    facet_wrap(~cq, ncol = 2)

ggsave(filename = here('paper','ts_simulation', 'cq_regime.png'), width = 6, height = 6)

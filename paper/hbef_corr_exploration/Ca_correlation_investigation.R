## based on Audrey's MSdata_formatconvert_phase2.R
library(here)
library(tidyverse)
library(feather)
library(data.table)
library(lubridate)
library(gridExtra)
library(grid)
library(lfstat)
library(ggthemes)

# read in chemistry data ####
simple_chem_and_Q <- read_csv(here("paper","hbef_corr_exploration", "HBEFdata_All_2022-11-17.csv")) %>%
    mutate(wy = water_year(date, origin = 'usgs')) %>%
    filter(site == 'W3',
           wy == 2016)

# run fit Ca and spCond ####
complete_ds <- simple_chem_and_Q %>%
    select(datetime, spCond, Ca) %>%
    na.omit(spCond)

summary(lm(Ca~spCond+0, data = complete_ds))


complete_ds %>%
    pivot_longer(cols = -c(datetime, spCond), names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = spCond, y = val)) +
    geom_point() +
    facet_wrap(~var)

# extract fit ####
fit <- summary(lm(Ca~spCond+0, data = complete_ds))
fit$coefficients[[1]]

## Ca in mg/L = spCond*0.06284158

ca_sc_plot <- ggplot(simple_chem_and_Q, aes(x = spCond, y = Ca))+
                         geom_point()+
                         geom_smooth(method = 'lm')+
                         theme_few()+
                         labs(y = 'Ca (mg/L)',
                              x = 'SC (uS/cm)')+
                        theme(text = element_text(size = 20),
                              plot.title = element_text(size = 30))

ca_sc_plot

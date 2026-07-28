## based on Audrey's MSdata_formatconvert_phase2.R
library(here)
library(tidyverse)
library(lfstat)
source(here('source/config.R'))
source(here('source/plot_theme.R'))

# read in chemistry data ####
simple_chem_and_Q <- read_csv(here('data','hbef','HBEFdata_All_2022-11-17.csv')) %>%
    mutate(wy = water_year(date, origin = 'usgs')) %>%
    filter(site == 'W3',
           wy == HBEF_TARGET_WY)

# run fit Ca and spCond ####
complete_ds <- simple_chem_and_Q %>%
    select(datetime, spCond, Ca) %>%
    na.omit(spCond)

summary(lm(Ca~spCond, data = complete_ds))


complete_ds %>%
    pivot_longer(cols = -c(datetime, spCond), names_to = 'var', values_to = 'val') %>%
    ggplot(aes(x = spCond, y = val)) +
    geom_point() +
    facet_wrap(~var)

# extract fit ####
fit <- lm(Ca~spCond, data = complete_ds)
cat('Intercept:', coef(fit)[1], '\nSlope:', coef(fit)[2], '\n')

ca_sc_plot <- ggplot(simple_chem_and_Q, aes(x = spCond, y = Ca))+
                         geom_point()+
                         geom_smooth(method = 'lm')+
                         theme_rsfme()+
                         labs(y = 'Ca (mg/L)',
                              x = 'SC (uS/cm)')

ca_sc_plot

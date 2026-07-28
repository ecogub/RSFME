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

fit <- lm(Ca~spCond, data = complete_ds)
print(summary(fit))
cat('Intercept:', coef(fit)[1], '\nSlope:', coef(fit)[2], '\n')

# inset panel used inside the HBEF chemistry time series (paper Figure 2);
# base_size is small because it renders at roughly a third of panel width
ca_sc_plot <- ggplot(simple_chem_and_Q, aes(x = spCond, y = Ca))+
                         geom_point(size = 0.8)+
                         geom_smooth(method = 'lm', linewidth = 0.6)+
                         theme_rsfme(base_size = 6)+
                         scale_x_continuous(n.breaks = 4)+
                         scale_y_continuous(n.breaks = 3)+
                         labs(y = 'Ca (mg/L)',
                              x = 'SC (uS/cm)')

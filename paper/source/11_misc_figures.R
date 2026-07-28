library(tidyverse)
library(feather)
library(here)
library(lfstat)
library(lubridate)
library(patchwork)
library(zoo)

set.seed(53045)


source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))
source(here('paper','source','10_ca_correlation.R'))

area <- HBEF_AREA
site_code <- HBEF_SITE_CODE

# Figures 2 and 4 stack chemistry over discharge. Giving every panel the same
# x scale is what makes their date axes line up, rather than leaving alignment
# to coincide from the data ranges.
water_year_x_scale <- function(datetimes) {
    scale_x_datetime(limits = range(datetimes),
                     date_breaks = '3 months',
                     date_labels = '%b %Y',
                     expand = expansion(mult = 0.02))
}

# read in HBEF data ####
d <- read_feather(here('w3_sensor_wdisch.feather')) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))
#slice(1:ts_len)

# subset to 2016 wy
target_wy <- HBEF_TARGET_WY

# clean HBEF data and convert conductivity to Ca ####
dn <- d %>%
    filter(wy == target_wy) %>%
    mutate(IS_discharge = na.approx(IS_discharge),
           IS_NO3 = na.approx(IS_NO3),
           IS_spCond = na.approx(IS_spCond) * CA_SPCOND_SLOPE + CA_SPCOND_INTERCEPT,
           season = 'Summer') %>%
    select(datetime, IS_spCond, IS_NO3, IS_discharge, season)
dn$season[month(dn$datetime) %in% c(12,1,2)] <- 'Winter'
dn$season[month(dn$datetime) %in% c(3,4,5)] <- 'Spring'
dn$season[month(dn$datetime) %in% c(9,10,11)] <- 'Fall'

# generate HBEF figures #####
q_breaks = c(1e-2, 1, 1e2)
q_labels = c('.01', '1', '100')

##  HBEF Calcium C:Q relationship (Figure 3, right panel) ####
p_hbef_ca_cq <- dn %>%
    ggplot(aes(x = IS_discharge, y = IS_spCond))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_rsfme() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Calcium at HBEF',
         color = 'Season')+
    scale_color_manual(values = season_colors)

##  HBEF nitrate C:Q relationship (Figure 3, left panel) ####
p_hbef_no3_cq <- dn %>%
    ggplot(aes(x = IS_discharge, y = IS_NO3))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10(breaks = q_breaks,
                  labels = q_labels) +
    theme_rsfme() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Nitrate-N at HBEF')+
    theme(legend.position = 'none')+
    scale_color_manual(values = season_colors)

## Figure 3: NO3 and Ca C:Q side by side ####
ggsave_hess(filename = here('paper','figures', 'fig03_hbef_cq.png'),
            plot = p_hbef_no3_cq + p_hbef_ca_cq,
            width = HESS_DOUBLE_COL_CM,
            height = HESS_DOUBLE_COL_CM / 2)

## HBEF chemistry time series (Figure 2, upper panels) ####
hbef_chem_long <- dn %>%
    rename(`Nitrate (as N)` = IS_NO3,
           Calcium = IS_spCond) %>%
    pivot_longer(cols = c(`Nitrate (as N)`, Calcium),
                 names_to = 'var',
                 values_to = 'val')

# The Ca~SpCond inset sits over the empty upper-left of the Calcium panel: it
# spans the first third of the year and clears the highest Ca value in that
# window, so it can never cover data. Setting the layer's data to a single row
# carrying the facet variable confines the annotation to the Calcium panel.
#
# patchwork::inset_element looks like the obvious tool here and is not usable:
# it anchors to the *last* patch in a composition, so at the top level it lands
# on the discharge panel, and attaching it to the chemistry plot first nests a
# patchwork inside a patchwork, which loses the x-axis alignment between the
# chemistry and discharge panels that this figure depends on.
ca_max <- max(hbef_chem_long$val[hbef_chem_long$var == 'Calcium'])
x0 <- min(dn$datetime)
x_span <- as.numeric(difftime(max(dn$datetime), x0, units = 'secs'))
inset_x <- c(x0 + 0.02 * x_span, x0 + 0.31 * x_span)
inset_floor <- max(dn$IS_spCond[dn$datetime <= inset_x[2]]) * 1.06

ca_headroom <- tibble(var = 'Calcium',
                      datetime = x0,
                      val = ca_max * 1.05)

inset_layer <- annotation_custom(
    ggplotGrob(ca_sc_plot),
    xmin = inset_x[1],
    xmax = inset_x[2],
    ymin = inset_floor,
    ymax = ca_max * 1.02
)
inset_layer$data <- tibble(var = 'Calcium')

hbef_x_scale <- water_year_x_scale(dn$datetime)

p_hbef_chem <- ggplot(hbef_chem_long, aes(x = datetime, y = val)) +
    geom_line()+
    geom_blank(data = ca_headroom)+
    inset_layer+
    facet_wrap(~var, ncol = 1, scales = 'free_y')+
    hbef_x_scale+
    theme_rsfme()+
    labs(x = '',
         y = 'C (mg/L)',
         title = 'HBEF Watershed 3 - 2016 Water Year')

## HBEF streamflow time series (Figure 2, lower panel) ####
p_hbef_q <- dn %>%
    ggplot(aes(x = datetime, y = IS_discharge)) +
    geom_line()+
    theme_rsfme()+
    theme(legend.position = 'none')+
    scale_y_log10(breaks = q_breaks,
                  labels = q_labels)+
    hbef_x_scale+
    labs(x = '',
         y = 'Q (Lps)')

## Figure 2: chemistry over streamflow ####
ggsave_hess(filename = here('paper','figures', 'fig02_hbef_data.png'),
            plot = p_hbef_chem / p_hbef_q + plot_layout(heights = c(2, 1)),
            width = HESS_DOUBLE_COL_CM,
            height = HESS_DOUBLE_COL_CM)

# read in Plynlimon data ####
area <- PLYN_AREA
site_code <- PLYN_SITE_CODE
target_wy <- PLYN_TARGET_WY

d <- read_csv(here('data','plynlimon','PlynlimonHighFrequencyHydrochemistry.csv')) %>%
    filter(Site == site_code) %>%
    mutate(datetime = ymd_hm(date_time)) %>%
    select(datetime,
           `Nitrate (as N)` = `NO3-N mg/l`,
           Calcium = `Ca mg/l`, `water flux mm/hr`) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'),
           q_lps = `water flux mm/hr`*area*(1/1000)*(10000/1)*(1/3600)*(1000/1)) %>%
    filter(wy == target_wy) %>%
    mutate(season = 'Summer') %>%
    select(datetime, Calcium, `Nitrate (as N)`, q_lps, season)
d$season[month(d$datetime) %in% c(12,1,2)] <- 'Winter'
d$season[month(d$datetime) %in% c(3,4,5)] <- 'Spring'
d$season[month(d$datetime) %in% c(9,10,11)] <- 'Fall'

# Upper Hafren flows run 10-2000 Lps, so let the log scale pick its own breaks
##  PLY calcium C:Q relationship (Figure 5, right panel) ####
p_ply_ca_cq <- d %>%
    ggplot(aes(x = q_lps, y = Calcium))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10() +
    theme_rsfme() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Calcium at Plynlimon',
         color = 'Season')+
    scale_color_manual(values = season_colors)

##  PLY nitrate C:Q relationship (Figure 5, left panel) ####
p_ply_no3_cq <- d %>%
    ggplot(aes(x = q_lps, y = `Nitrate (as N)`))+
    geom_point(aes(color = season))+
    scale_y_log10()+
    scale_x_log10() +
    theme_rsfme() +
    geom_smooth(method = 'lm', color = 'black')+
    labs(x = 'Q (lps)',
         y = 'C (mg/L)',
         title = 'Nitrate-N at Plynlimon')+
    theme(legend.position = 'none')+
    scale_color_manual(values = season_colors)

## Figure 5: NO3 and Ca C:Q side by side ####
ggsave_hess(filename = here('paper','figures', 'fig05_plynlimon_cq.png'),
            plot = p_ply_no3_cq + p_ply_ca_cq,
            width = HESS_DOUBLE_COL_CM,
            height = HESS_DOUBLE_COL_CM / 2)

ply_x_scale <- water_year_x_scale(d$datetime)

## PLY chemistry time series (Figure 4, upper panels) ####
p_ply_chem <- d %>%
    pivot_longer(cols = c(`Nitrate (as N)`, Calcium),
                 names_to = 'var',
                 values_to = 'val') %>%
    ggplot(aes(x = datetime, y = val)) +
    geom_line()+
    facet_wrap(~var, ncol = 1, scales = 'free_y')+
    ply_x_scale+
    theme_rsfme()+
    labs(x = '',
         y = 'C (mg/L)',
         title = 'Plynlimon Upper Hafren - 2008 Water Year')

## PLY streamflow time series (Figure 4, lower panel) ####
p_ply_q <- d %>%
    ggplot(aes(x = datetime, y = q_lps)) +
    geom_line()+
    theme_rsfme()+
    theme(legend.position = 'none')+
    scale_y_log10()+
    ply_x_scale+
    labs(x = '',
         y = 'Q (Lps)')

## Figure 4: chemistry over streamflow ####
ggsave_hess(filename = here('paper','figures', 'fig04_plynlimon_data.png'),
            plot = p_ply_chem / p_ply_q +
                plot_layout(heights = c(2, 1)),
            width = HESS_DOUBLE_COL_CM,
            height = HESS_DOUBLE_COL_CM / 1.2)


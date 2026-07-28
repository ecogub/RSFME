library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(lfstat)
library(lubridate)
library(here)
library(RiverLoad)
library(patchwork)

source(here('source/config.R'))
source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))
source(here('paper','source','calculate_truth_ts.R'))

area <- HBEF_AREA
site_code <- HBEF_SITE_CODE

# HBEF Flux Method Comparison
hbef_loads <- read_csv(here('data', 'load_annual.csv'),
                       show_col_types = FALSE) %>%
    filter(domain == 'hbef', site_code == 'w3')

w3_flux <- hbef_loads %>%
    select(-ms_recommended) %>%
    distinct(water_year, site_code, method, var, .keep_all = TRUE) %>%
    pivot_wider(names_from = var, values_from = load, id_cols = c('water_year', 'site_code', 'method')) %>%
    filter(!is.na(Ca),
           site_code == 'w3') %>%
    rename(wy = water_year) %>%
    mutate(wy = as.character(wy))

w3_recc <- hbef_loads %>%
    filter(ms_recommended == 1,
           var == 'Ca') %>%
    select(-ms_recommended) %>%
    distinct(water_year, site_code, method, var, .keep_all = TRUE) %>%
    pivot_wider(names_from = var, values_from = load, id_cols = c('water_year', 'site_code', 'method')) %>%
    filter(!is.na(Ca), site_code == 'w3') %>%
    mutate(method = 'recommended') %>%
    rename(wy = water_year) %>%
    mutate(wy = as.character(wy)) %>%
    select(wy, site_code, method, Ca)

# flux methods sf
w3_flux_methods <- w3_flux %>%
    select(wy, method, Ca)

# bring in 'true' flux Ca from sensor
w3_flux_true <- read_feather(here('w3_sensor_wdisch.feather')) %>%
    mutate(wy = water_year(date, origin = 'usgs')) %>%
    group_by(wy) %>%
    summarise(Ca = sum(IS_spCond * CA_SPCOND_SLOPE + CA_SPCOND_INTERCEPT, na.rm = TRUE)) %>%
    mutate(site_code = site_code,
           method = 'true') %>%
    select(-Ca, Ca)

d <- read_feather(here('w3_sensor_wdisch.feather')) %>%
    mutate(wy = water_year(datetime, origin = 'usgs'))

w3_true <- tibble(wy = as.integer(),
                  Ca = as.numeric())
for(i in unique(water_year(d$date, origin = 'usgs'))){
    target_wy = as.integer(i)

    dn <- d %>%
        filter(wy == target_wy)

    w3_chem <- dn$IS_spCond * CA_SPCOND_SLOPE + CA_SPCOND_INTERCEPT

    w3_q <- dn %>%
        group_by(date) %>%
        summarize(q_lps = mean(IS_discharge)) %>%
        select(date, q_lps)

    truth <- calculate_truth(w3_chem, w3_q, period = 'annual', dn = dn, target_wy = target_wy)$estimate[1]

    out <- tibble(wy = target_wy,
                  Ca = truth)

    w3_true <- bind_rows(w3_true, out)
}

w3_true <- w3_true %>%
    mutate(method = 'true',
           wy = as.character(wy)) %>%
    select(wy, method, Ca)

# bring in published flux Ca
w3_flux_pub <- read.csv(here('data','hbef_published_flux','ws3_stream_monthly_flux_gHa.csv')) %>%
    mutate(date = paste0(Year, '-', Month, '-', '01'),
           wy = water_year(date, origin = 'usgs')) %>%
    select(wy, Ca_flux) %>%
    group_by(wy) %>%
    summarize(Ca = sum(Ca_flux)/1000) %>%
    filter(Ca > 0) %>%
    mutate(wy = as.character(wy),
           site_code = 'w3',
           method = 'published') %>%
    select(-Ca, Ca)

w3_all <- bind_rows(w3_flux_methods, w3_flux_pub, w3_true, w3_recc)

# look at flux time series
fluxpal <- c('#332288', '#117733','#44AA99', '#88CCEE', '#DDCC77', '#CC6677', '#AA4499', '#000000')
breaks <- c('average', 'pw', 'beale', 'rating','composite', 'true', 'published', 'recommended')
labels <- c('Average', 'LI', 'Beale', 'Rating','Composite', 'True', 'Published', 'Recommended')

p_ts <- w3_all %>%
    select(-site_code) %>%
    filter(as.integer(wy) > 2012,
           as.integer(wy) < 2018) %>%
ggplot( aes(x = as.integer(wy), y= Ca)) +
    geom_point(aes(color = method), size = 3) +
    geom_line(aes(color = method))+
    theme_rsfme() +
    scale_color_manual(breaks = breaks,
                       values = fluxpal,
                       labels = labels)+
    labs(x = '', color = 'Method',
         y = 'Annual Ca Load (kg/ha/yr)')

p_ts

ggsave_hess(filename = here('paper','figures', 'fig11_hbef_method_ts.png'))

# prepare comparison data
comp_data <- w3_all %>%
    left_join(w3_true, by = 'wy') %>%
    filter(!is.na(Ca.y),
           method.x != 'true',
           method.x != 'wrtds') %>%
    mutate(diff = Ca.x - Ca.y)

fit_check <- comp_data %>% filter(method.x == 'recommended')
summary(lm(Ca.y ~ Ca.x, data = fit_check))

# Panel A: 1:1 scatter
p_comp <- comp_data %>%
    ggplot(aes(x = Ca.y, y = Ca.x)) +
    geom_point(aes(fill = method.x), shape = 21, size = 2.5, color = 'black', stroke = 0.3) +
    geom_abline(slope = 1, linetype = 'dashed') +
    theme_rsfme() +
    scale_fill_manual(name = 'Method', breaks = breaks,
                      values = fluxpal, labels = labels) +
    scale_x_continuous(breaks = c(4, 6, 8, 10)) +
    expand_limits(x = c(4, 10)) +
    labs(y = 'Estimated Load (kg/ha/yr)',
         x = 'Sensor-Derived Load (kg/ha/yr)')

# Panel B: Difference from truth
p_diff <- comp_data %>%
    ggplot(aes(x = wy, y = diff, fill = method.x)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_rsfme() +
    scale_fill_manual(name = 'Method', breaks = breaks,
                      values = fluxpal, labels = labels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = 'Water Year',
         y = 'Difference from Sensor Load (kg/ha/yr)')

# Combine panels
p_combined <- p_comp + p_diff +
    plot_layout(guides = 'collect') +
    plot_annotation(tag_levels = 'A') &
    theme(legend.position = 'bottom')

ggsave_hess(filename = here('paper', 'figures', 'fig11_hbef_method_comparison.png'),
            plot = p_combined)

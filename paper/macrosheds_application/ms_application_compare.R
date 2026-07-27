library(tidyverse)
library(here)
library(patchwork)

source(here('source/plot_theme.R'))

# load annual loads computed by calculate_annual_flux.R
all_loads <- read_csv(here('paper', 'macrosheds_application', 'load_annual.csv'),
                      show_col_types = FALSE) %>%
    filter(method != 'wrtds',
           method != 'failed')

out_tbl <- all_loads %>%
    group_by(site_code, var, water_year) %>%
    summarize(
        ms_rec = load[ms_recommended == 1][1],
        max_val = max(load),
        min_val = min(load),
        mean_val = mean(load),
        rec_method = method[ms_recommended == 1][1],
        .groups = 'drop'
    ) %>%
    rename(site = site_code, solute = var, year = water_year)

# make method range plot #####
n_frame <- out_tbl %>%
    mutate(wcs_max = ((max_val-ms_rec)/((ms_rec+max_val)*.5)*100),
            wcs_min = ((min_val-ms_rec)/((ms_rec+min_val)*.5)*100),
           pct_range = ((max_val-min_val)/((min_val+max_val)*.5)*100),
           wcs_range = wcs_max-wcs_min) %>%
    filter(solute == 'NO3_N' |
           solute == 'Ca')

ggplot(n_frame, aes(x = rec_method)) +
    geom_bar()

# create boxplot
p_box <- n_frame %>%
    select(solute, pct_range, wcs_range) %>%
    pivot_longer(cols = -solute, names_to = 'var', values_to = 'val') %>%
    filter(var == 'pct_range') %>%
    ggplot(aes(x = solute, y = val, color = solute))+
    geom_boxplot(linewidth = 2)+
    scale_y_log10()+
    labs(x = 'Solute',
         y = 'Method Range (%)')+
    scale_x_discrete(labels = c('Calcium', 'Nitrate'))+
    scale_color_manual(labels = c('Calcium', 'Nitrate (as N)'),
                       values = c('Ca' = unname(solute_colors['Ca']), 'NO3_N' = unname(solute_colors['NO3'])))+
    theme_rsfme()+
    theme(legend.position = 'none')

# create density diagram
p_den <- out_tbl  %>%
     filter(solute == 'NO3_N' |
                solute == 'Ca') %>%
     select(ms_rec, solute) %>%
     ggplot(aes(x = ms_rec, color = solute))+
     geom_density(linewidth = 2)+
    scale_x_log10(breaks = c(1e-2, 1, 1e2),
                  labels = c('0.01', '1', '100'))+
    scale_color_manual(labels = c('Calcium', 'Nitrate (as N)'),
                         values = c('Ca' = unname(solute_colors['Ca']), 'NO3_N' = unname(solute_colors['NO3'])))+
    labs(x = 'Load (kg/ha/year)',
         y = 'Density',
         color = 'Solute')+
    theme_rsfme()
ggsave_hess(file = here('paper', 'macrosheds_application', 'method_comp_den.png'))

# combine figures and save out
p_den|p_box
ggsave_hess(file = here('paper', 'macrosheds_application', 'method_comp_combined.png'))

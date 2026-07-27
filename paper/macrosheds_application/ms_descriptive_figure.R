library(tidyverse)
library(here)

source(here('source/plot_theme.R'))

data <- read.csv(here('paper', 'macrosheds_application', 'load_annual.csv'))

label_tbl <- tibble(var = c('Ca', 'NO3_N'),
                    label = c('Calcium',
                              'Nitrate (as N)'))

data %>%
    filter(var %in% c('Ca', 'NO3_N'),
           ms_recommended > 0) %>%
    left_join(., label_tbl, by = 'var') %>%
    ggplot()+
        geom_histogram(aes(x = load, fill = var), color = 'black') +
        scale_x_log10(breaks = c(0.01, 1, 1000),
                      labels = c('0.01', '1', '1000'))+
        facet_wrap(~label, ncol = 1)+
    labs(x = 'Load (kg/ha/year, log)',
         y = 'Count')+
    scale_fill_manual(values = c('Ca' = unname(solute_colors['Ca']), 'NO3_N' = unname(solute_colors['NO3'])))+
    theme_rsfme()+
    theme(legend.position = 'none')
ggsave_hess(file = here('paper', 'macrosheds_application', 'descriptive_hist.png'))


library(tidyverse)
library(feather)
library(here)
library(patchwork)
library(ggthemes)

data <- read.csv(here('paper', 'macrosheds_application', 'load_annual.csv'))

label_tbl <- tibble(var = c('Ca', 'NO3_N'),
                    label = c('Calcium',
                              'Nitrate (as N)'))

data %>%
    filter(var %in% c('Ca', 'NO3_N'),
           ms_recommended > 0) %>% #need to fix this in rec pipeline
    left_join(., label_tbl, by = 'var') %>%
    ggplot()+
        geom_histogram(aes(x = load, fill = var), color = 'black') +
        scale_x_log10(breaks = c(0.01, 1, 1000),
                      labels = c('0.01', '1', '1000'))+
        facet_wrap(~label, ncol = 1)+
    labs(x = 'Load (kg/ha/year, log)',
         y = 'Count',
         title = 'MacroSheds Load Estimates')+
    scale_fill_manual(values = c('red', 'blue'))+
    theme_minimal()+
    theme(legend.position = 'none',
          text=element_text(size=20))
ggsave(file = here('paper', 'macrosheds_application', 'descriptive_hist.png'), width = 8, height = 6)


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
library(macrosheds)
library(ggthemes)

set.seed(53045)

source(here('source/flux_methods.R'))

# sp cond plots ####
## make file list ####
files <- list.files(here('paper', 'neon_discussion'), pattern="spCond")

files_years <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 4)

files_sites <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 5)

file_table <- tibble(site_code = files_sites, wy = files_years)

error_table <- tibble(wy = as.character(), site_code = as.character(),
                      n = as.integer(), error = as.numeric())

## compute errror ####
for(i in 1:length(files)){
    load(here('paper', 'neon_discussion', files[i]))
    truth <- out_tbl %>%
        filter(n == 1, method == 'composite') %>%
        pull(estimate)%>%
        .[1]

    err_int <- out_tbl %>%
        mutate(error = ((estimate-truth)*100)/truth,
               wy = files_years[i],
               site_code = files_sites[i])

    error_table <- bind_rows(error_table, err_int)
}

## set names and breaks #####
method_names <- c(
    `pw` = "Linear Interpolation",
    `beale` = "Beale",
    `rating` = "Rating",
    `composite` = "Composite"
)

breaks <- c(1,3,7,14,30,60)

x_labels <- c('Daily', 'Twice Weekly', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

## generate plots####

for(i in c('pw', 'beale', 'rating', 'composite')){
error_table %>%
    filter(method == i) %>%
    group_by(site_code, n) %>%
    mutate(max = max(error),
           min = min(error),
           median = median(error)) %>%
    ggplot(aes(x = n, shape = wy))+
    geom_line(aes(y = max))+
    geom_line(aes(y = min))+
    geom_line(aes(y = median), lwd = 1.5)+
    coord_cartesian(ylim = c(-30,30))+
    scale_fill_manual(name = 'Error',
                      values = c('chartreuse4','gold1'),
                      labels = c('+/-5%', '+/-20%'),
                      guide = guide_legend())+
    annotate('rect', xmin = -Inf,
             xmax = Inf,
             ymin = -5,
             ymax = 5,
             fill = 'chartreuse4',
             alpha = .2)+
    annotate('rect', xmin = -Inf,
         xmax = Inf,
         ymin = 5,
         ymax = 20,
         fill = 'gold1',
         alpha = .2)+
    annotate('rect', xmin = -Inf,
             xmax = Inf,
             ymin = -20,
             ymax = -5,
             fill = 'gold1',
             alpha = .2)+
    geom_hline(yintercept = 0, linetype = 'dashed', size = .25)+
    labs(x = 'Frequency',
         y = 'Error (%)')+
    theme_few()+
    scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE)
    )+
    theme(text = element_text(size = 20),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
          panel.spacing = unit(.25,'lines'))+
    facet_wrap(~site_code, ncol = 1)+
    labs(title = 'Conductivity Load Accuracy')

ggsave(here('paper', 'neon_discussion', paste0('cond_figure_', i,'.png')))
}

# turb plots ####
## make file list ####
files <- list.files(here('paper', 'neon_discussion'), pattern="turb")

files_years <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 4)

files_sites <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 5)

file_table <- tibble(site_code = files_sites, wy = files_years)

error_table <- tibble(wy = as.character(), site_code = as.character(),
                      n = as.integer(), error = as.numeric())

## compute errror ####
for(i in 1:length(files)){
    load(here('paper', 'neon_discussion', files[i]))
    truth <- out_tbl %>%
        filter(n == 1, method == 'composite') %>%
        pull(estimate)%>%
        .[1]

    err_int <- out_tbl %>%
        mutate(error = ((estimate-truth)*100)/truth,
               wy = files_years[i],
               site_code = files_sites[i])

    error_table <- bind_rows(error_table, err_int)
}

## set names and breaks #####
method_names <- c(
    `pw` = "Linear Interpolation",
    `beale` = "Beale",
    `rating` = "Rating",
    `composite` = "Composite"
)

breaks <- c(1,3,7,14,30,60)

x_labels <- c('Daily', 'Twice Weekly', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

## generate plots ####

for(i in c('pw', 'beale', 'rating', 'composite')){
    error_table %>%
        filter(method == i) %>%
        group_by(site_code, n) %>%
        mutate(max = max(error),
               min = min(error),
               median = median(error)) %>%
        ggplot(aes(x = n, shape = wy))+
        geom_line(aes(y = max))+
        geom_line(aes(y = min))+
        geom_line(aes(y = median), lwd = 1.5)+
        coord_cartesian(ylim = c(-30,30))+
        scale_fill_manual(name = 'Error',
                          values = c('chartreuse4','gold1'),
                          labels = c('+/-5%', '+/-20%'),
                          guide = guide_legend())+
        annotate('rect', xmin = -Inf,
                 xmax = Inf,
                 ymin = -5,
                 ymax = 5,
                 fill = 'chartreuse4',
                 alpha = .2)+
        annotate('rect', xmin = -Inf,
                 xmax = Inf,
                 ymin = 5,
                 ymax = 20,
                 fill = 'gold1',
                 alpha = .2)+
        annotate('rect', xmin = -Inf,
                 xmax = Inf,
                 ymin = -20,
                 ymax = -5,
                 fill = 'gold1',
                 alpha = .2)+
        geom_hline(yintercept = 0, linetype = 'dashed', size = .25)+
        labs(x = 'Frequency',
             y = 'Error (%)')+
        theme_few()+
        scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE)
        )+
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 20),
              panel.spacing = unit(.25,'lines'))+
        facet_wrap(~site_code, ncol = 1)+
        labs(title = 'Turbidity Load Accuracy')

    ggsave(here('paper', 'neon_discussion', paste0('turb_figure_', i,'.png')))
}

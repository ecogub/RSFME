library(tidyverse)
library(here)

set.seed(53045)

source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))

# sp cond plots ####
## make file list ####
files <- list.files(here('paper', 'neon_discussion'), pattern="spCond.*\\.RData$")

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
    mutate(max = max(error), min = min(error), median = median(error)) %>%
    ggplot(aes(x = n, shape = wy))+
    geom_line(aes(y = max))+
    geom_line(aes(y = min))+
    geom_line(aes(y = median), linewidth = 1.5)+
    coord_cartesian(ylim = c(-30,30))+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = error_band_colors['band_5pct'], alpha = .2)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = 5, ymax = 20, fill = error_band_colors['band_20pct'], alpha = .2)+
    annotate('rect', xmin = -Inf, xmax = Inf, ymin = -20, ymax = -5, fill = error_band_colors['band_20pct'], alpha = .2)+
    geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .25)+
    labs(x = 'Frequency', y = 'Error (%)',
         title = paste0('Conductivity Load Accuracy - ', method_labels[i]))+
    theme_rsfme()+
    scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE))+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    facet_wrap(~site_code, ncol = 1)

ggsave_hess(here('paper', 'neon_discussion', paste0('cond_figure_', i,'.png')))
}

# turb plots ####
## make file list ####
files <- list.files(here('paper', 'neon_discussion'), pattern="turb.*\\.RData$")

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
        mutate(max = max(error), min = min(error), median = median(error)) %>%
        ggplot(aes(x = n, shape = wy))+
        geom_line(aes(y = max))+
        geom_line(aes(y = min))+
        geom_line(aes(y = median), linewidth = 1.5)+
        coord_cartesian(ylim = c(-30,30))+
        annotate('rect', xmin = -Inf, xmax = Inf, ymin = -5, ymax = 5, fill = error_band_colors['band_5pct'], alpha = .2)+
        annotate('rect', xmin = -Inf, xmax = Inf, ymin = 5, ymax = 20, fill = error_band_colors['band_20pct'], alpha = .2)+
        annotate('rect', xmin = -Inf, xmax = Inf, ymin = -20, ymax = -5, fill = error_band_colors['band_20pct'], alpha = .2)+
        geom_hline(yintercept = 0, linetype = 'dashed', linewidth = .25)+
        labs(x = 'Frequency', y = 'Error (%)',
             title = paste0('Turbidity Load Accuracy - ', method_labels[i]))+
        theme_rsfme()+
        scale_x_continuous(breaks = breaks, labels = x_labels, guide = guide_axis(check.overlap = TRUE))+
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
        facet_wrap(~site_code, ncol = 1)

    ggsave_hess(here('paper', 'neon_discussion', paste0('turb_figure_', i,'.png')))
}

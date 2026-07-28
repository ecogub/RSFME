library(tidyverse)
library(here)

set.seed(53045)

source(here('source/flux_methods.R'))
source(here('source/plot_theme.R'))

# sp cond plots ####
## make file list ####
files <- list.files(here('data', 'coarsen_neon'), pattern="spCond.*\\.RData$")

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
    load(here('data', 'coarsen_neon', files[i]))
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

breaks <- c(1,3,7,14,30,60)

x_labels <- c('Daily', 'Twice Weekly', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

## generate plots####
# conductivity panels are paper Figures A1-A4; the map is also the loop order
cond_fig_num <- c(li = 'a01', beale = 'a02', rating = 'a03', composite = 'a04')
n_sites <- length(unique(error_table$site_code))

for(i in names(cond_fig_num)){
    method_data <- error_table %>% filter(method == i)
    y_ext <- ceiling(max(abs(method_data$error[method_data$n <= 30]), na.rm = TRUE) / 10) * 10
    y_ext <- max(y_ext, 25)

    method_data %>%
        group_by(site_code, n) %>%
        mutate(max = max(error), min = min(error), median = median(error)) %>%
        ggplot(aes(x = n, shape = wy))+
        geom_line(aes(y = max))+
        geom_line(aes(y = min))+
        geom_line(aes(y = median), linewidth = 1.5)+
        coord_cartesian(ylim = c(-y_ext, y_ext))+
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

    ggsave_hess(here('paper', 'figures',
                     paste0('fig', cond_fig_num[i], '_neon_cond_', i,'.png')),
                height = n_sites * FACET_ROW_CM)
}

# turb plots ####
## make file list ####
files <- list.files(here('data', 'coarsen_neon'), pattern="turb.*\\.RData$")

files_years <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 3) %>%
    str_replace('^turb', '')

files_sites <- files %>%
    tools::file_path_sans_ext() %>%
    str_split_i('_', 4)

file_table <- tibble(site_code = files_sites, wy = files_years)

error_table <- tibble(wy = as.character(), site_code = as.character(),
                      n = as.integer(), error = as.numeric())

## compute errror ####
for(i in 1:length(files)){
    load(here('data', 'coarsen_neon', files[i]))
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

breaks <- c(1,3,7,14,30,60)

x_labels <- c('Daily', 'Twice Weekly', 'Weekly', 'Biweekly', 'Monthly', 'Bimonthly')

## generate plots ####
# turbidity panels are paper Figures A5-A8; the map is also the loop order
turb_fig_num <- c(li = 'a05', beale = 'a06', rating = 'a07', composite = 'a08')
n_sites <- length(unique(error_table$site_code))

for(i in names(turb_fig_num)){
    method_data <- error_table %>% filter(method == i)
    n_cutoff <- if (i %in% c('beale', 'rating', 'composite')) 14 else 30
    y_ext <- ceiling(max(abs(method_data$error[method_data$n <= n_cutoff]), na.rm = TRUE) / 10) * 10
    y_ext <- max(y_ext, 25)

    method_data %>%
        group_by(site_code, n) %>%
        mutate(max = max(error), min = min(error), median = median(error)) %>%
        ggplot(aes(x = n, shape = wy))+
        geom_line(aes(y = max))+
        geom_line(aes(y = min))+
        geom_line(aes(y = median), linewidth = 1.5)+
        coord_cartesian(ylim = c(-y_ext, y_ext))+
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

    ggsave_hess(here('paper', 'figures',
                     paste0('fig', turb_fig_num[i], '_neon_turb_', i,'.png')),
                height = n_sites * FACET_ROW_CM)
}

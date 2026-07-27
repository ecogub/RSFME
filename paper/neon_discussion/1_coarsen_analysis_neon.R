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
library(RiverLoad)
library(macrosheds)

library(neonUtilities)

small_sites <- read_csv(here('paper', 'neon_discussion', 'ms_stream_order.csv')) %>%
    left_join(ms_site_data, by = 'site_code') %>%
    filter(domain == 'neon',
           stream_order == 1)

set.seed(53045)

source(here('source/flux_methods.R'))
source(here('paper/coarsen_plot/coarsen_helpers.R'))

# dl neon data
# zipsByProduct(dpID="DP1.20288.001", site=small_sites$site_code,
#                      package="expanded", release="current",
#                      check.size = F, savepath = here('paper', 'neon_discussion', 'data'))
# zipsByProduct(dpID="DP4.00130.001", site=small_sites$site_code,
#               package="expanded", release="current",
#               check.size = F, savepath = here('paper', 'neon_discussion', 'data'))
#
# stackByTable(filepath=here('paper', 'neon_discussion', 'data', 'filesToStack00130'))
# stackByTable(filepath=here('paper', 'neon_discussion', 'data', 'filesToStack20288'))
#set watershed attributes #####

macrosheds_root <- Sys.getenv("MACROSHEDS_ROOT", unset = here('data', 'macrosheds'))
q_dat<- ms_load_product(prodname = 'discharge', site_codes = unique(small_sites$site_code),
                        macrosheds_root = macrosheds_root)
c_dat<- ms_load_product(prodname = 'stream_chemistry', site_codes = unique(small_sites$site_code),
                        macrosheds_root = macrosheds_root)


for(target_site in unique(c_dat$site_code)){
area <- ms_site_data %>%
    filter(site_code == target_site) %>%
    pull(ws_area_ha)
# begin solute loop ####
for(target_solute in c('turbid_FNU')){
    ## set solute #####
    #target_solute = "turbid_FNU"
    gc()
    ## read in data ####
   q_data_in <- q_dat %>%
       filter(site_code == target_site) %>%
       select(date, site_code, q_lps = val)

   c_data_in <- c_dat %>%
       filter(site_code == target_site,
              var == target_solute,
              grab_sample == 0) %>%
       select(date, site_code, con = val)

   com_dat_raw <- q_data_in %>%
       full_join(c_data_in) %>%
       mutate(wy = as.integer(as.character(water_year(date, origin = 'usgs'))))

   good_years <- com_dat_raw %>% na.omit() %>% select(wy, site_code) %>% count(wy) %>%
       filter(n > 364)

   if(nrow(good_years) > 0){
   com_dat <- com_dat_raw %>%
       filter(wy %in% good_years$wy)


   for(target_wy in unique(com_dat$wy)){

    com_dat_wy <- com_dat %>%
       filter(wy == target_wy)

    if(nrow(na.omit(com_dat_wy)) > 300){

    ## calculate truth ####
    chem_df <- com_dat_wy %>%
        select(-q_lps) %>%
        na.omit()

    q_df <- com_dat_wy %>%
        select(-con) %>%
        na.omit()

    out_val <- generate_residual_corrected_con(chem_df = chem_df, q_df = q_df, sitecol = 'site_code', datecol = 'date') %>%
        rename(datetime = date) %>%
        calculate_composite_from_rating_filled_df() %>%
        pull(flux)
    truth <- tibble(method = 'truth', estimate = out_val)

    ## make gradually coarsened chem ###
    ## iniitalize output and loop
    coarse_chem <- list()
    loopid = 0

    # create vector of nth elements
    # go from daily to biweekly by day
    # set monthly and bimonthly discretely
    loop_vec <- c(seq(from = 1, to = 13, by = 1),
                  seq(from = 14, to = 28, by = 7),
                  30, 60, 90)

    ## Start coarsening loop ####
    reps <- 100
    for(i in loop_vec){
        n = i

        for(j in 1:reps){
            loopid <- loopid+1
            start_pos <- sample(1:n, size = 1) # take a random starting position from inside the interval
            coarse_chem[[loopid]] <- tibble(date =  nth_element(com_dat_wy$date, 1, n = start_pos),
                                            con = nth_element(com_dat_wy$con, 1, n = start_pos))
            names(coarse_chem)[loopid] <- paste0('sample_',n)
        }

            ## Start method application loop ####
            out_tbl <- tibble(method = as.character(), estimate = as.numeric(), n = as.integer())
            for(k in 2:length(coarse_chem)){

                n <- as.numeric(str_split_fixed(names(coarse_chem[k]), pattern = 'sample_', n = 2)[2])

                chem_df <- coarse_chem[[k]] %>%
                    group_by(lubridate::yday(date)) %>%
                    summarize(date = date(date),
                              con = mean(con)) %>%
                    ungroup() %>%
                    unique() %>%
                    select(date, con) %>%
                    mutate(site_code = target_site, wy = target_wy)

                out_tbl <- apply_methods_coarse(chem_df, q_df) %>%
                    mutate(n = n) %>%
                    rbind(., out_tbl)
            }
    }

            ## save/load data from previous runs #####
            if(target_solute == 'spCond'){save(out_tbl, file = here('paper','neon_discussion', paste0('TEST100reps_annual_spCond_', target_wy, '_', target_site, '.RData') ))}
            if(target_solute == 'turbid_FNU'){save(out_tbl, file = here('paper','neon_discussion', paste0('TEST100reps_annual_turb', target_wy, '_', target_site, '.RData')))}
   }else{next}
} }else{next} #end wy loop
} # end solute loop
} # end site loop


#### Thinning Functions for USGS Data ####
# Load in packages
library(dataRetrieval)
library(tidyverse)
library(lubridate)
library(glue)
library(feather)
library(zoo)
library(here)
library(lfstat)
library(RiverLoad)
library(imputeTS)

# source helper functions
source('source/helper_functions.R')

# Thinning Functions
# Daily Thinning
usgs_thin_daily <- function(chem_data, site, var) {
    chem_data_thin <- chem_data %>%
      filter(hour(datetime) %in% c(13:18)) %>%
      mutate(date = lubridate::date(datetime)) %>%
      distinct(date, .keep_all = T) %>%
      select(-datetime)

    directory <- glue('data/thinned/{var}/{t}',
                      t = 'daily')

    if(!dir.exists(directory)){
      dir.create(directory, recursive = TRUE)
    }

    write_feather(chem_data_thin, glue('{directory}/{site}.feather'))
}

# Weekly Thinning
usgs_thin_weekly <- function(chem_data) {
    chem_data_thin <- chem_data %>%
        # mutate(date = date(datetime)) %>%
        # group_by(site_code, var, date) %>%
        # summarise(val = mean(val, na.rm = T)) %>%
        filter(hour(datetime) %in% c(13:18)) %>%
        filter(lubridate::wday(datetime) == 1) %>%
        mutate(date = lubridate::date(datetime)) %>%
        distinct(date, .keep_all = T) %>%
        select(-datetime)

    directory <- glue('data/thinned/{var}/{t}',
                      t = 'weekly')

    if(!dir.exists(directory)){
        dir.create(directory, recursive = TRUE)
    }

    write_feather(chem_data_thin, glue('{directory}/{site}.feather'))
}

# Biweekly Thinning
usgs_thin_biweekly <- function(chem_data) {
    chem_data_thin <- chem_data %>%
        # mutate(date = date(datetime)) %>%
        # group_by(site_code, var, date) %>%
        # summarise(val = mean(val, na.rm = T)) %>%
        filter(hour(datetime) %in% c(13:18)) %>%
        filter(lubridate::mday(datetime) %in% c(1, 15)) %>%
        mutate(date = lubridate::date(datetime)) %>%
        distinct(date, .keep_all = T) %>%
        select(-datetime)

    directory <- glue('data/thinned/{var}/{t}',
                      t = 'biweekly')

    if(!dir.exists(directory)){
        dir.create(directory, recursive = TRUE)
    }

    write_feather(chem_data_thin, glue('{directory}/{site}.feather'))
}

# Monthly Thinning
usgs_thin_monthly <- function(chem_data) {

    chem_data_thin <- chem_data %>%
        filter(hour(datetime) %in% c(13:18)) %>%
        mutate(date = date(datetime)) %>%
        filter(day(date) == 1) %>%
        distinct(date, .keep_all = T) %>%
        select(-datetime)

    directory <- glue('data/thinned/{var}/{t}',
                      t = 'monthly')

    if(!dir.exists(directory)){
        dir.create(directory, recursive = TRUE)
    }

    write_feather(chem_data_thin, glue('{directory}/{site}.feather'))
}


# watershed attribute functions
get_site_ecoregions <- function(site_data, eco_fp, good_sites = NULL, level = "NA_L2NAME") {

    if(typeof(site_data) == 'character') {
      ws <- read.csv(site_fp, colClasses = "character") %>%
          sf::st_as_sf(., coords = c('long', 'lat'), crs = 4326) %>%
          st_make_valid()
    } else {
      ws <- site_data %>%
          sf::st_as_sf(., coords = c('long', 'lat'), crs = 4326) %>%
          st_make_valid()
    }

    if(!is.null(good_sites)) {
      ws <- ws %>%
          filter(site_code %in% !!good_sites)
    }

    epa_eco_ii <- read_sf(eco_fp) %>%
      select(!!level, geometry) %>%
      st_transform(crs = 4326) %>%
      st_make_valid()

    site_eco <- st_intersection(ws, epa_eco_ii)

    return(site_eco)
}

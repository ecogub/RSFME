# Flux Functions
# PREP and HELPERS
### Riverload conversion function #####
prep_raw_for_riverload <- function(chem_df, q_df, datecol = 'date'){
    conv_q <- q_df %>%
                mutate(datetime = as.POSIXct(get(datecol), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) %>%
                mutate(flow = q_lps*0.001) %>% # convert lps to cubic meters per second)
                select(datetime, flow) %>%
                arrange(datetime) %>%
                data.frame()
    if(month(conv_q$datetime[1]) == 9 & day(conv_q$datetime[1]) == 30){
        conv_q = conv_q[-1,]
    }

    conv_c <- chem_df %>%
                mutate(datetime = as.POSIXct(get(datecol), format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')) %>%
                select(datetime, con) %>%
                data.frame()

    db <- full_join(conv_q, conv_c, by = "datetime") %>%
                #filter(!is.na(flow)) %>%
                arrange(datetime)

    return(db)
}


# FLUX CALCS
###### calculate linear interpolation #########
# RiverLoad::method6 linearly interpolates concentration and discharge to a
# daily series (approx(), rule = 2) and sums C * Q * 86400. This is the method
# described in the paper. Do NOT use method1 here: it computes
# mean(C) * mean(Q) * duration, an averaging estimator that discards the
# covariance between concentration and discharge.
calculate_li <- function(chem_df, q_df, datecol = 'date', period = NULL){
  rl_data <- prep_raw_for_riverload(chem_df = chem_df, q_df = q_df, datecol = datecol)

  if(is.null(period)){
  flux_from_li <- method6(rl_data, ncomp = 1) %>%
    sum(.)/(1000*area)
  }else{

  if(period == 'month'){

      flux_from_li <- method6(rl_data, ncomp = 1, period = period)

      flux_from_li <- tibble(date = rownames(flux_from_li),
                          flux = (flux_from_li[,1]/(1000*area)))


  }
  }

  return(flux_from_li)
}

###### calculate beale ######
calculate_beale <- function(chem_df, q_df, datecol = 'date', period = NULL){
    rl_data <- prep_raw_for_riverload(chem_df = chem_df, q_df = q_df, datecol = datecol)

    if(is.null(period)){
    flux_from_beale <- beale.ratio(rl_data, ncomp = 1) %>%
      sum(.)/(1000*area)
    }else{

    if(period == 'month'){
        flux_from_beale <- beale.ratio(rl_data, ncomp = 1, period = period)

        flux_from_beale <- tibble(date = rownames(flux_from_beale),
                               flux = (flux_from_beale[,1]/(1000*area)))
    }
    }

    return(flux_from_beale)
}

##### calculate rating #####
calculate_rating <- function(chem_df, q_df, datecol = 'date', period = NULL){
    rl_data <- prep_raw_for_riverload(chem_df = chem_df, q_df = q_df, datecol = datecol)

    if(is.null(period)){
    flux_from_reg <- RiverLoad::rating(rl_data, ncomp = 1) %>%
        sum(.)/(1000*area)
    }else{

    if(period == 'month'){
        flux_from_reg <- RiverLoad::rating(rl_data, ncomp = 1, period = period)

        flux_from_reg <- tibble(date = rownames(flux_from_reg),
                                  flux = (flux_from_reg[,1]/(1000*area)))
    }
    }

    return(flux_from_reg)
}


generate_residual_corrected_con <- function(chem_df, q_df, datecol = 'date', sitecol = 'site_no'){
        # first make c:q rating
        paired_df <- q_df %>%
            full_join(chem_df, by = c(datecol, sitecol, 'wy'), relationship = 'many-to-many') %>%
            na.omit() %>%
            filter(q_lps > 0,
                   is.finite(q_lps))

        if(nrow(paired_df) <= 2){return(NA)}else{

        q_log <- log10(paired_df$q_lps)
        c_log <- log10(paired_df$con)
        model_data <- tibble(c_log, q_log) %>%
            filter(is.finite(c_log),
                   is.finite(q_log))%>%
            na.omit()

        rating <- summary(lm(model_data$c_log ~ model_data$q_log, singular.ok = T))

        # extract model info
        intercept <- rating$coefficients[1]
        slope <- rating$coefficients[2]

        # create modeled c, calc residuals, adjust modeled c by interpolated residuals
        rating_filled_df <- q_df %>%
          mutate(con_reg = 10^(intercept+(slope*log10(q_lps)))) %>%
          select(all_of(datecol), con_reg, q_lps) %>%
          full_join(., chem_df, by = datecol, relationship = 'many-to-many') %>%
          select(site_code, all_of(datecol), con, con_reg, q_lps, wy)  %>%
            mutate(res = con_reg-con,
                   res = imputeTS::na_interpolation(res),
                   con_com = con_reg-res,
                   site_code = get(sitecol),
                   wy = water_year(get(datecol), origin = 'usgs'))

        rating_filled_df$con_com[!is.finite(rating_filled_df$con_com)] <- NA
        return(rating_filled_df)
        }
        }

##### calculate monthly flux from composite ####
calculate_composite_from_rating_filled_df <- function(rating_filled_df, site_no = 'site_no', period = NULL){

        if(is.null(period)){
        flux_from_comp <- rating_filled_df %>%
            select(datetime, con_com, q_lps, wy) %>%
            na.omit() %>%
            mutate(flux = con_com*q_lps*86400*(1/area)*1e-6) %>%
            group_by(wy) %>%
          summarize(flux = sum(flux)) %>%
            mutate(site_code = site_no)
        }else{

        if(period == 'month'){
            flux_from_comp <- rating_filled_df %>%
                mutate(month = month(datetime),
                       flux = con_com*q_lps*86400*(1/area)*1e-6) %>%
                group_by(wy, month) %>%
                summarize(date = max(datetime),
                          flux = sum(flux))
        }
        }

        return(flux_from_comp)
        }

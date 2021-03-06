estimate_flux_rating_daily <- function(chem_df, q_df, ws_size){
    
    paired_df <- chem_df %>%
        full_join(., q_df, by = c('date', 'water_year')) %>%
        na.omit() %>%
        filter(q_lps > 0,
               is.finite(q_lps))
    
    q_log <- log10(paired_df$q_lps)
    c_log <- log10(paired_df$con)
    model_data <- tibble(c_log, q_log) %>%
        filter(is.finite(c_log),
               is.finite(q_log))%>%
        na.omit()
    
    rating <- summary(lm(model_data$c_log ~ model_data$q_log))
    
    intercept <- rating$coefficients[1]
    slope <- rating$coefficients[2]
    
    new_chem_df <- q_df %>%
        mutate(con = 10^(intercept+(slope*log10(q_lps)))) %>%
        select(date, con, wy = water_year)
    
    flux_df <- new_chem_df %>%
        full_join(., q_df, by = c('date', 'site_code')) %>%
        mutate(flux_kg_ha = ((con*q_lps*86400)/(1e6*ws_size)),
               method = 'rating') %>%
        ungroup() %>%
        select(date, flux = flux_kg_ha, method, wy) 
    
    return(flux_df)
}

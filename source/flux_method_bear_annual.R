# create function of hbef flux estimation for annual
estimate_flux_bear_annual <- function(chem_df, q_df, ws_size){
  source('source/flux_method_bear.R')
  flux_df <- estimate_flux_bear(chem_df = chem_df, q_df = q_df, ws_size = ws_size)
  # compute total flux in kg/ha and return it
  out <- flux_df %>%
    group_by(wy = water_year(date, origin = "usgs")) %>%
    summarize(flux = sum(flux))
  
  out <- out %>%
    mutate(method = 'bear')
  return(out)
}
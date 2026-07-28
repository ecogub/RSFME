apply_methods_coarse <- function(chem_df, q_df){
    out <- tibble(method = as.character(), estimate = as.numeric())
    out[1,2] <- calculate_li(chem_df, q_df)
    out[2,2] <- calculate_beale(chem_df, q_df)
    out[3,2] <- calculate_rating(chem_df, q_df)
    comp_df <- generate_residual_corrected_con(chem_df = chem_df, q_df = q_df, sitecol = 'site_code')
    if (is.data.frame(comp_df)) {
        out[4,2] <- comp_df %>%
            rename(datetime = date) %>%
            calculate_composite_from_rating_filled_df() %>%
            pull(flux)
    } else {
        out[4,2] <- NA_real_
    }
    out$method <- c('li', 'beale', 'rating', 'composite')
    return(out)
}

nth_element <- function(vector, starting_position, n) {
    vector[seq(starting_position, length(vector), n)]
}

# Shared coarsening experiment: thin a time series at multiple frequencies,
# apply flux methods at each, return a tibble of estimates.
#
# ts_df: tibble with columns date, con, q_lps (any temporal resolution)
# site_code, target_wy: identifiers added to intermediate data frames
# area: watershed area in ha (assigned to .GlobalEnv for flux_methods.R)
# loop_vec: integer vector of thinning intervals (in units of ts_df rows)
# reps: number of random-start repetitions per interval
# daily_agg: if TRUE, aggregate ts_df to daily means before computing
#            truth and building q_df (needed for sub-daily input like HBEF/Plynlimon)
run_coarsening_experiment <- function(ts_df, site_code, target_wy, area,
                                      loop_vec, reps = 100, daily_agg = TRUE) {
    assign('area', area, envir = .GlobalEnv)

    if (daily_agg) {
        chem_daily <- ts_df %>%
            group_by(lubridate::yday(date)) %>%
            summarize(date = lubridate::date(date), con = mean(con), .groups = 'drop') %>%
            unique() %>%
            select(date, con) %>%
            mutate(site_code = site_code, wy = target_wy)

        q_daily <- ts_df %>%
            select(date, q_lps) %>%
            group_by(lubridate::yday(date)) %>%
            summarize(date = lubridate::date(date), q_lps = mean(q_lps), .groups = 'drop') %>%
            unique() %>%
            mutate(site_code = site_code, wy = target_wy)
    } else {
        chem_daily <- ts_df %>%
            select(date, con) %>%
            na.omit() %>%
            mutate(site_code = site_code, wy = target_wy)

        q_daily <- ts_df %>%
            select(date, q_lps) %>%
            na.omit() %>%
            mutate(site_code = site_code, wy = target_wy)
    }

    truth_df <- generate_residual_corrected_con(
        chem_df = chem_daily, q_df = q_daily, sitecol = 'site_code')
    if (!is.data.frame(truth_df)) {
        warning(sprintf("Truth computation failed for %s WY%s (too few paired obs), skipping",
                        site_code, target_wy), call. = FALSE, immediate. = TRUE)
        return(tibble(method = character(), estimate = numeric(), n = numeric()))
    }
    truth_val <- truth_df %>%
        rename(datetime = date) %>%
        calculate_composite_from_rating_filled_df() %>%
        pull(flux)

    out_list <- list()

    for (coarse_n in loop_vec) {
        for (j in 1:reps) {
            start_pos <- sample(1:coarse_n, size = 1)
            coarse_data <- tibble(
                date = nth_element(ts_df$date, start_pos, n = coarse_n),
                con  = nth_element(ts_df$con,  start_pos, n = coarse_n))

            chem_df <- coarse_data %>%
                group_by(lubridate::yday(date)) %>%
                summarize(date = lubridate::date(date), con = mean(con), .groups = 'drop') %>%
                unique() %>%
                select(date, con) %>%
                mutate(site_code = site_code, wy = target_wy)

            out_list[[length(out_list) + 1]] <- apply_methods_coarse(chem_df, q_daily) %>%
                mutate(n = coarse_n)
        }
    }

    bind_rows(out_list)
}

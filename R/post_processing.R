#' Raw output to long
#' 
#' Converts wide (by age groups) to long
#'
#' @param x Model output
#' @param ... Additional columns to select (for example run names)
#'
#' @return Long output
model_output_to_long <- function(x, ...){
  x %>%
    # Convert all to long
    tidyr::pivot_longer(-c(.data$year, ...), names_to = "var", values_to = "y") %>%
    # Remove _smooth subscript for neater names
    dplyr::mutate(var = stringr::str_remove(.data$var, "_smooth")) %>%
    # Isolate lower and upper age bounds from variable names
    tidyr::separate(.data$var, into = c("type", "age_lower", "age_upper"), sep = "_", convert = TRUE) %>%
    # Convert back to wide
    tidyr::pivot_wider(id_cols = c(..., .data$year, .data$age_lower, .data$age_upper), names_from = .data$type, values_from = .data$y)
}

#' Replaces -999 values in outputs
#'
#' @param x Model output
replace_missing <- function(x){
  x %>%
    dplyr::mutate(prev = ifelse(.data$prev == -999, 0, .data$prev),
                  inc = ifelse(.data$inc == -999, 0, .data$inc),
                  inc = ifelse(.data$inc == Inf, 0, .data$inc),
                  sev = ifelse(.data$sev == -999, 0, .data$sev),
                  sev = ifelse(.data$sev == Inf, 0, .data$sev),
                  prop = ifelse(.data$prop == -999, 0, .data$prop)) %>%
    tidyr::replace_na(
      list(
        prev = 0,
        inc = 0,
        sev = 0,
        prop = 0
      )
    )
}

#' Isolate annual summary
#' 
#' To ensure compatible run options as GTS modelling we return monthly output. However,
#' to reduce storage and processing requirements we immediately trim to annual output.
#'
#' @param x Model output
#' @param baseline_year Start year
#' @param max_year End year
#'
#' @return Annual output
year_summary <- function(x, baseline_year = 2000, max_year = 2050){
  x %>%
    # We -1 here as we are using smoothed outputs (the average over the previous 12 months)
    ## Therefore the smoothed output for time 2010 is essentially the average over 2009 
    dplyr::mutate(year = round(.data$year + baseline_year - 1, 1)) %>%
    # Select the integer years in specified range
    dplyr::filter(.data$year %in% baseline_year:max_year)
}

#' Add mortality rate (dependent on severe case incidence and treatment coverage)
#' 
#' This follows methodology in \href{https://pubmed.ncbi.nlm.nih.gov/26809816/}{Griffin _et al_, 2016}.
#'
#' @param x Model output
#' @param scaler Severe case to death scaler
#' @param treatment_scaler Treatment modifier
mortality_rate <- function(x, scaler = 0.215, treatment_scaler = 0.5, treatment_coverage = 0.52){
  x %>%
    dplyr::mutate(mort = (1 - (treatment_scaler * treatment_coverage)) * scaler * .data$sev)
}

process_epi <- function(out, pop = 100000){
  out %>%
    # Dropping non_smooth output and intervention number output
    dplyr::select(pfpr, season, draw, rtss_coverage, year, dplyr::contains("smooth")) %>%
    # Formatting
    model_output_to_long(pfpr, season, draw, rtss_coverage) %>%
    # Replace -999
    replace_missing() %>%
    # Process epi outputs
    mortality_rate(treatment_coverage = 0)%>%
    mutate(cases = round(inc * prop * pop),
           deaths = round(mort * prop * pop))
}

process_vx_tx <- function(x){
  x %>%
    dplyr::mutate(
      num_vaccinees = c(0, diff(num_vaccinees)),
      num_vacc_doses = c(0, diff(num_vacc_doses)),
      num_vaccinees_boost = c(0, diff(num_vaccinees_boost)),
      num_act = c(0, diff(num_act)),
      num_non_act = c(0, diff(num_trt)) - num_act) %>%
    select(pfpr, season, draw, rtss_coverage, year, num_vaccinees, num_vacc_doses, num_vaccinees_boost, num_act, num_non_act)

  
}

estimate_impact <- function(x){
  # Counterfactual runs
  x_cf <- x  %>%
    select(-prev, -inc, -sev, -mort, -prop) %>%
    filter(rtss_coverage == 0) %>%
    select(-rtss_coverage) %>%
    rename(cases_cf = cases,
           deaths_cf = deaths)
  # Vx runs
  x_int <- x %>%
    filter(rtss_coverage > 0)
  # Comparison
  compare <- left_join(x_int, x_cf, by = c("pfpr", "season", "draw", "year", "age_lower", "age_upper")) %>%
    mutate(cases_averted = cases_cf - cases,
           deaths_averted = deaths_cf - deaths)
  
  return(compare)
}

tx_cf <- function(x){
  x_cf <- x %>%
    filter(rtss_coverage == 0) %>%
    select(-rtss_coverage, -num_vaccinees, -num_vacc_doses, -num_vaccinees_boost) %>%
    rename(num_act_cf = num_act,
           num_non_act_cf = num_non_act)

  x %>%
    filter(rtss_coverage > 0) %>%
    left_join(x_cf, by = c("pfpr", "season", "draw", "year"))
}

aggregate_epi <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(across(cases:deaths_averted, sum)) %>%
    ungroup()
}

aggregate_vx_tx <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(across(num_vaccinees:num_non_act_cf, sum)) %>%
    ungroup()
}





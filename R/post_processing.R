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


#' Join life expectancy
#'
#' @param x data
#' @param le Life expectancy table
life_expectancy <- function(x, le){
  x %>%
    left_join(le, by = c("age_lower", "age_upper"))
}


#' Estimate DALYs
#'
#' @param x data
dalys <- function(x){
  x %>%
    mutate(dalys = deaths * life_expectancy + cases * 0.01375342 * 0.211 + severe * 0.04794521 * 0.6,
           # Discounted dalys
           ddalys = deaths * life_expectancy_discounted + cases * 0.01375342 * 0.211 + severe * 0.04794521 * 0.6) %>%
    select(-life_expectancy, -life_expectancy_discounted)
}


process_epi <- function(out, pop = 100000, tx = 0.52){
  le <- read.csv("analysis/data/raw_data/life_expectancy.csv")
  out %>%
    # Dropping non_smooth output and intervention number output
    dplyr::select(pfpr, season, draw, rtss_coverage, year, dplyr::contains("smooth")) %>%
    # Formatting
    model_output_to_long(pfpr, season, draw, rtss_coverage) %>%
    # Replace -999
    replace_missing() %>%
    # Process epi outputs
    mortality_rate(treatment_coverage = 0) %>%
    mutate(cases = round(inc * prop * pop),
           severe = round(sev * prop * pop),
           hospitalisations = round(severe * tx),
           deaths = round(mort * prop * pop)) %>%
    life_expectancy(le = le) %>%
    dalys()
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
           severe_cf = severe,
           hospitalisations_cf = hospitalisations,
           deaths_cf = deaths,
           dalys_cf = dalys,
           ddalys_cf = ddalys)
  # Vx runs
  x_int <- x %>%
    filter(rtss_coverage > 0)
  # Comparison
  compare <- left_join(x_int, x_cf, by = c("pfpr", "season", "draw", "year", "age_lower", "age_upper")) %>%
    mutate(cases_averted = cases_cf - cases,
           severe_averted = severe_cf - severe,
           hospitalisations_averted = hospitalisations_cf - hospitalisations,
           deaths_averted = deaths_cf - deaths,
           dalys_averted = dalys_cf - dalys,
           ddalys_averted = ddalys_cf - ddalys)
  
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
    summarise(across(cases:ddalys_averted, sum)) %>%
    ungroup()
}

aggregate_vx_tx <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(across(num_vaccinees:num_non_act_cf, sum)) %>%
    ungroup()
}

add_costs <- function(x, cost_per_dose = c(2, 5, 10), delivery_cost = c(0.96, 1.62,	2.67), tx_unit_cost = 1.47, severe_unit_cost = 22.41){
  cost_df <- expand_grid(cost_per_dose = cost_per_dose, delivery_cost = delivery_cost)
  merge(x, cost_df, by = NULL) %>%
    mutate(full_cost_per_dose = case_when(
      cost_per_dose == 2 ~ 2.69,
      cost_per_dose == 5 ~ 6.52,
      cost_per_dose == 10 ~ 12.91
    ),
    vaccine_cost = num_vacc_doses * (full_cost_per_dose + delivery_cost),
    tx_cost = (num_act + num_non_act) * tx_unit_cost,
    tx_cost_cf = (num_act_cf + num_non_act_cf) * tx_unit_cost,
    severe_cost = severe * severe_unit_cost,
    severe_cost_cf = severe_cf * severe_unit_cost,
    cost = vaccine_cost + tx_cost + severe_cost,
    cost_cf = tx_cost_cf + severe_cost_cf,
    marginal_cost = cost - cost_cf)
}

add_proportion <- function(x){
  x %>%
    mutate(prop_cases_averted = cases_averted / cases_cf,
           prop_deaths_averted = deaths_averted / deaths_cf)
}

extimate_icer <- function(x){
  x %>%
    mutate(
      icer_case = marginal_cost / cases_averted,
      icer_daly = marginal_cost / dalys_averted,
      icer_ddaly = marginal_cost / ddalys_averted
    )
}

add_impact_fvp <- function(x){
  x %>%
    mutate(
      cases_averted_per_100000_fvp = 100000 * (cases_averted / num_vaccinees),
      severe_averted_per_100000_fvp = 100000 * (severe_averted / num_vaccinees),
      hospitalisations_averted_per_100000_fvp = 100000 * (hospitalisations_averted / num_vaccinees),
      deaths_averted_per_100000_fvp = 100000 * (deaths_averted / num_vaccinees)
    )
}

quantile_impact <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(
      cases_averted_median = quantile(cases_averted, 0.5),
      cases_averted_lower = quantile(cases_averted, 0.025),
      cases_averted_upper = quantile(cases_averted, 0.975),
      
      hospitalisations_averted_median = quantile(hospitalisations_averted, 0.5),
      hospitalisations_averted_lower = quantile(hospitalisations_averted, 0.025),
      hospitalisations_averted_upper = quantile(hospitalisations_averted, 0.975),
      
      deaths_averted_median = quantile(deaths_averted, 0.5),
      deaths_averted_lower = quantile(deaths_averted, 0.025),
      deaths_averted_upper = quantile(deaths_averted, 0.975),
      
      cases_averted_per_100000_fvp_median = quantile(cases_averted_per_100000_fvp, 0.5),
      cases_averted_per_100000_fvp_lower = quantile(cases_averted_per_100000_fvp, 0.025),
      cases_averted_per_100000_fvp_upper = quantile(cases_averted_per_100000_fvp, 0.975),
      
      severe_averted_per_100000_fvp_median = quantile(severe_averted_per_100000_fvp, 0.5),
      severe_averted_per_100000_fvp_lower = quantile(severe_averted_per_100000_fvp, 0.025),
      severe_averted_per_100000_fvp_upper = quantile(severe_averted_per_100000_fvp, 0.975),
      
      hospitalisations_averted_per_100000_fvp_median = quantile(hospitalisations_averted_per_100000_fvp, 0.5),
      hospitalisations_averted_per_100000_fvp_lower = quantile(hospitalisations_averted_per_100000_fvp, 0.025),
      hospitalisations_averted_per_100000_fvp_upper = quantile(hospitalisations_averted_per_100000_fvp, 0.975),
      
      deaths_averted_per_100000_fvp_median = quantile(deaths_averted_per_100000_fvp, 0.5),
      deaths_averted_per_100000_fvp_lower = quantile(deaths_averted_per_100000_fvp, 0.025),
      deaths_averted_per_100000_fvp_upper = quantile(deaths_averted_per_100000_fvp, 0.975),
      
      icer_case_median = quantile(icer_case, 0.5),
      icer_case_lower = quantile(icer_case, 0.025),
      icer_case_upper = quantile(icer_case, 0.975),
      
      icer_daly_median = quantile(icer_daly, 0.5),
      icer_daly_lower = quantile(icer_daly, 0.025),
      icer_daly_upper = quantile(icer_daly, 0.975),
      
      icer_ddaly_median = quantile(icer_ddaly, 0.5),
      icer_ddaly_lower = quantile(icer_ddaly, 0.025),
      icer_ddaly_upper = quantile(icer_ddaly, 0.975)
    ) %>%
    ungroup()
}

quantile_epi_impact <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(
      cases_averted_median = quantile(cases_averted, 0.5),
      cases_averted_lower = quantile(cases_averted, 0.025),
      cases_averted_upper = quantile(cases_averted, 0.975),
      
      hospitalisations_averted_median = quantile(hospitalisations_averted, 0.5),
      hospitalisations_averted_lower = quantile(hospitalisations_averted, 0.025),
      hospitalisations_averted_upper = quantile(hospitalisations_averted, 0.975),
      
      deaths_averted_median = quantile(deaths_averted, 0.5),
      deaths_averted_lower = quantile(deaths_averted, 0.025),
      deaths_averted_upper = quantile(deaths_averted, 0.975),
      
      cases_averted_per_100000_fvp_median = quantile(cases_averted_per_100000_fvp, 0.5),
      cases_averted_per_100000_fvp_lower = quantile(cases_averted_per_100000_fvp, 0.025),
      cases_averted_per_100000_fvp_upper = quantile(cases_averted_per_100000_fvp, 0.975),
      
      severe_averted_per_100000_fvp_median = quantile(severe_averted_per_100000_fvp, 0.5),
      severe_averted_per_100000_fvp_lower = quantile(severe_averted_per_100000_fvp, 0.025),
      severe_averted_per_100000_fvp_upper = quantile(severe_averted_per_100000_fvp, 0.975),
      
      hospitalisations_averted_per_100000_fvp_median = quantile(hospitalisations_averted_per_100000_fvp, 0.5),
      hospitalisations_averted_per_100000_fvp_lower = quantile(hospitalisations_averted_per_100000_fvp, 0.025),
      hospitalisations_averted_per_100000_fvp_upper = quantile(hospitalisations_averted_per_100000_fvp, 0.975),
      
      deaths_averted_per_100000_fvp_median = quantile(deaths_averted_per_100000_fvp, 0.5),
      deaths_averted_per_100000_fvp_lower = quantile(deaths_averted_per_100000_fvp, 0.025),
      deaths_averted_per_100000_fvp_upper = quantile(deaths_averted_per_100000_fvp, 0.975)) %>%
    ungroup()
}

quantile_table <- function(x, ...){
  x %>%
    group_by(...) %>%
    summarise(
      prop_cases_averted_median = quantile(prop_cases_averted, 0.5),
      prop_cases_averted_lower = quantile(prop_cases_averted, 0.025),
      prop_cases_averted_upper = quantile(prop_cases_averted, 0.975),
      
      prop_deaths_averted_median = quantile(prop_deaths_averted, 0.5),
      prop_deaths_averted_lower = quantile(prop_deaths_averted, 0.025),
      prop_deaths_averted_upper = quantile(prop_deaths_averted, 0.975),
      
      cases_averted_per_100000_fvp_median = quantile(cases_averted_per_100000_fvp, 0.5),
      cases_averted_per_100000_fvp_lower = quantile(cases_averted_per_100000_fvp, 0.025),
      cases_averted_per_100000_fvp_upper = quantile(cases_averted_per_100000_fvp, 0.975),
      
      hospitalisations_averted_per_100000_fvp_median = quantile(hospitalisations_averted_per_100000_fvp, 0.5),
      hospitalisations_averted_per_100000_fvp_lower = quantile(hospitalisations_averted_per_100000_fvp, 0.025),
      hospitalisations_averted_per_100000_fvp_upper = quantile(hospitalisations_averted_per_100000_fvp, 0.975),
      
      deaths_averted_per_100000_fvp_median = quantile(deaths_averted_per_100000_fvp, 0.5),
      deaths_averted_per_100000_fvp_lower = quantile(deaths_averted_per_100000_fvp, 0.025),
      deaths_averted_per_100000_fvp_upper = quantile(deaths_averted_per_100000_fvp, 0.975),
      
      icer_case_median = quantile(icer_case, 0.5),
      icer_case_lower = quantile(icer_case, 0.025),
      icer_case_upper = quantile(icer_case, 0.975),
      
      icer_ddaly_median = quantile(icer_ddaly, 0.5),
      icer_ddaly_lower = quantile(icer_ddaly, 0.025),
      icer_ddaly_upper = quantile(icer_ddaly, 0.975)
    ) %>%
    ungroup()
}

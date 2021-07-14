#' PfPr levels
#' 
#' Transmission levels to include. These represent baseline PfPR2–10 levels.
#'
#' @export
pfpr_levels <- function(){
  c(0.03, 0.05, 0.1, 0.15, 0.2, 0.25, 0.35, 0.45, 0.55, 0.65)
}

#' Define seasonal profiles
#'
#' @param type Type of profile, this can either be "perennial" or "seasonal". The perennial
#' is flat, the seasonal profile is lifted from the rainfall fit for Bamako in Mali.
#'
#' @export
seasonality <- function(type){
  if(type == "perennial"){
    return(c(seasonal_a0 = 1, seasonal_a1 = 0, seasonal_b1 = 0, seasonal_a2 = 0, seasonal_b2 = 0, seasonal_a3 = 0, seasonal_b3 = 0))
  }
  if(type == "seasonal"){
    return(c(seasonal_a0 = 1.14, seasonal_a1 = -1.55, seasonal_b1 = -1.05, seasonal_a2 = 0.365, seasonal_b2 = 0.969, seasonal_a3 = 0.0721, seasonal_b3 = -0.309))
  }
  stop("Type not recognised")
}

#' Vector species proportions
#'
#' @param prop_gamb_ss Proportion gambiae ss
#' @param prop_fun Proportion funestus
#' @param prop_arab Proportion arabiensis
#'
#' @export
vectors <- function(prop_gamb_ss = 0.5, prop_fun = 0.25, prop_arab = 0.25){
  data.frame(
    prop_gamb_ss = prop_gamb_ss,
    prop_fun = prop_fun,
    prop_arab = prop_arab,
    irs_dif_gamb_ss = 0.813,
    irs_dif_fun = 0.813,
    irs_dif_arab = 0.422,
    gamb_ss_Q0 = 0.92,
    gamb_ss_Q_in = 0.97,
    gamb_ss_Q_bed = 0.89,
    fun_Q0 = 0.94,
    fun_Q_in = 0.98,
    fun_Q_bed = 0.9,
    arab_Q0 = 0.71,
    arab_Q_in = 0.96,
    arab_Q_bed = 0.9
    )
}

#' Uncertainty draws
#'
#' @export
uncertainty_draws <- function(){
  0:50
}

#' RTS,S coverage options
#'
#' @export
rtss_coverage <- function(){
  c(0, 0.5, 0.8, 0.9)
}

#' Treatment options
#' 
#' Following Penny et al: 
#' In the Imperial model this was implemented as 52% access to ACT or non-ACT treatment (26% each), with efficacies of 95% and 75% respectively.
#'
#' @param act_coverage ACT coverage
#' @param non_act_coverage non ACT coverage
#'
#' @export
treatment_option <- function(act_coverage = 0.2647059, non_act_coverage = 0.2647059){
  # 0: Sulphadoxine-Pyrimethamine (SP). The front-line treatment from early to ~ mid 2000.
  # 1: Artemether/lumefantrine (AL). Tradename: coartem.
  # 2: Dihydroartemisinin/piperaquine (DHA-P/DHA-Pip/DHA-PQP)
  # 3: Amodiaquine plus sulfadoxine-pyrimethamine (SP-AQ or SP with less resistance).
  data.frame(par = c("drug_cov_0_0", "drug_cov_1_0", "drug_cov_2_0", "drug_cov_3_0"),
             val = c(non_act_coverage, act_coverage, 0, 0))
}

#' Define output variables
#' 
#' Prevalence, incidence, severe incidence and proportion in age group
#'
#' @param lower_age Vector of lower bounds for age groups
#' @param upper_age Vector of upper bounds for age groups
#'
#' @export
output_vars <- function(lower_age = 0:99, upper_age = 1:100){
  output_prev <- data.frame(type = "prev", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("prev", lower, upper, sep = "_"))
  output_inc <- data.frame(type = "clin_inc", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("inc", lower, upper, sep = "_"))
  output_sev <- data.frame(type = "sev_inc", lower =lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("sev", lower, upper, sep = "_"))
  output_prop <- data.frame(type = "prop", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("prop", lower, upper, sep = "_"))
  dplyr::bind_rows(output_prev, output_inc, output_sev, output_prop)
}


#' Define output variables for calibration runs
#' 
#' Prevalence, incidence, severe incidence and proportion in age group
#'
#' @param lower_age Vector of lower bounds for age groups
#' @param upper_age Vector of upper bounds for age groups
#'
#' @export
calibration_output_vars <- function(){
  data.frame(type = "prev", lower = 2, upper = 10) %>%
    dplyr::mutate(name = paste("prev", lower, upper, sep = "_"))
}

#' Total_M guess
#'
#' @param pfpr Prevalence 2-10
#'
#' @return A ballpark guess at total_M
#' @export
total_m_guess <- function(pfpr){
  exp(-1.3439 + 8.0916 * pfpr)
}

#' Vaccine options
#'
#' Replicate vaccine options from 2015 analysis
#' @export
vaccine_options <- function(){
  "epi_age_1 8.98 epi_age_2 8.99 epi_age_3 9 epi_age_4 27 pev1_alpha 0.77  pev1_beta 87.3  pev1_Vmax 0.90  pev1_ab_mu 6.6170  pev1_ab_boost_mu 5.9112  pev1_ab_sigma 0.832  pev1_ab_boost_sigma 0.9262  pev1_d1_mu 3.771538  pev1_d1_sigma 0.3379437  pev1_d2_mu 6.297028  pev1_d2_sigma  0.3772515  pev1_rho_mu 2.37704  pev1_rho_boost_mu 1.03098  pev1_rho_sigma 1.01076  pev1_rho_boost_sigma 1.03912"
}

#' Create a site file
#'
#' @param pfpr Prevalence 2-10
#' @param season Seanal profile
#' @export
create_site <- function(pfpr, season){
  site <- rbind(
    mlgts::site_create(
      vectors = vectors(),
      seasonality = seasonality(season),
      total_M = 1
    ),
    treatment_option()
  )
  site[site$par == "prev", 2] <- pfpr
  site[site$par == "prev_years", 2] <- 2
  return(site)
}

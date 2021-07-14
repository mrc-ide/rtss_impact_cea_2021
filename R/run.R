#' Run the model
#'
#' @param pfpr PRevalence band 2-10
#' @param rtss_coverage Vaccine coverage
#' @param draw Parameter draw
#' @param season Seasonality
model_run <- function(pfpr, rtss_coverage, draw = 0, season = "perennial"){
  
  site <- create_site(pfpr, season)
  
  if(rtss_coverage > 0){
    rtss_options <- paste("ab_model 1 pev_epi_coverage", rtss_coverage, "pev_boost_coverage", 0.8, "pev_epi_start 5", vaccine_options())
    options <- paste("recalculate 2 num_people 100000 num_runs 1 final_run 21 itn 0 irs 0 smc 0 epi_pev 1 add", mlgts::bionomics_string(vectors()), rtss_options)
  } else {
    options <- paste("recalculate 2 num_people 100000 num_runs 1 final_run 21 itn 0 irs 0 smc 0 epi_pev 0 add", mlgts::bionomics_string(vectors()))
  }
  
  out_raw <- mlgts::launch(
    name = "run",
    draw = draw,
    site = site,
    demog = mlgts::flat_demog,
    output_vars = output_vars(),
    options = options
  )
  
  out <- out_raw$output %>%
    dplyr::mutate(
      pfpr = pfpr,
      season = season,
      draw = draw,
      rtss_coverage = rtss_coverage
    ) %>%
    year_summary(baseline_year = -4) %>%
    filter(year < 16)
  
  return(out)
}
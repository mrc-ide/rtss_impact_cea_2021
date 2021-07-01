library(dplyr)
library(ggplot2)

source("R/set_up.R")
source("R/post_processing.R")

model_runs <- readRDS("analysis/data/model_runs.rds") %>%
  select(-rtss)

# In this summary, impact is measure in year 5- 19 (inclusive)

model_run <- function(pfpr, season, draw, rtss_coverage, total_m){
  
  site <- rbind(
    mlgts::site_create(
      vectors = vectors(),
      seasonality = seasonality(season),
      total_M = 1
    ),
    treatment_option()
  )
  
  if(rtss_coverage > 0){
    rtss_options <- paste("pev_epi_coverage", rtss_coverage, "pev_boost_coverage", rtss_coverage * 0.8, "pev_epi_start 5 epi_age_1 6 epi_age_2 7.5 epi_age_3 9 epi_age_4 27")
    options <- paste("num_people 100000 num_runs 1 final_run 21 itn 0 irs 0 smc 0 epi_pev 1 total_M", total_m,  "add", mlgts::bionomics_string(vectors()), rtss_options)
  } else {
    options <- paste("num_people 100000 num_runs 1 final_run 21 itn 0 irs 0 smc 0 epi_pev 0 total_M", total_m,  "add", mlgts::bionomics_string(vectors()))
  }
  
  out <- mlgts::launch(
    name = "run",
    draw = draw,
    site = site,
    output_vars = output_vars(),
    options = options,
    demog = mlgts::generic_demog
  )
  
  out <- out$output %>%
    dplyr::mutate(
      pfpr = pfpr,
      season = season,
      draw = draw,
      rtss_coverage = rtss_coverage,
      fvp = max(num_vaccinees_boost)
    ) %>%
    # Dropping non_smooth output and intervention number output
    dplyr::select(pfpr, season, draw, rtss_coverage, year, fvp, dplyr::contains("smooth")) %>%
    # Annual summary
    year_summary(baseline_year = 0) %>%
    # Formatting
    model_output_to_long(pfpr, season, draw, rtss_coverage, fvp) %>%
    # Replace -999
    replace_missing() %>%
    mortality_rate()
  
  
  return(out)
}


test <- model_runs %>%
  filter(total_m < 50,
         draw == 0,
         season == "perennial",
         rtss_coverage %in% c(0, 0.9))

#t1 <- purrr::pmap(test[11,], model_run)[[1]]
# Run site calibration in parallel (locally)
library(furrr)
future::plan(multisession)
model_output_raw <- future_pmap(test, model_run, .options = furrr_options(seed = TRUE))

model_output <- dplyr::bind_rows(model_output_raw)

saveRDS(model_output, "analysis/data/model_output.rds")

# Calibrate sites

## Calibration of the baseline carrying capacity so that equilibrium PfPr2-10
## matches target levels.
source("R/set_up.R")

calibration_runs <- expand.grid(pfpr = pfpr_levels(), season = c("perennial", "seasonal"))

# Function to calibrate a site to a given level of baseline transmission
calibration_run <- function(pfpr, season){
  
  site <- rbind(
    mlgts::site_create(
      vectors = vectors(),
      seasonality = seasonality(season),
      total_M = 1
    ),
    treatment_option()
  )
  
  options <- paste("num_people 50000 final_run 1 itn 0 irs 0 smc 0 add", mlgts::bionomics_string(vectors()))
  
  interval <- c(0.0001,10)
  if(pfpr > 0.4){
    interval <- c(10, 200)
  }
  
  mlgts::fit_m(
    variable = "prev_2_10_smooth",
    target = pfpr,
    rows = 12,
    tolerance = 0.005, 
    extendInt = "yes", 
    interval = interval,
    maxiter = 20,
    site = site,
    output_vars = calibration_output_vars(),
    options = options
  )
}


# Run site calibration in parallel (locally)
library(furrr)
future::plan(multisession)
calibration_runs$total_m <- future_pmap_dbl(calibration_runs, calibration_run, .options = furrr_options(seed = TRUE))

# Create the set of model runs
model_runs <- expand.grid(pfpr = pfpr_levels(),
                          season = c("perennial", "seasonal"),
                          draw = uncertainty_draws(),
                          rtss_coverage = rtss_coverage()) %>%
  dplyr::left_join(calibration_runs, by = c("pfpr", "season"))

saveRDS(model_runs, "analysis/data/derived_data/model_runs.rds")

library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)

source("R/set_up.R")
source("R/run.R")

# Specify suite of runs:
model_runs <- expand_grid(pfpr = pfpr_levels(), season = "perennial", rtss_coverage = c(0, 0.9))
dim(model_runs)

# Run in parallel (locally)
library(furrr)
future::plan(multisession)
model_output_raw <- future_pmap(model_runs, model_run, .options = furrr_options(seed = TRUE))
saveRDS(model_output_raw, "analysis/data/derived_data/model_output_raw.RDS")

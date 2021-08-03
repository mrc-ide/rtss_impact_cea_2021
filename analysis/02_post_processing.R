# Post processing of model runs

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

# Load functions
source("R/post_processing.R")

# Load raw model output
model_output_raw <- readRDS("analysis/data/derived_data/model_output_raw.RDS")

# Process epidmiological output at various levels of disaggregation
epi_age_year <- map(model_output_raw, process_epi) %>%
  bind_rows() %>%
  estimate_impact() %>%
  filter(year > 0)

epi_age <- epi_age_year %>%
  aggregate_epi(pfpr, season, rtss_coverage, draw, age_lower, age_upper)

epi <- epi_age_year %>%
  aggregate_epi(pfpr, season, rtss_coverage, draw)

# Process commodity output at various levels of disaggregation
vx_tx_year <- map(model_output_raw, process_vx_tx) %>%
  bind_rows() %>%
  tx_cf() %>%
  filter(year > 0)

vx_tx <- vx_tx_year %>%
  aggregate_vx_tx(pfpr, season, draw, rtss_coverage)

# Combined epi and costing (aggregated)
impact <- epi %>%
  left_join(vx_tx, by = c("pfpr", "season", "rtss_coverage", "draw")) %>%
  add_impact_fvp() %>%
  add_costs() %>%
  extimate_icer()
  #quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost)

# Combine epi and costing (age-disaggregated)
impact_age <- epi_age %>%
  left_join(vx_tx, by = c("pfpr", "season", "rtss_coverage", "draw")) %>%
  add_impact_fvp() %>%
  add_costs() %>%
  extimate_icer()
  # quantile_epi_impact(pfpr, season, rtss_coverage, age_lower, age_upper)

# # Impact table
# impact_table1 <- epi %>%
#   left_join(vx_tx, by = c("pfpr", "season", "rtss_coverage", "draw")) %>%
#   filter(pfpr >= 0.1, pfpr <= 0.65) %>%
#   add_impact_fvp() %>%
#   add_costs() %>%
#   #add_proportion() %>%
#   extimate_icer() %>%
#   quantile_impact(season, rtss_coverage, cost_per_dose, delivery_cost) %>%
#   filter(delivery_cost == 1.62, rtss_coverage == 0.8) %>%
#   select(season, rtss_coverage, cost_per_dose, delivery_cost, 
#          cases_averted_per_100000_fvp_median,
#          cases_averted_per_100000_fvp_lower,
#          cases_averted_per_100000_fvp_upper,
#          deaths_averted_per_100000_fvp_median,
#          deaths_averted_per_100000_fvp_lower,
#          deaths_averted_per_100000_fvp_upper,
#          icer_case_median,
#          icer_case_lower,
#          icer_case_upper,
#          icer_ddaly_median,
#          icer_ddaly_lower,
#          icer_ddaly_upper)
# 
# impact_table2 <- epi_age %>%
#   filter(age_upper <= 5) %>%
#   group_by(season, rtss_coverage, draw) %>%
#   summarise(prop_cases_averted = sum(cases_averted) / sum(cases_cf),
#             prop_deaths_averted = sum(deaths_averted) / sum(deaths_cf)) %>%
#   group_by(season, rtss_coverage) %>%
#   summarise(prop_cases_averted_median = quantile(prop_cases_averted, 0.5),
#             prop_cases_averted_lower = quantile(prop_cases_averted, 0.025),
#             prop_cases_averted_upper = quantile(prop_cases_averted, 0.975),
#             prop_deaths_averted_median = quantile(prop_deaths_averted, 0.5),
#             prop_deaths_averted_lower = quantile(prop_deaths_averted, 0.025),
#             prop_deaths_averted_upper = quantile(prop_deaths_averted, 0.975))
#   
# impact_table <-  left_join(impact_table1, impact_table2) 
# 

  

### Save output ################################################################
saveRDS(impact, "analysis/data/derived_data/impact.RDS")
saveRDS(impact_age, "analysis/data/derived_data/impact_age.RDS")
################################################################################


### Checking prevalence baseline ###############################################
prev <- epi_age_year %>%
  filter(age_lower >= 2,
         age_upper <= 10) %>%
  group_by(year, pfpr, season, draw) %>%
  summarise(prev = weighted.mean(prev, prop))

baseline_check <- ggplot(prev, aes(x = year, y = prev, col = factor(pfpr), group = interaction(factor(pfpr), draw))) +
  geom_hline(aes(yintercept = pfpr, col = factor(pfpr)), lty = 2) +
  geom_line(size = 1) +
  scale_color_discrete(name = "PfPr2-10") +
  ylab("PfPr2-10") +
  xlab("Year") +
  theme_bw() +
  facet_wrap(~season)
baseline_check
################################################################################








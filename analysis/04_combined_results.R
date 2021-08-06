## Combined analysis

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

source("R/main_figure.R")
dropbox_loc <- "C:/Users/pwinskil/Dropbox (SPH Imperial College)/shared_RTSScomparison2021/"

### Main figure ###############################################################
# 80% coverage, mean delivery cost.
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.8, delivery_cost == 1.62)
load(paste0(dropbox_loc, "2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_80_4d_64.Rdata"))
figure <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/main_figure.png", figure, height = 6, width = 10)
################################################################################

### Main table #################################################################
swiss_table <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov80/summariesForReport2021/summaryTransmission.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to50, range_10to50)) %>%
  select(outcome, SwissTPH)
imperial_table <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.8, delivery_cost == 1.62) %>% 
  mutate(Imperial = paste(med_10to50, range_10to50)) %>%
  select(outcome, Imperial)
table <- left_join(swiss_table, imperial_table, by = "outcome")
write.csv(table, "analysis/combined_output/main_table.csv")
################################################################################





### Appendix figures ###########################################################
# Main figure: low coverage (50%)
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.5, delivery_cost == 1.62)
load("C:/Users/pwinskil/Dropbox (SPH Imperial College)/shared_RTSScomparison2021/2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_50_4d_40.Rdata")
figure_low_coverage <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_low_coverage.png", figure_low_coverage, height = 6, width = 10)

# Main figure: high coverage (90%)
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.9, delivery_cost == 1.62)
load("C:/Users/pwinskil/Dropbox (SPH Imperial College)/shared_RTSScomparison2021/2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_90_4d_72.Rdata")

figure_high_coverage <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_high_coverage.png", figure_high_coverage, height = 6, width = 10)
################################################################################

### Appendix table for comparison ##############################################
swiss_table_comparison <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov80/summariesForReport2021/summaryTransmission.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to65, range_10to65)) %>%
  select(outcome, SwissTPH)
imperial_table_comparison <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.8, delivery_cost == 1.62) %>% 
  mutate(Imperial = paste(med_10to65, range_10to65)) %>%
  select(outcome, Imperial)
penny_et_al_table <- read.csv("analysis/data/raw_data/penny_et_al_table2.csv")
table_comparison <- left_join(swiss_table_comparison, imperial_table_comparison, by = "outcome") %>%
  left_join(penny_et_al_table, by = "outcome")
write.csv(table_comparison, "analysis/combined_output/table_comparison.csv")
################################################################################

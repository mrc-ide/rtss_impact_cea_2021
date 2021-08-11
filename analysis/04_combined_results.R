## Combined analysis

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

source("R/main_figure.R")
source("R/post_processing.R")

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





### Annex  ####################################################################
# Main figure: low coverage (50%)
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.5, delivery_cost == 1.62)
load(paste0(dropbox_loc, "2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_50_4d_40.Rdata"))
figure_low_coverage <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_low_coverage.png", figure_low_coverage, height = 6, width = 10)

# Main table: low coverage (50%)
swiss_table_low <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov50/summariesForReport2021/summaryTransmission.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to50, range_10to50)) %>%
  select(outcome, SwissTPH)
imperial_table_low <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.5, delivery_cost == 1.62) %>% 
  mutate(Imperial = paste(med_10to50, range_10to50)) %>%
  select(outcome, Imperial)
table_low <- left_join(swiss_table_low, imperial_table_low, by = "outcome")
write.csv(table_low, "analysis/combined_output/main_table_low_coverage.csv")

# Main figure: high coverage (90%)
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.9, delivery_cost == 1.62)
load(paste0(dropbox_loc, "2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_90_4d_72.Rdata"))
figure_high_coverage <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_high_coverage.png", figure_high_coverage, height = 6, width = 10)

# Main table: high coverage (90%)
swiss_table_high <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov90/summariesForReport2021/summaryTransmission.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to50, range_10to50)) %>%
  select(outcome, SwissTPH)
imperial_table_high <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.9, delivery_cost == 1.62) %>% 
  mutate(Imperial = paste(med_10to50, range_10to50)) %>%
  select(outcome, Imperial)
table_high <- left_join(swiss_table_high, imperial_table_high, by = "outcome")
write.csv(table_high, "analysis/combined_output/main_table_high_coverage.csv")

# Main figure: min Cod
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.8, delivery_cost == 0.96)
load(paste0(dropbox_loc, "2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_80_4d_64_min.Rdata"))
figure_min_cod <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_min_cod.png", figure_min_cod, height = 6, width = 10)

# Main table: min Cod
swiss_table_min_cod <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov80/summariesForReport2021/summaryTransmission_80cov_minCOD.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to50, range_10to50)) %>%
  select(outcome, SwissTPH)
imperial_table_min_cod <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.5, delivery_cost == 0.96) %>% 
  mutate(Imperial = paste(med_10to50, range_10to50)) %>%
  select(outcome, Imperial)
table_min_cod <- left_join(swiss_table_min_cod, imperial_table_min_cod, by = "outcome")
write.csv(table_min_cod, "analysis/combined_output/main_table_min_cod.csv")

# Main figure: max Cod
imperial_impact <- readRDS("analysis/data/derived_data/impact.RDS") %>%
  filter(rtss_coverage == 0.8, delivery_cost == 2.67)
load(paste0(dropbox_loc, "2021_predictions/Data_July2021/SwissTPH/SwissTPH_transmissionCovSen_20210709_Cov_3d_80_4d_64_max.Rdata"))
figure_max_cod <- make_main_figure(imperial_impact, Data_SwissTPH)
ggsave("analysis/combined_output/figure_max_cod.png", figure_max_cod, height = 6, width = 10)

# Main table: max Cod
swiss_table_max_cod <- read.csv(paste0(dropbox_loc, "/2021_Figures_SwissTPH/report_July2021_Cov80/summariesForReport2021/summaryTransmission_80cov_maxCOD.csv")) %>%
  filter(vaccine == "with_booster", outcome != "propDeathsAverted", outcome != "severeCasesper100kvaccaverted") %>%
  mutate(SwissTPH = paste(med_10to50, range_10to50)) %>%
  select(outcome, SwissTPH)
imperial_table_max_cod <- read.csv("analysis/output/imperial_impact_estimate_2021.csv") %>%
  filter(rtss_coverage == 0.5, delivery_cost == 2.67) %>% 
  mutate(Imperial = paste(med_10to50, range_10to50)) %>%
  select(outcome, Imperial)
table_max_cod <- left_join(swiss_table_max_cod, imperial_table_max_cod, by = "outcome")
write.csv(table_max_cod, "analysis/combined_output/main_table_max_cod.csv")
################################################################################

### Appex table for comparison #################################################
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

### CE comparison table ########################################################
cov80codmean <- read.csv("analysis/combined_output/main_table.csv") %>%
  mutate(scenario = "cov80codmean")
cov50codmean <- read.csv("analysis/combined_output/main_table_low_coverage.csv") %>%
  mutate(scenario = "cov50codmean")
cov90codmean <- read.csv("analysis/combined_output/main_table_high_coverage.csv") %>%
  mutate(scenario = "cov90codmean")
cov80codmin <- read.csv("analysis/combined_output/main_table_min_cod.csv") %>%
  mutate(scenario = "cov80codmin")
cov80codmax <- read.csv("analysis/combined_output/main_table_max_cod.csv") %>%
  mutate(scenario = "cov80codmax")

ce_compare <- bind_rows(cov80codmean, cov50codmean, cov90codmean, cov80codmin, cov80codmax) %>%
  select(-X) %>%
  filter(grepl("costper", outcome)) %>%
  pivot_longer(-c(outcome, scenario), names_to = "Group", values_to =  "y") %>%
  pivot_wider(names_from = scenario, values_from = y) %>%
  arrange(Group)
write.csv(ce_compare, "analysis/combined_output/cea_comparsion.csv")


ce_compare_pd <- ce_compare %>%
  pivot_longer(-c(outcome, Group), names_to = "scenario", values_to = "y") %>%
  mutate(y = stringr::str_replace(y, " to ", " "),
         y = stringr::str_replace(y, "[(]", ""),
         y = stringr::str_replace(y, "[)]", "")) %>%
  separate(y, into = c("y", "y_lower", "y_upper"), sep = " ") %>%
  mutate(y = as.numeric(y),
         y_lower = as.numeric(y_lower),
         y_upper = as.numeric(y_upper)) %>%
  mutate(outcome2 = ifelse(grepl("daly", outcome), "DALYs", "Clinical cases"),
         level = case_when(grepl("2", outcome) ~ 2,
                           grepl("5", outcome) ~ 5,
                           grepl("10", outcome) ~ 10),
         scenario2 = case_when(
           scenario == "cov80codmean" ~ "Coverage\n 80%\n CoD\n mean",
           scenario == "cov50codmean" ~ "Coverage\n 50%\n CoD\n mean",
           scenario == "cov90codmean" ~ "Coverage\n 90%\n CoD\n mean",
           scenario == "cov80codmin" ~ "Coverage\n 80%\n CoD\n min",
           scenario == "cov80codmax" ~ "Coverage\n 80%\n CoD\n max"
         ),
         scenario2 = factor(scenario2, levels = c("Coverage\n 80%\n CoD\n mean", "Coverage\n 50%\n CoD\n mean",
                                                  "Coverage\n 90%\n CoD\n mean", "Coverage\n 80%\n CoD\n min",
                                                  "Coverage\n 80%\n CoD\n max")))

ce_compare_plot <- ggplot(ce_compare_pd, aes(x = scenario2, y = y, ymin = y_lower, ymax = y_upper, col = factor(level))) + 
  geom_linerange(position = position_dodge(width = 0.5)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(outcome2 ~ Group, scales = "free_y") +
  scale_colour_discrete(name = "Cost\nper\ndose ($)") +
  ylab("Cost ($) per event averted") +
  xlab("") + 
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'))
ggsave("analysis/combined_output/ce_compare.png", ce_compare_plot, height = 5, width = 8)
################################################################################

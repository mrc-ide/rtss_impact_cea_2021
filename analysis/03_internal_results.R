# Internal result summary

# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)

source("R/post_processing.R")
source("R/table.R")

# Load output
impact <- readRDS("analysis/data/derived_data/impact.RDS")
impact_age <- readRDS("analysis/data/derived_data/impact_age.RDS")

### Comparison with prior results ##############################################
impact_data <- impact %>%
  filter(rtss_coverage == 0.9) %>%
  quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost) %>%
  left_join(readRDS("analysis/data/raw_data/imperial_2015.rds"), by = "pfpr")

c1 <- ggplot(impact_data, aes(x = cases_averted_imperial, y = cases_averted_per_100000_fvp_median,
                              ymin = cases_averted_per_100000_fvp_lower,
                              ymax = cases_averted_per_100000_fvp_upper)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_linerange(col = "grey70") +
  geom_point(col = "darkred", size = 1) +
  xlab("Penny et al: Clinical cases averted") +
  ylab("Current: Clinical cases averted") +
  coord_fixed(xlim = c(20000, 185000), ylim = c(20000, 185000)) +
  theme_bw()

d1 <- ggplot(impact_data, aes(x = deaths_averted_imperial, y = deaths_averted_per_100000_fvp_median,
                              ymin = deaths_averted_per_100000_fvp_lower,
                              ymax = deaths_averted_per_100000_fvp_upper)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_linerange(col = "grey70") +
  geom_point(col = "darkblue", size = 1) +
  xlab("Penny et al: Deaths averted") +
  ylab("Current: Deaths averted") +
  coord_fixed(xlim = c(100, 600), ylim = c(100, 600)) +
  theme_bw()

compare_plot <- (c1 | d1)
ggsave("analysis/plots/Comparison.png", compare_plot, width = 6, height = 3)
################################################################################

### Impact by transmission level, replicating figure 2 #########################
figure_2_pd <- impact %>%
  filter(rtss_coverage == 0.8, cost_per_dose == 5, delivery_cost == 1.62) %>%
  quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost) 


ca <- ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp_median,
                              ymin = cases_averted_per_100000_fvp_lower,
                              ymax = cases_averted_per_100000_fvp_upper)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-20000, 240000, 20000), limits = c(-20000, 240000)) +
  ylab("Clinical cases averted\n(per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

ha <- ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = hospitalisations_averted_per_100000_fvp_median,
                              ymin = hospitalisations_averted_per_100000_fvp_lower,
                              ymax = hospitalisations_averted_per_100000_fvp_upper)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-0, 3000, 500), limits = c(-100, 3000)) +
  ylab("Hospitalisations averted\n(per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

da <- ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp_median,
                              ymin = deaths_averted_per_100000_fvp_lower,
                              ymax = deaths_averted_per_100000_fvp_upper)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-100, 1200, 100), limits = c(-100, 1200)) +
  ylab("Deaths averted\n(per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

impact_plot <- ca / ha / da
ggsave("analysis/plots/impact_plot.png", impact_plot, height = 13, width = 5)
################################################################################

### Cost per case and daly averted, replicating Figure 4 #######################
figure_4_pd <- impact %>%
  filter(rtss_coverage == 0.8, delivery_cost == 1.62) %>%
  quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost) 

cp_plot <- function(x, title, ylab, ylimit, y, ymin, ymax){
  ggplot(x, aes(x = pfpr, y = {{y}}, ymin = {{ymin}}, ymax = {{ymax}})) + 
    geom_ribbon(fill = "grey70") +
    geom_line() +
    ylab(ylab) +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    ylim(0, ylimit) +
    theme_bw() +
    ggtitle(title)
}

cpc1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 600, icer_case_median, icer_case_lower, icer_case_upper)
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 5), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 600, icer_case_median, icer_case_lower, icer_case_upper)
cpc3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 10), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 600, icer_case_median, icer_case_lower, icer_case_upper)

cpd1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 3000, icer_ddaly_median, icer_ddaly_lower, icer_ddaly_upper)
cpd2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 5), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 3000, icer_ddaly_median, icer_ddaly_lower, icer_ddaly_upper)
cpd3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 10), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 3000, icer_ddaly_median, icer_ddaly_lower, icer_ddaly_upper)


cp_plots <- (cpc1 | cpc2 | cpc3) / (cpd1 | cpd2 | cpd3)
ggsave("analysis/plots/cost_per_case_daly_averted.png", cp_plots, height = 6, width = 8)
################################################################################


### Impact by age, replicating Figure S4.1 #####################################
age_pd <- impact_age %>%
  filter(age_upper <= 20, rtss_coverage == 0.8) %>%
  quantile_epi_impact(pfpr, season, rtss_coverage, age_lower, age_upper)

ca_age <- ggplot(age_pd, aes(x = age_lower, y = cases_averted_per_100000_fvp_median,
                                 ymin = cases_averted_per_100000_fvp_lower,
                                 ymax = cases_averted_per_100000_fvp_upper)) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab("Age") +
  facet_wrap(~pfpr, ncol = 5) +
  theme_bw()
da_age <- ggplot(age_pd, aes(x = age_lower, y = deaths_averted_per_100000_fvp_median,
                                 ymin = deaths_averted_per_100000_fvp_lower,
                                 ymax = deaths_averted_per_100000_fvp_upper)) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Deaths averted (per 100000 fully vaccinated children)") +
  xlab("Age") +
  facet_wrap(~pfpr, ncol = 5) +
  theme_bw()

impact_age_plot <- ca_age / da_age
ggsave("analysis/plots/impact_age_plot.png", impact_age_plot, height = 10, width = 8)
################################################################################

### Table 2 summary output #####################################################
prop_impact <- get_prop_impact(impact_age, 0.03, 0.65) %>%
  left_join(get_prop_impact(impact_age, 0.1, 0.65), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_prop_impact(impact_age, 0.1, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_prop_impact(impact_age, 0.3, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost"))

vac_impact <- get_impact(impact, 0.03, 0.65) %>%
  left_join(get_impact(impact, 0.1, 0.65), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_impact(impact, 0.1, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_impact(impact, 0.3, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost"))

icer <- get_icer(impact, 0.03, 0.65) %>%
  left_join(get_icer(impact, 0.1, 0.65), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_icer(impact, 0.1, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost")) %>%
  left_join(get_icer(impact, 0.3, 0.5), by = c("outcome", "rtss_coverage", "delivery_cost"))
table <- bind_rows(prop_impact, vac_impact, icer)
write.csv(table, "analysis/output/imperial_impact_estimate_2021.csv", row.names = FALSE)
################################################################################
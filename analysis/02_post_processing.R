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
  extimate_icer() %>%
  quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost)


################################################################################
### Diagnostic/Test outputs ####################################################
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

### Comparison with prior results ##############################################
impact_data <- impact %>%
  filter(rtss_coverage == 0.9) %>%
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
  filter(rtss_coverage == 0.8, cost_per_dose == 5, delivery_cost == 1.62)


ca <- ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp_median,
                         ymin = cases_averted_per_100000_fvp_lower,
                         ymax = cases_averted_per_100000_fvp_upper)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-20000, 240000, 20000), limits = c(-20000, 240000)) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

ha <- ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = severe_averted_per_100000_fvp_median,
                              ymin = severe_averted_per_100000_fvp_lower,
                              ymax = severe_averted_per_100000_fvp_upper)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-0, 6000, 500), limits = c(-100, 6000)) +
  ylab("Hospitalisations averted (per 100000 fully vaccinated children)") +
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
  ylab("Deaths averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

impact_plot <- ca / ha / da
ggsave("analysis/plots/impact_plot.png", impact_plot, height = 13, width = 5)
################################################################################

### Cost per case and daly averted, replicating Figure 4 #######################
figure_4_pd <- impact %>%
  filter(rtss_coverage == 0.8, delivery_cost == 1.62)

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
impact_age <- epi_age %>%
  filter(age_upper <= 20) %>%
  left_join(vx_tx, by = c("pfpr", "season", "rtss_coverage", "draw")) %>%
  add_impact_fvp() %>%
  group_by(pfpr, season, rtss_coverage, age_lower, age_upper) %>%
  summarise(
    cases_averted_per_100000_fvp_median = quantile(cases_averted_per_100000_fvp, 0.5),
    cases_averted_per_100000_fvp_lower = quantile(cases_averted_per_100000_fvp, 0.025),
    cases_averted_per_100000_fvp_upper = quantile(cases_averted_per_100000_fvp, 0.975),
    
    deaths_averted_per_100000_fvp_median = quantile(deaths_averted_per_100000_fvp, 0.5),
    deaths_averted_per_100000_fvp_lower = quantile(deaths_averted_per_100000_fvp, 0.025),
    deaths_averted_per_100000_fvp_upper = quantile(deaths_averted_per_100000_fvp, 0.975)) %>%
  filter(rtss_coverage == 0.8)

ca_age <- ggplot(impact_age, aes(x = age_lower, y = cases_averted_per_100000_fvp_median,
                                 ymin = cases_averted_per_100000_fvp_lower,
                                 ymax = cases_averted_per_100000_fvp_upper)) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab("Age") +
  facet_wrap(~pfpr, ncol = 5) +
  theme_bw()
da_age <- ggplot(impact_age, aes(x = age_lower, y = deaths_averted_per_100000_fvp_median,
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

prop_averted <- epi_age %>%
  filter(rtss_coverage == 0.8,
         age_upper <= 5) %>%
  mutate(prop_cases = cases_averted / cases_cf,
         prop_deaths = deaths_averted / deaths_cf)

pca <- round(100 * quantile(prop_averted$prop_cases, c(0.025, 0.5, 0.975)), 1)
pda <- round(100 * quantile(prop_averted$prop_deaths, c(0.025, 0.5, 0.975)), 1)

averted <- epi %>%
  left_join(vx_tx, by = c("pfpr", "season", "rtss_coverage", "draw")) %>%
  add_impact_fvp() %>%
  add_costs() %>%
  extimate_icer()  %>%
  filter(rtss_coverage == 0.8,
         delivery_cost == 1.62)

ca_s <- round(quantile(filter(averted, cost_per_dose == 5)$cases_averted_per_100000_fvp, c(0.025, 0.5, 0.975)))
da_s <- round(quantile(filter(averted, cost_per_dose == 5)$deaths_averted_per_100000_fvp, c(0.025, 0.5, 0.975)))

icer_ca_2 <- round(quantile(filter(averted, cost_per_dose == 2)$icer_case, c(0.025, 0.5, 0.975)))
icer_ca_5 <- round(quantile(filter(averted, cost_per_dose == 5)$icer_case, c(0.025, 0.5, 0.975)))
icer_ca_10 <- round(quantile(filter(averted, cost_per_dose == 10)$icer_case, c(0.025, 0.5, 0.975)))

icer_ddalya_2 <- round(quantile(filter(averted, cost_per_dose == 2)$icer_ddaly, c(0.025, 0.5, 0.975)))
icer_ddalya_5 <- round(quantile(filter(averted, cost_per_dose == 5)$icer_ddaly, c(0.025, 0.5, 0.975)))
icer_ddalya_10 <- round(quantile(filter(averted, cost_per_dose == 10)$icer_ddaly, c(0.025, 0.5, 0.975)))

table2 <- bind_rows(pca, pda, ca_s, da_s, icer_ca_2, icer_ca_5, icer_ca_10, icer_ddalya_2, icer_ddalya_5, icer_ddalya_10) %>%
  mutate(output = c("Proportion of clinical cases averted in children under 5 years",
                  "Proportion of deaths averted in children under 5 years",
                  "Clinical cases averted per 100,000 FVP",
                  "Deaths averted per 100,000 FVP",
                  "ICER per clinical case averted ($2 per dose)",
                  "ICER per clinical case averted ($5 per dose)",
                  "ICER per clinical case averted ($10 per dose)",
                  "ICER per DALY averted ($2 per dose)",
                  "ICER per DALY averted ($5 per dose)",
                  "ICER per DALY averted ($10 per dose)"),
         `Imperial estimate (95% CrI)` = paste0(`50%`, " (", `2.5%`, ", ", `97.5%`, ")")) %>%
  select(output, `Imperial estimate (95% CrI)`)

write.csv(table2, "analysis/output/imperial_impact_estimate_2021.csv", row.names = FALSE)
################################################################################
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
  aggregate_epi(pfpr, season, draw, age_lower, age_upper)

epi <- epi_age_year %>%
  aggregate_epi(pfpr, season, draw)

# Process commodity output at various levels of disaggregation
vx_tx_year <- map(model_output_raw, process_vx_tx) %>%
  bind_rows() %>%
  tx_cf() %>%
  filter(year > 0)

vx_tx <- vx_tx_year %>%
  aggregate_vx_tx(pfpr, season, draw, rtss_coverage)


# Combined epi and costing (aggregated)
impact <- epi %>%
  left_join(vx_tx, by = c("pfpr", "season", "draw")) %>%
  mutate(
    cases_averted_per_100000_fvp = 100000 * (cases_averted / num_vaccinees),
    deaths_averted_per_100000_fvp = 100000 * (deaths_averted / num_vaccinees)
  ) %>%
  add_costs() %>%
  extimate_icer()




################################################################################
### Diagnostic/Test outputs ####################################################
################################################################################

### Checking prevalence baseline ###############################################
prev <- epi_age_year %>%
  filter(age_lower >= 2,
         age_upper <= 10) %>%
  group_by(year, pfpr, season, draw) %>%
  summarise(prev = weighted.mean(prev, prop))

baseline_check <- ggplot(prev, aes(x = year, y = prev, col = factor(pfpr))) +
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
impact_data <- epi %>%
  left_join(vx_tx, by = c("pfpr", "season", "draw")) %>%
  mutate(
    cases_averted_per_100000_fvp = 100000 * (cases_averted / num_vaccinees),
    deaths_averted_per_100000_fvp = 100000 * (deaths_averted / num_vaccinees)
  ) %>%
  left_join(readRDS("analysis/data/raw_data/imperial_2015.rds"), by = "pfpr")

c1 <- ggplot(impact_data, aes(x = cases_averted_imperial, y = cases_averted_per_100000_fvp)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(col = "darkred", size = 1) +
  xlab("Penny et al: Clinical cases averted") +
  ylab("Current: Clinical cases averted") +
  xlim(20000, 185000) + 
  ylim(20000, 185000) + 
  coord_fixed() +
  theme_bw()

d1 <- ggplot(impact_data, aes(x = deaths_averted_imperial, y = deaths_averted_per_100000_fvp)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(col = "darkblue", size = 1) +
  xlab("Penny et al: Deaths averted") +
  ylab("Current: Deaths averted") +
  xlim(100, 600) + 
  ylim(100, 600) + 
  coord_fixed() +
  theme_bw()

compare_plot <- (c1 | d1)
ggsave("analysis/plots/Comparison.png", compare_plot, width = 6, height = 3)
################################################################################

### Impact by transmission level, replicating figure 2 #########################
ca <- ggplot(impact_data, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-20000, 240000, 20000), limits = c(-20000, 240000)) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()
da <- ggplot(impact_data, aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-100, 1200, 100), limits = c(-100, 1200)) +
  ylab("Deaths averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

impact_plot <- ca / da
ggsave("analysis/plots/impact_plot.png", impact_plot, height = 10, width = 5)
################################################################################

### Impact by age, replicating Figure S4.1 #####################################
impact_age <- epi_age %>%
  filter(age_upper <= 20) %>%
  left_join(vx_tx, by = c("pfpr", "season", "draw")) %>%
  mutate(
    cases_averted_per_100000_fvp = 100000 * (cases_averted / num_vaccinees),
    deaths_averted_per_100000_fvp = 100000 * (deaths_averted / num_vaccinees)
  )

ca_age <- ggplot(impact_age, aes(x = age_lower, y = cases_averted_per_100000_fvp)) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab("Age") +
  facet_wrap(~pfpr, ncol = 5) +
  theme_bw()
da_age <- ggplot(impact_age, aes(x = age_lower, y = deaths_averted_per_100000_fvp)) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Deaths averted (per 100000 fully vaccinated children)") +
  xlab("Age") +
  facet_wrap(~pfpr, ncol = 5) +
  theme_bw()

impact_age_plot <- ca_age / da_age
ggsave("analysis/plots/impact_age_plot.png", impact_age_plot, height = 10, width = 8)
################################################################################
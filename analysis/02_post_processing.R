library(dplyr)
library(ggplot2)
library(patchwork)

source("R/post_processing.R")


model_output_raw <- readRDS("analysis/data/derived_data/model_output_raw.RDS")
# Create age and pop outputs
age <- bind_rows(lapply(model_output_raw, create_age))
pop <- bind_rows(lapply(model_output_raw, create_pop))
# Work out cases and deaths averted
age_impact <- compare_age(age)
# Work out impact wrt # vaccinated
impact <- age_impact_vaccines(age_impact, pop)

### Checking prevalence baseline ###############################################
prev <- age %>%
  filter(age_lower >= 2,
         age_upper <= 10,
         rtss_coverage == 0) %>%
  group_by(year, pfpr, season, draw, rtss_coverage) %>%
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
# Compare with Penny et al
impact_compare_data <- impact %>%
  left_join(readRDS("analysis/data/raw_data/imperial_2015.rds"))

c1 <- ggplot(impact_compare_data, aes(x = cases_averted_imperial, y = cases_averted_per_100000_fvp)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(col = "darkred", size = 1) +
  xlab("Penny et al: Clinical cases averted") +
  ylab("Current: Clinical cases averted") +
  xlim(20000, 185000) + 
  ylim(20000, 185000) + 
  coord_fixed() +
  theme_bw()

d1 <- ggplot(impact_compare_data, aes(x = deaths_averted_imperial, y = deaths_averted_per_100000_fvp)) +
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
ca <- ggplot(impact, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") +
  scale_y_continuous(labels = scales::comma, breaks = seq(-20000, 240000, 20000), limits = c(-20000, 240000)) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()
da <- ggplot(impact, aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp)) +
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
impact_age <- age_impact %>%
  filter(year > 0,
         age_upper <= 20) %>%
  left_join(pop) %>%
  mutate(
    cases_averted_per_100000_fvp = (cases_averted / num_vaccinees) * 100000,
    deaths_averted_per_100000_fvp = (deaths_averted / num_vaccinees) * 100000
  ) %>%
  group_by(pfpr, season, age_lower, age_upper) %>%
  summarise(cases_averted_per_100000_fvp = sum(cases_averted_per_100000_fvp),
            deaths_averted_per_100000_fvp = sum(deaths_averted_per_100000_fvp))

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
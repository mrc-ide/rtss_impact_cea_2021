library(dplyr)
library(ggplot2)
library(patchwork)

source("R/set_up.R")
source("R/post_processing.R")

# Load model outputs
model_output_raw <- readRDS("analysis/data/derived_data/model_output.rds") %>%
  mortality_rate()
# Isolate the counterfactual runs (no rtss)
counterfactual <- dplyr::filter(model_output_raw, rtss_coverage == 0) %>%
  select(-prev, -prop, -rtss_coverage, -fvp) %>%
  rename(inc_cf = inc,
         sev_cf = sev,
         mort_cf = mort)
# Combine counter factual and scenarios
pop <- 100000
model_output <- filter(model_output_raw, rtss_coverage > 0) %>%
  left_join(counterfactual, by = c("pfpr", "season", "draw", "year", "age_lower", "age_upper")) %>%
  mutate(cases = inc * prop * pop,
         deaths = mort * prop * pop,
         cases_cf = inc_cf * prop * pop,
         deaths_cf = mort_cf * prop * pop,
         cases_averted = cases_cf - cases,
         deaths_averted = deaths_cf - deaths)

# Estimate impact per 100,000 FVP
impact <- model_output %>%
  filter(year >=5,
         year <= 19) %>%
  group_by(pfpr, season, draw, rtss_coverage) %>%
  summarise(fvp = mean(fvp),
            cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted)) %>%
  ungroup() %>%
  mutate(cases_averted_per_100000_fvp = (cases_averted / fvp) * 100000,
         deaths_averted_per_100000_fvp = (deaths_averted / fvp) * 100000)


### Comparing current results with Penny et al #################################
imperial_2015 <- readRDS("analysis/data/raw_data/imperial_2015.rds")
comparison_data <- impact %>%
  select(pfpr, cases_averted_per_100000_fvp, deaths_averted_per_100000_fvp) %>%
  left_join(imperial_2015, by = "pfpr")

compare_cases_plot <- ggplot(comparison_data, aes(x = cases_averted_imperial, y = cases_averted_per_100000_fvp)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  geom_point() +
  xlab("Penny et al: Clinical cases averted") +
  ylab("Current: Clinical cases averted") +
  coord_fixed() +
  theme_bw()
compare_deaths_plot <- ggplot(comparison_data, aes(x = deaths_averted_imperial, y = deaths_averted_per_100000_fvp)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) + 
  geom_point() +
  xlab("Penny et al: Deaths averted") +
  ylab("Current: Deaths averted") +
  coord_fixed() +
  theme_bw()
compare_cases_plot | compare_deaths_plot
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
ggsave("analysis/plots/impact_plot.pdf", impact_plot, height = 10, width = 5)
################################################################################

### Impact by age, replicating Figure S4.1 #####################################
impact_age<- model_output %>%
  filter(year >=5, year <= 19,
         age_upper <= 20) %>%
  group_by(pfpr, season, draw, rtss_coverage, age_lower) %>%
  summarise(fvp = mean(fvp),
            cases_averted = sum(cases_averted),
            deaths_averted = sum(deaths_averted)) %>%
  ungroup() %>%
  mutate(cases_averted_per_100000_fvp = (cases_averted / fvp) * 100000,
         deaths_averted_per_100000_fvp = (deaths_averted / fvp) * 100000)

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
ggsave("analysis/plots/impact_age_plot.pdf", impact_age_plot, height = 10, width = 6)
################################################################################
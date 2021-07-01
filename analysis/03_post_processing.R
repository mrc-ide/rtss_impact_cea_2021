library(dplyr)
library(ggplot2)
library(patchwork)

source("R/set_up.R")
source("R/post_processing.R")

imperial_2015 <- data.frame(
  pfpr = pfpr_levels(),
  ca = c(21354,
  34380,
  56371,
  76293,
  92080,
  105797,
  131926,
  147711,
  157981,
  159282),
  da = c(182,
         250,
         343,
         411,
         446,
         465,
         513,
         516,
         502,
         475))

model_output <- readRDS("analysis/data/model_output.rds")


counterfactual <- dplyr::filter(model_output, rtss_coverage == 0) %>%
  select(-prev, -prop, -rtss_coverage, -fvp) %>%
  rename(inc_cf = inc,
         sev_cf = sev,
         mort_cf = mort)

model_output <- filter(model_output, rtss_coverage > 0) %>%
  left_join(counterfactual, by = c("pfpr", "season", "draw", "year", "age_lower", "age_upper"))

impact <- model_output %>%
  filter(year >=5, year <= 19) %>%
  group_by(pfpr, season, draw, rtss_coverage) %>%
  summarise(
    fvp = mean(fvp),
    cases = sum(inc * prop * 100000),
    cases_cf = sum(inc_cf * prop * 100000),
    deaths = sum(mort * prop * 100000),
    deaths_cf = sum(mort_cf * prop * 100000)
  ) %>%
  left_join(fvp) %>%
  mutate(cases_averted = (cases_cf - cases),
         cases_averted_per_fvp = cases_averted / fvp,
         cases_avertyed_per_100000_fvp = cases_averted_per_fvp * 100000,
         deaths_averted = (deaths_cf - deaths),
         deaths_averted_per_fvp = deaths_averted / fvp,
         deaths_avertyed_per_100000_fvp = deaths_averted_per_fvp * 100000)

compare <- imperial_2015 %>%
  left_join(impact)
plot(compare$cases_avertyed_per_100000_fvp ~ compare$ca)
abline(0, 1)
plot(compare$deaths_avertyed_per_100000_fvp ~ compare$da)
abline(0, 1)

ca <- ggplot(impact, aes(x = factor(pfpr * 100), y = cases_avertyed_per_100000_fvp)) + 
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") + 
  scale_y_continuous(labels = scales::comma, breaks = seq(-20000, 240000, 20000), limits = c(-20000, 240000)) +
  ylab("Clinical cases averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()
da <- ggplot(impact, aes(x = factor(pfpr * 100), y = deaths_avertyed_per_100000_fvp)) + 
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", fill = "#009fc4", col = "black") + 
  scale_y_continuous(labels = scales::comma, breaks = seq(-100, 1200, 100), limits = c(-100, 1200)) +
  ylab("Deaths averted (per 100000 fully vaccinated children)") +
  xlab(expression(PfPr[2-10]~(symbol("\045")))) +
  ggtitle("Four dose schedule") +
  theme_bw()

impact_plot <- ca / da
ggsave("analysis/plots/impact_plot.pdf", impact_plot, height = 10, width = 5)

impact_age<- model_output %>%
  filter(year >=5, year <= 19,
         age_upper <= 20,
         pfpr == 0.55) %>%
  group_by(pfpr, season, draw, rtss_coverage, age_lower) %>%
  summarise(
    cases = sum(inc * prop * 100000),
    cases_cf = sum(inc_cf * prop * 100000)
  )  %>%
  left_join(fvp) %>%
  mutate(cases_averted = (cases_cf - cases),
         cases_averted_per_fvp = cases_averted / fvp,
         cases_avertyed_per_100000_fvp = cases_averted_per_fvp * 100000)

ggplot(impact_age, aes(x = age_lower, y = cases_avertyed_per_100000_fvp)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = scales::comma)

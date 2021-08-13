tornado <- function(ce_compare_pd, g, o, title, xbreak = c(-100, -50, 0, 50, 100)){
  ylabel = "Cost per DALY averted ($)"
  if(o == "Clinical cases"){
    ylabel = "Cost per clinical case averted ($)"
  }
  
  tor_pd <- ce_compare_pd %>%
    mutate(
      "Cost per dose" = case_when(
        level == 5 ~ "y",
        level == 2 ~ "lower",
        level == 10 ~ "upper"
      ),
      "Cost of delivery" = case_when(
        grepl("codmean", scenario) ~ "y",
        grepl("codmin", scenario) ~ "lower",
        grepl("codmax", scenario) ~ "upper"
      ),
      "Coverage" = case_when(
        grepl("cov80", scenario) ~ "y",
        grepl("cov50", scenario) ~ "lower",
        grepl("cov90", scenario) ~ "upper"
      )) %>%
    filter(Group == g,
           outcome2 == o) %>%
    select(`Cost per dose`, `Cost of delivery`, Coverage, y, y_lower, y_upper)
  
  tor_bl <- tor_pd %>%
    filter(`Cost per dose` == "y",
           `Cost of delivery` == "y",
           Coverage == "y") %>%
    pull(y)
  
  tor_ul <- tor_pd %>%
    mutate(y = y - tor_bl,
           y_lower = y_lower - tor_bl,
           y_upper = y_upper - tor_bl) %>%
    filter((`Cost per dose` == "y") + (`Cost of delivery` == "y") + (Coverage == "y") == 2) %>%
    pivot_longer(-c(y, y_lower, y_upper), names_to = "scenario", values_to = "level") %>%
    filter(level != "y") %>%
    mutate(scenario = factor(scenario, levels = c("Coverage", "Cost of delivery", "Cost per dose")))
  
  ggplot(tor_ul, aes(x= scenario, y = y, ymin = y_lower , ymax = y_upper , fill = level)) +
    geom_bar(position = "identity", stat = "identity") +
    geom_linerange() +
    geom_hline(yintercept = 0, lty = 2) +
    ylab(ylabel) +
    scale_fill_discrete(guide = "none") +
    scale_y_continuous(breaks = xbreak, labels = xbreak + tor_bl, limits = range(xbreak)) +
    xlab("") +
    coord_flip() +
    theme_bw() +
    ggtitle(title)
}
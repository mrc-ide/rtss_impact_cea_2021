make_main_figure <- function(imperial_impact, swisstph){
  ### Top row: Impact per 100,000 FVP ##########################################
  swiss_pd <- swisstph %>%
    filter(Vaccine == "with_booster",
           prev_2_10 %in% c("3", "5", "10", "15", "20", "25", "35", "45", "55", "65"),
           Summary %in% c("median", "Q0.025", "Q0.975"),
           age_granulation == "all_ages",
           Event %in% c("clinical_cases_averted_per_100000_vac", "deaths_averted_per_100000_vac", "hospitalised_cases_averted_per_100000_vac")) %>%
    mutate(Summary = case_when(
      Summary == "Q0.025" ~ "lower",
      Summary == "Q0.975" ~ "upper",
      TRUE ~ Summary),
      pfpr = as.numeric(prev_2_10) / 100,
      Event = case_when(
        Event == "clinical_cases_averted_per_100000_vac" ~ "cases_averted_per_100000_fvp",
        Event == "deaths_averted_per_100000_vac" ~ "deaths_averted_per_100000_fvp",
        Event == "hospitalised_cases_averted_per_100000_vac" ~ "hospitalisations_averted_per_100000_fvp",
        TRUE ~ Event))  %>%
    select(Group, pfpr, Event, Summary, cumy15) %>%
    pivot_wider(names_from = Event, values_from  = cumy15) %>%
    pivot_wider(names_from = Summary, values_from = c(
      cases_averted_per_100000_fvp,
      hospitalisations_averted_per_100000_fvp,
      deaths_averted_per_100000_fvp))

  imperial_pd <- imperial_impact %>%
    quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost) %>%
    select(pfpr,
           cases_averted_per_100000_fvp_median,
           cases_averted_per_100000_fvp_lower,
           cases_averted_per_100000_fvp_upper,
           hospitalisations_averted_per_100000_fvp_median,
           hospitalisations_averted_per_100000_fvp_lower,
           hospitalisations_averted_per_100000_fvp_upper,
           deaths_averted_per_100000_fvp_median,
           deaths_averted_per_100000_fvp_lower,
           deaths_averted_per_100000_fvp_upper
    ) %>%
    mutate(Group = "Imperial")
  
  pd <- bind_rows(swiss_pd, imperial_pd)
  
  # Cases averted per 100,000 
  ca <- ggplot(pd, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp_median,
                                ymin = cases_averted_per_100000_fvp_lower,
                                ymax = cases_averted_per_100000_fvp_upper,
                                fill = Group)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", col = "black", position = position_dodge(width = 1)) +
    geom_linerange(col = "black", position = position_dodge(width = 1)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 240000, 20000), limits = c(0, 240000)) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    ylab("Clinical cases averted\n(per 100000 fully vaccinated children)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    theme_bw()
  
  # Hospitalisations averted per 100,000 
  ha <- ggplot(pd, aes(x = factor(pfpr * 100), y = hospitalisations_averted_per_100000_fvp_median,
                                ymin = hospitalisations_averted_per_100000_fvp_lower,
                                ymax = hospitalisations_averted_per_100000_fvp_upper,
                                fill = Group)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", col = "black", position = position_dodge(width = 1)) +
    geom_linerange(col = "black", position = position_dodge(width = 1)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(-0, 6000, 500), limits = c(0, 3000)) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    ylab("Hospitalisations averted\n(per 100000 fully vaccinated children)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    theme_bw()
  
  # Deaths averted per 100,000 
  da <- ggplot(pd, aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp_median,
                                ymin = deaths_averted_per_100000_fvp_lower,
                                ymax = deaths_averted_per_100000_fvp_upper,
                                fill = Group)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity", col = "black", position = position_dodge(width = 1)) +
    geom_linerange(col = "black", position = position_dodge(width = 1)) +
    scale_y_continuous(labels = scales::comma, breaks = seq(0, 1200, 100), limits = c(0, 1200)) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    ylab("Deaths averted\n(per 100000 fully vaccinated children)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    theme_bw()
  ##############################################################################
  
  ### Bottom row: ICERs ########################################################
  swiss_icer_pd <- swisstph %>%
    rename(cost_per_dose = Vaccine_price) %>%
    filter(Vaccine == "with_booster",
           prev_2_10 %in% c("3", "5", "10", "15", "20", "25", "35", "45", "55", "65"),
           Summary %in% c("median", "Q0.025", "Q0.975"),
           age_granulation == "all_ages",
           Event == "cost_per_DALY_averted_(undiscounted)") %>%
    mutate(Summary = case_when(
      Summary == "Q0.025" ~ "lower",
      Summary == "Q0.975" ~ "upper",
      TRUE ~ Summary),
      pfpr = as.numeric(prev_2_10) / 100,
      Event = "icer_daly") %>%
    select(Group, pfpr, cost_per_dose, Event, Summary, cumy15) %>%
    pivot_wider(names_from = Event, values_from  = cumy15) %>%
    pivot_wider(names_from = Summary, values_from = icer_daly, names_prefix = "icer_daly_")
  
  imperial_icer_pd <- imperial_impact %>%
    quantile_impact(pfpr, season, rtss_coverage, cost_per_dose, delivery_cost) %>%
    select(pfpr, cost_per_dose, icer_daly_lower, icer_daly_upper, icer_daly_median) %>%
    mutate(Group = "Imperial")
  
  icer_pd <- bind_rows(swiss_icer_pd, imperial_icer_pd)
  
  # $2 per dose
  icer2 <- ggplot(filter(icer_pd, cost_per_dose == 2), aes(x = pfpr * 100, y = icer_daly_median, ymin = icer_daly_lower, ymax = icer_daly_upper, colour = Group, fill = Group)) + 
    geom_ribbon(alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    ylab(" \nCost per DALY averted ($US)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    scale_colour_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    theme_bw() +
    coord_cartesian(ylim = c(0, 1000)) +
    ggtitle("$2")
  
  # $5 per dose
  icer5 <- ggplot(filter(icer_pd, cost_per_dose == 5), aes(x = pfpr * 100, y = icer_daly_median, ymin = icer_daly_lower, ymax = icer_daly_upper, colour = Group, fill = Group)) + 
    geom_ribbon(alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    ylab(" \nCost per DALY averted ($US)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    scale_colour_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    theme_bw() +
    coord_cartesian(ylim = c(0, 1000)) +
    ggtitle("$5")
  
  # $10 per dose
  icer10 <- ggplot(filter(icer_pd, cost_per_dose == 10), aes(x = pfpr * 100, y = icer_daly_median, ymin = icer_daly_lower, ymax = icer_daly_upper, colour = Group, fill = Group)) + 
    geom_ribbon(alpha = 0.2, colour = NA) +
    geom_line(size = 1) +
    ylab(" \nCost per DALY averted ($US)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    scale_fill_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    scale_colour_manual(values = c("#009fc4", "#c5a0c1"), name = "") +
    theme_bw() +
    coord_cartesian(ylim = c(0, 1000)) +
    ggtitle("$10")
  

  impact_plot <- (ca | ha | da) + plot_layout(guide = "collect")
  icer_plot <- (icer2 | icer5 | icer10) + plot_layout(guide = "collect")
  plot <- (impact_plot / icer_plot) + plot_annotation(tag_levels = "A")
  
  return(plot)
}
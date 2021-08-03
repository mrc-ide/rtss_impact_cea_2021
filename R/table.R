
get_prop_impact <- function(impact_age, lower_pfpr, upper_pfpr){
  label1 = paste0("med_", lower_pfpr * 100, "to", upper_pfpr * 100)
  label2 = paste0("range_", lower_pfpr * 100, "to", upper_pfpr * 100)
  
  out <- impact_age %>% 
    filter(
           age_upper <= 5,
           pfpr >= lower_pfpr,
           pfpr <= upper_pfpr) %>%
    add_proportion() %>%
    group_by(pfpr, rtss_coverage, delivery_cost) %>%
    summarise(propClinicalCasesAverted_U5 = 100 * median(prop_cases_averted),
              propDeathsAverted_U5 = 100 * median(prop_deaths_averted)) %>%
    pivot_longer(-c(pfpr, rtss_coverage, delivery_cost), names_to = "outcome", values_to = "y") %>%
    group_by(outcome, rtss_coverage, delivery_cost) %>%
    summarise(med = round(median(y), 1),
              range = paste0("(", round(min(y), 1), " to ", round(max(y), 1), ")"))
  
  colnames(out)[colnames(out) == "med"] <- label1
  colnames(out)[colnames(out) == "range"] <- label2
  return(out)
}


get_impact <- function(impact, lower_pfpr, upper_pfpr){
  label1 = paste0("med_", lower_pfpr * 100, "to", upper_pfpr * 100)
  label2 = paste0("range_", lower_pfpr * 100, "to", upper_pfpr * 100)
  
  out <- impact %>% 
    filter(
           pfpr >= lower_pfpr,
           pfpr <= upper_pfpr) %>%
    group_by(pfpr, rtss_coverage, delivery_cost) %>%
    summarise(
      deathper100kvaccaverted = median(deaths_averted_per_100000_fvp),
      severeCasesper100kvaccaverted = median(hospitalisations_averted_per_100000_fvp),
      clinicalCasesper100kvaccaverted = median(cases_averted_per_100000_fvp)
    ) %>%
    pivot_longer(-c(pfpr, rtss_coverage, delivery_cost), names_to = "outcome", values_to = "y") %>%
    group_by(outcome, rtss_coverage, delivery_cost) %>%
    summarise(med = round(median(y), 0),
              range = paste0("(", round(min(y), 0), " to ", round(max(y), 0), ")"))
  
  colnames(out)[colnames(out) == "med"] <- label1
  colnames(out)[colnames(out) == "range"] <- label2
  return(out)
}

get_icer <- function(impact, lower_pfpr, upper_pfpr){
  label1 = paste0("med_", lower_pfpr * 100, "to", upper_pfpr * 100)
  label2 = paste0("range_", lower_pfpr * 100, "to", upper_pfpr * 100)
  
  out <- impact %>% 
    filter(
           pfpr >= lower_pfpr,
           pfpr <= upper_pfpr) %>%
    group_by(pfpr, rtss_coverage, cost_per_dose, delivery_cost) %>%
    summarise(
      costperdaly = median(icer_daly),
      costperClinicalCase = median(icer_case)
    ) %>%
    pivot_longer(-c(pfpr, rtss_coverage, delivery_cost, cost_per_dose), names_to = "outcome", values_to = "y") %>%
    group_by(outcome, rtss_coverage, delivery_cost, cost_per_dose) %>%
    summarise(med = round(median(y), 0),
              range = paste0("(", round(min(y), 0), " to ", round(max(y), 0), ")")) %>%
    mutate(outcome = paste0(outcome, "_", cost_per_dose)) %>%
    select(-cost_per_dose)
  
  colnames(out)[colnames(out) == "med"] <- label1
  colnames(out)[colnames(out) == "range"] <- label2
  return(out)
}
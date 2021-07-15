# Convert old output to epi_age_year style
conversion_epi_age_year <- function(old){
  old %>%
    filter(Event %in% c("clinical_cases", "deaths", "clinical_cases_averted", "deaths_averted"),
           Summary == "median",
           Age_granularity == "fine",
           Vaccine_price == 5,
           Vaccine == "with_booster")%>%
    mutate(pfpr = Prev_2_10 / 100) %>%
    select(Group, pfpr, Event, Age_lower, Age_upper, y1:y15) %>%
  pivot_longer(-c(Group, pfpr, Event, Age_lower, Age_upper), names_to = "year", values_to = "y", names_prefix = "y") %>%
    pivot_wider(names_from = Event, values_from = y)  %>%
    rename(cases = clinical_cases,
           cases_averted = clinical_cases_averted,
           age_lower = Age_lower,
           age_upper = Age_upper) 
}

# Convert old output to vx_tx_year_style
conversion_vx_tx_year <- function(old){
  old %>%
    filter(Event %in% c("vaccinees", "num_trt", "treatments_averted"),
           Summary == "median",
           Age_granularity == "all_ages",
           Vaccine_price == 5,
           Vaccine == "with_booster")%>%
    mutate(pfpr = Prev_2_10 / 100) %>%
    select(Group, pfpr, Event, y1:y15) %>%
    pivot_longer(-c(Group, pfpr, Event), names_to = "year", values_to = "y", names_prefix = "y") %>%
    pivot_wider(names_from = Event, values_from = y)
}
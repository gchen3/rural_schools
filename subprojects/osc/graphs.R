###Load finance_summary data and generate graphs
source(here::here("r", "libraries.r"))
#source(here::here("subprojects", "osc", "OSC_data_clean.r"))
load(here::here("data", "finance_summary.RData"))

# Generate graphs over time
join_data <- join_data |>
  mutate(across(c(retirement_benefits, health_costs, debt_service, capital_costs), ~ . / expenditures, .names = "{.col}_perbudget")) |>
  mutate(across(c(retirement_benefits, health_costs, debt_service, capital_costs), ~ if_else(salaries != 0, . / salaries, NA), .names = "{.col}_persalary")) 

join_data_year <- join_data |>
  group_by(calendar_year) |>
  summarize(across(c(retirement_benefits, health_costs,debt_service, capital_costs, 
                     retirement_benefits_perbudget, health_costs_perbudget, debt_service_perbudget, capital_costs_perbudget,
                     retirement_benefits_persalary, health_costs_persalary, debt_service_persalary, capital_costs_persalary
  ), ~ mean(.x, na.rm = TRUE), .names="{col}_mean")) 

time_trends <- ggplot(join_data_year, aes(x = calendar_year)) +
  geom_point(aes(y = retirement_benefits_mean, color = "Retirement Benefits")) +
  geom_line(aes(y = retirement_benefits_mean, color = "Retirement Benefits", group = 1)) +
  geom_point(aes(y = health_costs_mean, color = "Health Costs")) +
  geom_line(aes(y = health_costs_mean, color = "Health Costs", group = 2)) +
  geom_point(aes(y = debt_service_mean, color = "Debt Service")) +
  geom_line(aes(y = debt_service_mean, color = "Debt Service", group = 3)) +
  geom_point(aes(y = capital_costs_mean, color = "Capital Costs")) +
  geom_line(aes(y = capital_costs_mean, color = "Capital Costs", group = 4)) +
  ylab("Time Trends of Spending (nominal dollar)") +
  scale_y_continuous(breaks = (seq(0, 10000000, by = 1000000)), limit = c(0, 10000000), labels = scales::dollar) +
  theme_minimal()

time_trends

time_trends_perbudget <- ggplot(join_data_year, aes(x = calendar_year)) +
  geom_point(aes(y = retirement_benefits_perbudget_mean, color = "Retirement Benefits")) +
  geom_line(aes(y = retirement_benefits_perbudget_mean, color = "Retirement Benefits", group = 1)) +
  geom_point(aes(y = health_costs_perbudget_mean, color = "Health Costs")) +
  geom_line(aes(y = health_costs_perbudget_mean, color = "Health Costs", group = 2)) +
  geom_point(aes(y = debt_service_perbudget_mean, color = "Debt Service")) +
  geom_line(aes(y = debt_service_perbudget_mean, color = "Debt Service", group = 3)) +
  geom_point(aes(y = capital_costs_perbudget_mean, color = "Capital Costs")) +
  geom_line(aes(y = capital_costs_perbudget_mean, color = "Capital Costs", group = 4)) +
  ylab("Time Trends of Spending (% of expenditures)") +
  scale_y_continuous(breaks = (seq(0, 0.15, by = 0.02)), limit = c(0, 0.15), labels = scales::percent) +
  theme_minimal()

time_trends_perbudget

time_trends_persalary <- ggplot(join_data_year, aes(x = calendar_year)) +
  geom_point(aes(y = retirement_benefits_persalary_mean, color = "Retirement Benefits")) +
  geom_line(aes(y = retirement_benefits_persalary_mean, color = "Retirement Benefits", group = 1)) +
  geom_point(aes(y = health_costs_persalary_mean, color = "Health Costs")) +
  geom_line(aes(y = health_costs_persalary_mean, color = "Health Costs", group = 2)) +
  geom_point(aes(y = debt_service_persalary_mean, color = "Debt Service")) +
  geom_line(aes(y = debt_service_persalary_mean, color = "Debt Service", group = 3)) +
  geom_point(aes(y = capital_costs_persalary_mean, color = "Capital Costs")) +
  geom_line(aes(y = capital_costs_persalary_mean, color = "Capital Costs", group = 4)) +
  ylab("Time Trends of Spending (% fo salaries)") +
  scale_y_continuous(breaks = (seq(0, 1, by = 0.1)), limit = c(0, 1), labels = scales::percent) +
  theme_minimal()

time_trends_persalary   ##Something not right in year 1998

# Graph over time by location type

# Create a function
create_year_location_plot <- function(data, variable_prefix) {
  year_location_summary <- data %>%
    select(starts_with(variable_prefix), calendar_year, locgrp) %>%
    filter(!is.na(locgrp)) %>%
    group_by(calendar_year, locgrp) %>%
    summarize(across(starts_with(variable_prefix), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"))
  
  plot <- ggplot(year_location_summary, aes(x = calendar_year)) +
    geom_point(aes_string(y = paste0(variable_prefix, "_perbudget_mean"), color = "locgrp")) +
    geom_line(aes_string(y = paste0(variable_prefix, "_perbudget_mean"), color = "locgrp", group = "locgrp")) +
    labs(y = paste0(variable_prefix, " per Budget"), color = "Location Group") +
    theme_minimal()
  
  return(plot)
}

year_ret_location_plot <- create_year_location_plot(join_data, "retirement_benefits")
year_ret_location_plot

year_debt_location_plot <- create_year_location_plot(join_data, "debt_service")
year_debt_location_plot

year_health_costs_location_plot <- create_year_location_plot(join_data, "health_costs")
year_health_costs_location_plot

year_capital_costs_location_plot <- create_year_location_plot(join_data, "capital_costs")
year_capital_costs_location_plot

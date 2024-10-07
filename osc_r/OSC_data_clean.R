## OSC data for school districts are cleaned following these steps
## 1. Load all the cvs file and convert from long to wide, keeping only account code
## 2. Merge all 2000 to 2023 data to one single file 
## 3. Identify the account code that associate with
### a. Health benefits
### b. Retirement benefits
### c. Debt service costs
### d. Capital project maintenance costs
## 4. Generate descriptive tables and figures over time
### a. Expenditures in nominal dollars
### b. Expenditures as % of total budget
### c. Expenditures as % of instructional and non-instructional salaries
## 5. Next steps
### a. Match with SED data to identify rural vs non-rural school districts
### b. Separate costs to current costs and legacy costs

# Libraries 
source(here::here("r", "libraries.r"))

# Data location
data_path <- here::here("osc_data")

# Load data to one large list
school_data <- list()

begin_year <- 1995
end_year <-2023
for (year in begin_year:end_year) {
  filename <- paste0(year, "_SchoolDistrict.csv")
  school_data[[as.character(year)]] <- vroom(path(data_path,filename),
                                      col_types = cols(.default = col_character())) 
  #select(CALENDAR_YEAR, MUNICIPAL_CODE, ENTITY_NAME, COUNTY, ACCOUNT_CODE, AMOUNT)
  names(school_data) <- tolower(names(school_data))
  }

# Convert from long to wide #
merged_data <- bind_rows(school_data) 

merged_data <- merged_data |> mutate(across(where(is.character), tolower))
  
names(merged_data) <- tolower(names(merged_data))

save(merged_data, file = here::here("data", "osc_data.RData"))

load(here::here("data", "osc_data.RData"))

# For retirement benefits
select_retirement_data <- merged_data |>
  filter(level_2_category == "retirement - state/local" |
           level_2_category == "retirement - teacher") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(retirement_benefits = sum(amount, na.rm = TRUE))

select_retirement_data |> filter(calendar_year == 1995) |> summary()  # Check the number of observations for retirement benefits in 1995
length(unique(school_data[[as.character(1995)]]$ENTITY_NAME))         # Check the unique values in the raw data 1995

select_retirement_data |> filter(calendar_year == 2022) |> summary()
length(unique(school_data[[as.character(2022)]]$ENTITY_NAME))

# retirement_benefits_narrative <- merged_data |>   
#   filter(level_2_category == "retirement - state/local" |
#            level_2_category == "retirement - teacher") |>
#   distinct(account_code_narrative) |>
#   left_join(merged_data, 
#             by = c("account_code_narrative"= "account_code_narrative")) |>
#   select(account_code_narrative, level_1_category, level_2_category, object_of_expenditure, 
#          financial_statement, financial_statement_segment) |>
#   distinct()

# For health insurance costs
select_health_data <- merged_data |>
  filter(level_2_category == "medical insurance") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(health_costs = sum(amount, na.rm = TRUE))

select_health_data |> filter(calendar_year == 1995) |> summary()  # Check the number of observations for retirement benefits in 1995
length(unique(school_data[[as.character(1995)]]$ENTITY_NAME))  

# health_costs_narrative <- merged_data |>   
#   filter(level_2_category == "medical insurance") |>
#   distinct(account_code_narrative) |>
#   left_join(merged_data, 
#             by = c("account_code_narrative"= "account_code_narrative")) |>
#   select(account_code_narrative, level_1_category, level_2_category, object_of_expenditure, 
#          financial_statement, financial_statement_segment) |>
#   distinct()

# For debt service costs
select_debt_data <- merged_data |>
  filter(level_1_category == "debt service") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(debt_service = sum(amount, na.rm = TRUE))

select_debt_data |> filter(calendar_year == 2001) |> summary()  # Check the number of observations for retirement benefits in 1995
length(unique(school_data[[as.character(2001)]]$ENTITY_NAME)) 

debt_narrative <- merged_data |>   filter(level_1_category == "debt service") |>
                 distinct(account_code_narrative) |>
                 left_join(merged_data, 
                           by = c("account_code_narrative"= "account_code_narrative")) |>
                 select(account_code, account_code_narrative, level_1_category, level_2_category, 
                        object_of_expenditure, financial_statement, financial_statement_segment) |>
                 distinct()

# For capital maintenance costs
select_capital_data <- merged_data |>
  filter(object_of_expenditure == "equipment and capital outlay") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(capital_costs = sum(amount, na.rm = TRUE))

select_capital_data |> filter(calendar_year == 1995) |> summary()  # Check the number of observations for retirement benefits in 1995

capital_narrative <- merged_data |>   
  filter(object_of_expenditure == "equipment and capital outlay") |>
  distinct(account_code_narrative) |>
  left_join(merged_data, 
            by = c("account_code_narrative"= "account_code_narrative")) |>
  select(account_code_narrative, level_1_category, level_2_category, object_of_expenditure, 
         financial_statement, financial_statement_segment) |>
  distinct()

## Generate other financial variables

process_finance <- function(name, filter_statement, col_name) {
  results <- merged_data |>
    filter(eval(rlang::parse_expr(filter_statement))) |>
    select(calendar_year, entity_name, municipal_code, county, amount) |>
    mutate(amount=as.integer(amount)) |>
    group_by(calendar_year, entity_name, municipal_code, county) |>
    summarize(!!col_name := sum(amount, na.rm = TRUE), .groups = 'drop')
  
  assign(name, results, envir = .GlobalEnv)
}


filter_expenditure <- "financial_statement_segment == 'expenditures' |
                      financial_statement_segment == 'expenditure' | 
                      account_code_section == 'expenditure' | 
                      account_code_section == 'expenditures'"
process_finance("expenditures", filter_expenditure, "expenditures")

print(expenditures)

filter_salaries <- "account_code_narrative == 'instructional salaries' | 
                    account_code_narrative == 'noninstructional salaries' |
                    object_of_expenditure == 'instructional salaries' | 
                    object_of_expenditure == 'noninstructional salaries'"
process_finance("salaries", filter_salaries, "salaries")

print(salaries)

# Join datasets
join_data <- select_retirement_data |>
  left_join(select_debt_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_health_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_capital_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(expenditures, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(salaries, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(merge_xwalk, by = c("municipal_code" = "osc_municipal_code"))    # match municipal_code with sedcodes_locales or leaid

# Generate graphs over time
join_data <- join_data |>
  mutate(across(c(retirement_benefits, health_costs, debt_service, capital_costs), ~ . / expenditures, .names = "{.col}_perbudget")) |>
  mutate(across(c(retirement_benefits, health_costs, debt_service, capital_costs), ~ . / salaries, .names = "{.col}_persalary")) 

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

year_ret_location <- join_data %>%
  select(starts_with("retirement_benefits"), calendar_year, locgrp) %>%
  group_by(calendar_year, locgrp) %>%
  summarize(across(starts_with("retirement_benefits"), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"))

year_ret_location_plot <- ggplot(year_ret_location, aes(x = calendar_year)) +
  geom_point(aes(y = retirement_benefits_perbudget_mean, color = locgrp)) +
  geom_line(aes(y = retirement_benefits_perbudget_mean, color = locgrp, group = locgrp)) + 
  labs(y = "Retirement Benefits per Budget (Mean)", color = "Location Group") +
  theme_minimal()

year_ret_location_plot

# Create a function
create_year_location_plot <- function(data, variable_prefix) {
  year_location_summary <- data %>%
    select(starts_with(variable_prefix), calendar_year, locgrp) %>%
    group_by(calendar_year, locgrp) %>%
    summarize(across(starts_with(variable_prefix), ~ mean(.x, na.rm = TRUE), .names = "{.col}_mean"))
  
  plot <- ggplot(year_location_summary, aes(x = calendar_year)) +
    geom_point(aes_string(y = paste0(variable_prefix, "_perbudget_mean"), color = "locgrp")) +
    geom_line(aes_string(y = paste0(variable_prefix, "_perbudget_mean"), color = "locgrp", group = "locgrp")) +
    labs(y = paste0("Mean ", variable_prefix, " per Budget"), color = "Location Group") +
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

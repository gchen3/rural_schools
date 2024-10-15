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
## 5. Match and identify legacy costs
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

# retirement_benefits_narrative <- merged_data |>
#   filter(level_2_category == "retirement - state/local" |
#   level_2_category == "retirement - teacher") |>
#   distinct(account_code_narrative) |>
#   left_join(merged_data,
#                 +             by = c("account_code_narrative"= "account_code_narrative")) |>
#   select(account_code_narrative, level_1_category, level_2_category, object_of_expenditure,
#   financial_statement, financial_statement_segment) |>
#   distinct()

# For health insurance costs
select_health_data <- merged_data |>
  filter(level_2_category == "medical insurance") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(health_costs = sum(amount, na.rm = TRUE))

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


debt_narrative <- merged_data |>  filter(level_1_category == "debt service") |>
                 distinct(account_code, account_code_narrative, level_1_category, level_2_category, 
                        object_of_expenditure, financial_statement, financial_statement_segment)

# For capital maintenance costs
select_capital_data <- merged_data |>
  filter(object_of_expenditure == "equipment and capital outlay") |>
  select(calendar_year, entity_name, municipal_code, county, amount) |>
  mutate(amount=as.integer(amount)) |>
  group_by(calendar_year, entity_name, municipal_code, county) |>
  summarize(capital_costs = sum(amount, na.rm = TRUE))

capital_narrative <- merged_data |>   
  filter(object_of_expenditure == "equipment and capital outlay") |>
  distinct(account_code_narrative, level_1_category, level_2_category, object_of_expenditure, 
           financial_statement, financial_statement_segment) 

## Merge four legacy cost variables

# merge_finance <- merge_xwalk |> 
#   select(sedcode, leaid, legalname, locale, locgrp, cocode, coname, osc_municipal_code)  |>
#   left_join(select_retirement_data, by = c ("osc_municipal_code" =  "municipal_code"))  |>
#   left_join(select_health_data, by = c ("osc_municipal_code" =  "municipal_code", "calendar_year")) |>
#   left_join(select_debt_data, by = c ("osc_municipal_code" =  "municipal_code", "calendar_year")) |>
#   left_join(select_capital_data, by = c ("osc_municipal_code" =  "municipal_code", "calendar_year"))
# 
# miss_retirement <- merge_finance |> filter(is.na(retirement_benefits))
# miss_health <- merge_finance |> filter(is.na(health_costs))
# miss_debt <- merge_finance |> filter(is.na(debt_service))
# miss_capital <- merge_finance |> filter(is.na(capital_costs))

## Generate other financial variables

generate_finance <- function(name, filter_statement, col_name) {
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
generate_finance("expenditures_data", filter_expenditure, "expenditures")

print(expenditures_data)

filter_salaries <- "account_code_narrative == 'instructional salaries' | 
                    account_code_narrative == 'noninstructional salaries' |
                    object_of_expenditure == 'instructional salaries' | 
                    object_of_expenditure == 'noninstructional salaries'"
generate_finance("salaries_data", filter_salaries, "salaries")
print(salaries_data)

# Join data sets
join_data <- expenditures_data |>
  left_join(salaries, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_retirement_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_debt_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_health_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(select_capital_data, by = c("calendar_year", "entity_name", "municipal_code", "county")) |>
  left_join(merge_xwalk, by = c("municipal_code" = "osc_municipal_code"))   |>  # match municipal_code with sedcodes_locales or leaid
  select(calendar_year, municipal_code, county, expenditures, salaries, retirement_benefits, debt_service,
         health_costs, capital_costs, sedcode, sdcode, leaid, legalname, cocode, locale, localef, locgrp) |>
  arrange(county, legalname, calendar_year)

miss_osc_code <- join_data |> 
  filter(is.na(locgrp)) |>
  group_by(calendar_year) |> 
  summarise(count = n())        ##Miss values due to NYC school districts and other merges

save(join_data, file = here::here("data", "finance_summary.RData"))

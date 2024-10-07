### Merge data from SED and OSC ###
library(tidyverse)
library(readxl)
library(here)

filepath_oscbeds <- here("data", "osc_beds_crosswalk.xlsx")
osc_beds <- read_excel(filepath_oscbeds)[, 1:4] |>
  rename_with(~ str_replace_all(tolower(.), " ", "_"))

filepath_sedcodes <- here("data", "sedcodes_xwalk.rds")
sedcodes_xwalk <- readRDS(filepath_sedcodes) |>
  rename_with(~ str_replace_all(tolower(.), " ", "_")) 

filepath_nylocales <- here("data", "nylocales.rds")
nylocales <- readRDS(filepath_nylocales) |>
  select(leaid, name, cnty, nmcnty, locale, localef) |>
  rename(name_locales = name)

filepath_sedlocales <- here("data", "sedcodes_locales.rds")
sed_locales <- readRDS(filepath_sedlocales) 

merge_xwalk <- sed_locales |>
  left_join(osc_beds, by = c("beds" = "sdcode"))

merge_xwalk <- sed_locales |>
  left_join(osc_beds, by = c("sdcode" = "beds"))

# check the match
merge_xwalk <- merge_xwalk |>
  mutate(name_match = tolower(ncesname) == tolower(osc_municipal_name))

matched_names <- merge_xwalk |>
  filter(name_match == FALSE) |>
  select(ncesname, osc_municipal_name)

print(matched_names, n = nrow(matched_names)) # Skim check looks ok

merge_xwalk <- merge_xwalk |>
  select(-osc_municipal_name, -name_match)

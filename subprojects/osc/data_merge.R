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

sed_locales |> filter(is.na(locale))  ## Eight school districts couldn't find match

#impute location data using the same county
impute_locales <- sed_locales |> 
  filter(is.na(locale)) 

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

## Identify location ##
merge_xwalk <- merge_xwalk |>
  mutate(locale=as.integer(locale),
  locgrp=case_when(locale %in% 11:13 ~ "city",
                        locale %in% 21:23 ~ "suburban",
                        locale %in% 31:33 ~ "town",
                        locale %in% 41:43 ~ "rural",
                        TRUE ~ "no match"), 
  locgrp=case_when(sedcode == "150301040000" ~ "rural",  ##Impute using SED category https://www.p12.nysed.gov/irs/accountability/2011-12/NeedResourceCapacityIndex.pdf
                   sedcode == "151601040000" ~ "rural",  ##hand search https://www.adirondackexplorer.org/stories/a-model-school-merger-for-the-adirondacks
                   sedcode == "010622080000" ~ "suburban", ##https://nces.ed.gov/nceskids/tools/AllSchool/districtInfo.asp?SearchType=3&leaid=3618420
                   sedcode == "171001040000" ~ "rural",  ##Impute using SED category https://www.p12.nysed.gov/irs/accountability/2011-12/NeedResourceCapacityIndex.pdf
                   sedcode == "210501060000" ~ "suburban", ##Impute using SED
                   sedcode == "210502040000" ~ "rural", ##hand search https://en.wikipedia.org/wiki/Mohawk,_Herkimer_County,_New_York
                   sedcode == "271102040000" ~ "rural",  ##Impute using SED
                   sedcode == "307900010000" ~ "city",   ##in NY city
                   TRUE ~ locgrp))


merge_xwalk |> filter(is.na(locale)) ## Eight school districts couldn't find match but have been impute

merge_xwalk |> filter(locgrp == "no match") ## Eight school districts couldn't find match but have been impute

merge_xwalk |> filter(is.na(osc_municipal_code)) ## 39 school districts couldn't find match in the xwalk (sdcode does not exist)

saveRDS(merge_xwalk, here("data","merge_xwalk.rds"))


##
        
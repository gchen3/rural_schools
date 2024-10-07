
# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
dpad <- r"(E:\Gang Chen\data\cornell_pad\)"
dschools <- path(dpad, "schools")
dnysed <- r"(E:\Gang Chen\data\nysed\)"

# get locale (rural) markings ----------------------------------------------------------------
xwalk <- readRDS(here::here("data", "sedcodes_xwalk_nodups.rds")) # beds codes and nces codes
locales <- readRDS(here::here("data", "nylocales.rds"))

glimpse(xwalk)
glimpse(locales)

marked <- xwalk |> 
  left_join(locales, by="leaid") |> 
  select(sedcode, sdcode, leaid,
         legalname, ncesname=name,
         cocode, coname,
         cnty, nmcnty,
         locale, localef)
marked |> filter(is.na(ncesname))

marked |> filter(coname=="WASHINGTON")
marked |> filter(coname=="WASHINGTON") |> select(sdcode, legalname, coname, locale, localef)
saveRDS(marked, here::here("data", "sedcodes_locales.rds"))


# put locale marking on enrollment data -----------------------------------
marked <- readRDS(here::here("data", "sedcodes_locales.rds"))
enroll <- readRDS(here::here("data", "enrollwide.rds"))

glimpse(marked)
glimpse(enroll)

enroll_marked <- enroll |>
  left_join(marked |> 
              select(districtid=sdcode, leaid, legalname, cnty, nmcnty, locale, localef), 
                     by="districtid")

saveRDS(enroll_marked, here::here("data", "enroll_marked.rds"))



# investigate -------------------------------------------------------------

df <- readRDS(here::here("data", "enroll_marked.rds"))
count(df, locale, localef)
count(df |> filter(year==max(year)), locale, localef)

df2 <-df |> 
  filter(!is.na(locale)) |> # remove nonmatches
  arrange(year, districtid) |> 
  group_by(year, districtid) |> 
  mutate(count=n()) |> 
  ungroup()
count(df2, count) # good - no dups

df3 <- df2 |> 
  mutate(locale=as.integer(locale),
        locgrp=case_when(locale %in% 11:13 ~ "city",
                 locale %in% 21:23 ~ "suburb",
                 locale %in% 31:33 ~ "town",
                 locale %in% 41:43 ~ "rural",
                 TRUE ~ "error"))
count(df3, locgrp, locale, localef)


df3 |> 
  filter(locgrp=="rural") |> 
  group_by(year) |>
  summarise(total=sum(total)) |> 
  ggplot(aes(year, total)) +
  geom_line() +
  geom_point() 

df3 |> 
  filter(locgrp=="rural") |> 
  group_by(year) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() 
  

df3 |>
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value / value[year==1995], colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels=scales::percent_format(accuracy = .1))

df3 |>
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA), labels=scales::comma_format())

df3 |> 
  mutate(locgrp=ifelse(districtid=="641610", "Cambridge", locgrp)) |> 
  filter(locgrp %in% c("rural", "Cambridge")) |> 
  group_by(year, locgrp) |>
  summarise(value=median(total)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits=c(0, NA), labels=scales::comma_format())


df3 |> 
  filter(locgrp=="rural") |> 
  group_by(year) |>
  summarise(value=median(g12)) |> 
  ggplot(aes(year, value)) +
  geom_line() +
  geom_point() 

df3 |> 
  filter(locgrp=="rural") |> 
  select(year, g1, g12) |> 
  pivot_longer(-year) |> 
  group_by(year, name) |>
  summarise(value=median(value)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() 

df3 |> 
  filter(locgrp=="rural") |> 
  select(year, g1, g12) |> 
  pivot_longer(-year) |> 
  mutate(year=ifelse(name=="g12", year, year + 11)) |> 
  group_by(year, name) |>
  summarise(value=median(value)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() 

df3 |> 
  filter(locgrp=="rural") |> 
  select(year, districtid, g1, g12) |> 
  pivot_longer(-c(year, districtid)) |> 
  mutate(year=ifelse(name=="g12", year, year + 11)) |> 
  group_by(year, districtid) |> 
  pivot_wider() |> 
  mutate(ratio=g12 / g1) |> 
  group_by(year) |>
  summarise(ratio=median(ratio, na.rm=TRUE)) |> 
  ggplot(aes(year, ratio)) +
  geom_line() +
  geom_point() 


df3 |> 
  mutate(locgrp=ifelse(districtid=="641610", "Cambridge", locgrp)) |> 
  filter(locgrp %in% c("rural", "Cambridge")) |> 
  group_by(year, locgrp) |>
  summarise(value=median(g12)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 120, 10), limits=c(0, NA), labels=scales::comma_format())

df3 |> 
  filter(locgrp != "city") |> 
  group_by(year, locgrp) |>
  summarise(value=p25(g12)) |> 
  ggplot(aes(year, value, colour=locgrp)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 300, 10), limits=c(0, NA), labels=scales::comma_format())


df3 |> 
  filter(locgrp=="rural") |> 
  group_by(year, locgrp) |>
  summarise(p25=p25(g12),
            p50=p50(g12),
            p75=p75(g12)) |> 
  pivot_longer(-c(year, locgrp)) |> 
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(0, 300, 10), limits=c(0, NA), labels=scales::comma_format())



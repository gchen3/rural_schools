

# TODO --------------------------------------------------------------------

# graduation rate percentiles


# links -------------------------------------------------------------------

## NCES ----
# https://nces.ed.gov/programs/edge/Geographic/SchoolLocations

## NYSED ----
# https://www.p12.nysed.gov/irs/schoolDirectory/
# https://www.oms.nysed.gov//sedref/home.html
# https://eservices.nysed.gov/sedreports/list?id=1
# https://cognos.nysed.gov/ibmcognos/bi/v1/disp?b_action=cognosViewer&ui.tool=CognosViewer&ui.action=run&ui.object=storeID(%27iE2F9F18D05114622ABE308D0AA265CE9%27)&cv.header=false&cv.toolbar=false&run.outputFormat=HTML
# https://data.nysed.gov/downloads.php
# https://www.p12.nysed.gov/irs/statistics/enroll-n-staff/home.html
# https://www.oms.nysed.gov//sedref/documents/GRADE-Organization-DESC.pdf
# https://www.p12.nysed.gov/irs/pmf/
# https://www.p12.nysed.gov/irs/pmf/PersonnelMasterFileStatisticalRuns2019-20.xlsx  has needs-resource codes
# https://data.ny.gov/browse?q=state%20education%20department&sortBy=relevance
# https://data.ny.gov/Government-Finance/New-York-State-School-Aid-Beginning-School-Year-19/9pb8-dg53


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# locations ---------------------------------------------------------------
dnysed <- r"(E:\data\nysed\)"


# id crosswalk ------------------------------------------------------------
# https://eservices.nysed.gov/sedreports/list?id=1  landing page
# https://eservices.nysed.gov/sedreports/view?rpt=%2Fcontent%2Ffolder%5B%40name%3D%27NYSED+Reports%27%5D%2Ffolder%5B%40name%3D%27SEDREF%27%5D%2Ffolder%5B%40name%3D%27SEDREF+Reports+for+Public+Website%27%5D%2Freport%5B%40name%3D%27School+Districts%2C+Public+%26+Charter+Schools%3A+NCES+IDs%27%5D&format=CSV&reportId=iE0680D8ED23E4088B5F288AEE1B63E23
fn <- "School Districts, Public & Charter Schools_ NCES IDs.csv"

# character info (from Emeditor)
# 8
# U+0038
# UTF-16LE: 0x0038
# DIGIT EIGHT
# Unicode Script: Zyyy (Common)
# Unicode General Category: Nd (Decimal Number)
# File position: 1,896 bytes


# get district ids --------------------------------------------------------
# the col_names are not valid
ids1 <- vroom(path(dnysed, fn),
             col_types = cols(.default = col_character()),
             # col_names = FALSE,
             locale=locale(encoding="UTF-16LE"),
             delim="\t", trim_ws = TRUE)
names(ids1)
(vnames <- str_replace_all(names(ids1), " ", "_"))
# [1] "Institution_ID"            "Legal_Name"                "Popular_Name"              "SED_Code"                 
# [5] "SchDistofLoc_Code"         "SchDistofLoc_Description"  "County_Code"               "County_Description"       
# [9] "INST_Type_Code"            "INST_Type_Description"     "INST_Sub_Type_Code"        "INST_Sub_Type_Description"
# [13] "Active_Date"               "Inactive_Date"             "EDEN_NCES_LEA_ID"          "EDEN_LEA_Type_CODE"       
# [17] "EDEN_LEA_OP_Status_Code"   "EDEN_NCES_SCH_ID"          "EDEN_Sch_Type_Code"        "EDEN_Sch_OP_Status_Code"  

ids2 <- ids1 |> 
  setNames(vnames)
glimpse(ids2)
count(ids2, INST_Type_Code, INST_Type_Description)
# INST_Type_Code INST_Type_Description     n
# <chr>          <chr>                 <int>
#   1 10             GOVERNMENT AGENCIES      53
# 2 16             SCHOOL DISTRICTS        734
# 3 17             PUBLIC SCHOOLS         5291
# 4 18             BOCES                    38
# 5 21             CHILD NUTRITION           5

count(ids2, INST_Type_Code, INST_Type_Description, INST_Sub_Type_Code, INST_Sub_Type_Description)

count(ids2 |> filter(INST_Type_Code=="16"), INST_Type_Code, INST_Type_Description, INST_Sub_Type_Code, INST_Sub_Type_Description)
# INST_Type_Code INST_Type_Description INST_Sub_Type_Code INST_Sub_Type_Description     n
# <chr>          <chr>                 <chr>              <chr>                     <int>
#   1 16             SCHOOL DISTRICTS      1                  CITY                         89
# 2 16             SCHOOL DISTRICTS      10                 100% CONTRACT                 3
# 3 16             SCHOOL DISTRICTS      2                  UNION FREE                   62
# 4 16             SCHOOL DISTRICTS      3                  INDEPENDENT UNION FREE       86
# 5 16             SCHOOL DISTRICTS      4                  CENTRAL                     268
# 6 16             SCHOOL DISTRICTS      5                  COMMON                        8
# 7 16             SCHOOL DISTRICTS      6                  CITY CENTRAL                  7
# 8 16             SCHOOL DISTRICTS      7                  INDEPENDENT CENTRAL         195
# 9 16             SCHOOL DISTRICTS      8                  SPECIAL ACT                  13
# 10 16             SCHOOL DISTRICTS      9                  CENTRAL HIGH SCHOOL           3

# convert all-numeric codes to integers for more-convenient sorting
ids3 <- ids2 |> 
  mutate(across(c(INST_Type_Code, INST_Sub_Type_Code), as.integer))

# focus on school districts
sdids1 <- ids3 |> 
  filter(INST_Type_Code==16) |> 
  mutate(across(c(Active_Date, Inactive_Date), as.Date))

summary(sdids1)
# which codes can we drop?
count(sdids1, EDEN_LEA_Type_CODE) # keep
count(sdids1, EDEN_LEA_OP_Status_Code) # keep
count(sdids1, EDEN_NCES_SCH_ID) # all NA, drop
count(sdids1, EDEN_Sch_Type_Code)  # all NA, drop
count(sdids1, EDEN_Sch_OP_Status_Code)  # all NA, drop

sdids2 <- sdids1 |> 
  select(-c(EDEN_NCES_SCH_ID, EDEN_Sch_Type_Code, EDEN_Sch_OP_Status_Code))

glimpse(sdids2)
saveRDS(sdids2, here::here("data", "sedcodes_xwalk.rds"))

# create a no dups version
sdids3 <- readRDS(here::here("data", "sedcodes_xwalk.rds"))
sdids_nodups <- sdids3 |> 
  select(sdcode=SchDistofLoc_Code, leaid=EDEN_NCES_LEA_ID, sedcode=SED_Code, 
         legalname=Legal_Name, subtype=INST_Sub_Type_Code, subtypedesc=INST_Sub_Type_Description,
         cocode=County_Code, coname=County_Description) |> 
  filter(subtype !=8)
sum(duplicated(sdids_nodups$sdcode))
glimpse(sdids_nodups)
saveRDS(sdids_nodups, here::here("data", "sedcodes_xwalk_nodups.rds"))


## scratch area ----
count(sdids2, INST_Type_Code, INST_Type_Description)
count(sdids2, INST_Sub_Type_Code, INST_Sub_Type_Description)

dups <- sdids2 |> 
  select(bedscode=SchDistofLoc_Code, leacode=EDEN_NCES_LEA_ID, sedcode=SED_Code, 
         legalname=Legal_Name, subtype=INST_Sub_Type_Code, subtypedesc=INST_Sub_Type_Description) |> 
  group_by(bedscode) |> 
  filter(n()>1) |> 
  ungroup() |> 
  arrange(bedscode, leacode)
dups # drop subtype 8 special act


temp <- sdids2 |> 
  select(bedscode=SchDistofLoc_Code, leacode=EDEN_NCES_LEA_ID, sedcode=SED_Code, 
         legalname=Legal_Name)
anyDuplicated(temp$bedscode)  # 48
anyDuplicated(temp$leacode) # 0
summary(temp)
temp |> 
  filter(is.na(bedscode))
duplicated(temp$bedscode)
sum(duplicated(temp$bedscode))
beds_dups <- temp$bedscode[duplicated(temp$bedscode)]




dups2 <- temp |> 
  filter(bedscode %in% beds_dups) |> 
  arrange(bedscode, leacode)

# AP and similar course data ----
# https://data.nysed.gov/APIB.php?year=2019&state=yes
# https://data.nysed.gov/downloads.php
# https://data.nysed.gov/files/apib/2021/APIB21.zip  AP_IB_Course_Summary_2021
# https://data.nysed.gov/files/apib/1920/APIB20.zip
# https://data.nysed.gov/files/apib/1819/APIB19.zip
# does not appear to be available for earlier years

url <- "https://data.nysed.gov/files/apib/2021/APIB21.zip"
url <- "https://data.nysed.gov/files/apib/1920/APIB20.zip"
fpath <- path(dnysed, "apib", path_file(url))
download.file(url, fpath, mode="wb")




# Report cards ----
# https://data.nysed.gov/files/essa/20-21/SRC2021.zip
url <- "https://data.nysed.gov/files/essa/20-21/SRC2021.zip"
fpath <- path(dnysed, "rptcard", path_file(url))
download.file(url, fpath, mode="wb")


# Read all tables in the Microsoft Access database Nwind.mdb
library(Hmisc)
library(mdbr)
d <- mdb.get('Nwind.mdb')
contents(d)
for(z in d) print(contents(z))
# Just print the names of tables in the database
mdb.get('Nwind.mdb', tables=TRUE)


mdb_path <- paste0(here(), "/data/portal_db/portal_sample.accdb")
mdb_path <- r"(C:\Users\donbo\Downloads\bulk\SRC2022\SRC2022_GroupIV.laccdb)"
mdb.get(mdb_path, tables=TRUE)


# Import one table
Orders <- mdb.get('Nwind.mdb', tables='Orders')

library(odbc)
library(RODBC)

db_path <- r"(C:\Users\donbo\Downloads\bulk\SRC2022\SRC2022_GroupIV.accdb)"
con <- RODBC::odbcConnectAccess2007(db_path)
RODBC::sqlTables(con)
RODBC::sqlTables(con) |> 
  filter(TABLE_TYPE=="TABLE") |> 
  pull(TABLE_NAME)

df <- RODBC::sqlFetch(con, "Annual EM MATH", as.is=TRUE)
glimpse(df)

df |> 
  setNames(str_to_lower(names(.data)))

df1 <- df |> 
  setNames(str_to_lower(names(df))) |> 
  rename_with(function(x) str_replace(x, "%", "pct"))
glimpse(df1)

df2 <- df1 |> 
  select(instid=institution_id, 
         beds=entity_cd,
         name=entity_name,
         year, 
         test=assessment_name,
         subgroup=subgroup_name,
         count=total_count,
         score=mean_score,
         num_tested, contains("count")) |> 
  mutate(year=as.integer(year)) |> 
  mutate(across(c(score, contains("count"), num_tested), ~ as.numeric(.x))) |> 
  mutate(lev345=naz(level3_count) + naz(level4_count) + naz(level5_count),
         lev345pct=lev345 / num_tested)
tmp <- count(df2, beds, name)

cambridge <- "641610040000"

tmp2 <- df2 |> 
  # filter(str_detect(name, coll("cambridge", ignore_case=TRUE))) |> 
  filter(beds==cambridge) |> 
  filter(subgroup=="All Students")

df2 |> 
  filter(str_sub(beds, 1, 2)=="64", str_sub(beds, -4, -1)=="0000") |> 
  filter(test=="MATH3", subgroup=="All Students") |> 
  select(beds, name, year, lev345pct) |> 
  mutate(year=paste0("y", year)) |> 
  pivot_wider(names_from = year, values_from = lev345pct) |> 
  arrange(desc(y2022))


# 32 bit windows for the following
db_path <- r"(C:\Users\donbo\Downloads\bulk\pathways\Cohort Pathways 2022.mdb)"
con <- RODBC::odbcConnectAccess(db_path)
RODBC::sqlTables(con)
RODBC::sqlTables(con) |> 
  filter(TABLE_TYPE=="TABLE") |> 
  pull(TABLE_NAME)

df <- RODBC::sqlFetch(con, "Annual EM MATH", as.is=TRUE)
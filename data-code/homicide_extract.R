## Preliminaries -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

# Field positions + state codes
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

# Positions per NCHS 2024 Mortality Multiple Cause-of-Death record layout
col_positions = fwf_positions(
  start = c(19, 20, 21, 29, 55, 59, 63, 64, 65, 69, 70, 71, 75, 77, 79,
            83, 84, 85, 102, 106, 107, 144, 145, 146, 154, 163, 165, 341, 344,
            448, 450, 484, 487, 489, 806, 810, 812, 816),
  end   = c(19, 20, 22, 30, 56, 60, 63, 64, 66, 69, 70, 73, 76, 78, 80,
            83, 84, 85, 105, 106, 107, 144, 145, 149, 156, 164, 304, 342, 443,
            448, 450, 486, 488, 490, 809, 811, 815, 817),
  col_names = c(
    "record_type",
    "resident_status",
    "state_occ_fips",
    "state_res_fips",
    "state_birth_fips",
    "state_country_birth_recode",
    "education_2003",
    "education_flag",
    "month_of_death",
    "sex",
    "detail_age_type",
    "detail_age",
    "age_recode52",
    "age_recode27",
    "age_recode12",
    "place_of_death",
    "marital_status",
    "day_of_week_death",
    "data_year",
    "injury_at_work",
    "manner_of_death",
    "activity_code",
    "place_of_injury",
    "icd10_code",
    "cause_recode113",
    "n_entity_axis",
    "entity_axis_raw",
    "n_record_axis",
    "record_axis_raw",
    "race_imputation_flag",
    "race_recode6",
    "hispanic_origin",
    "hispanic_origin_race_recode",
    "race_recode40",
    "occupation_code",
    "occupation_recode",
    "industry_code",
    "industry_recode"))

# read fixed width file + filter to homicides
read_homicides = function(path) {
  read_fwf(path, col_positions = col_positions, col_types = cols(.default = "c")) |>
    filter(
      cause_recode113 %in% c("128", "129"),   # ICD-10 X85-Y09, Y87.1
      state_res_fips %in% us_states_dc)}

## read in 2024 data ----------------------------------------------------------------------

# combine US and Puerto Rico / Territories
input_dir = "data/input/MULT2024_LmtGeo_USPS"

homicides_2024 = bind_rows(
  read_homicides(file.path(input_dir, "Y2024_MortLimGeo_US_r20251210")),
  read_homicides(file.path(input_dir, "Y2024_MortLimGeo_PS_r20251209")))

nrow(homicides_2024)

# write 2024 CSV
write_csv(homicides_2024, "data/output/homicides_2024.csv")



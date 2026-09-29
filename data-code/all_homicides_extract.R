## Preliminaries -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

## File paths per year -------------------------------------------------------------------------
year_files = list(
  "2014" = list(us = "data/input/MULT2014.LmtGeo/MULT2014.USLmtGeo.txt",
                ps = "data/input/MULT2014.LmtGeo/MULT2014.PSLmtGeo.txt"),
  "2015" = list(us = "data/input/MULT2015.LmtGeo/MULT2015.USLmtGeo.txt",
                ps = "data/input/MULT2015.LmtGeo/MULT2015.PSLmtGeo.txt"),
  "2016" = list(us = "data/input/MULT2016.LmtGeo/MULT2016.USLmtGeo.txt",
                ps = "data/input/MULT2016.LmtGeo/MULT2016.PSLmtGeo.txt"),
  "2017" = list(us = "data/input/MULT2017.LimGeo/MULT2017US.LimGeo.txt",
                ps = "data/input/MULT2017.LimGeo/MULT2017PS.LimGeo.txt"),
  "2018" = list(us = "data/input/MULT2018.LimGeo/MULT2018US.LimGeo.txt",
                ps = "data/input/MULT2018.LimGeo/MULT2018PS.LimGeo.txt"),
  "2019" = list(us = "data/input/MULT2019.LimGeo/MULT2019.LimGeo.txt",
                ps = NA),                                                 # no separate territories file for 2019
  "2020" = list(us = "data/input/MULT2020.LimGeo/Mort2020US.LimGeo.txt",
                ps = "data/input/MULT2020.LimGeo/Mort2020PS.LimGeo.txt"),
  "2021" = list(us = "data/input/MULT2021.LimGeo/MORT2021US.LimGeo.txt",
                ps = "data/input/MULT2021.LimGeo/MORT2021PS.LimGeo.txt"),
  "2022" = list(us = "data/input/MULT2022.LimGeo/MULT2022us.LimGeo.txt",
                ps = "data/input/MULT2022.LimGeo/MULT2022ps.LimGeo.txt"),
  "2023" = list(us = "data/input/MULT2023.LimGeo/MULT2023US.LimGeo.txt",
                ps = "data/input/MULT2023.LimGeo/MULT2023PS.LimGeo.txt"),
  "2024" = list(us = "data/input/MULT2024_LmtGeo_USPS/Y2024_MortLimGeo_US_r20251210",
                ps = "data/input/MULT2024_LmtGeo_USPS/Y2024_MortLimGeo_PS_r20251209"))

## Check each file's record length ------------------------------------------------------------
get_record_length = function(path) {
  if (is.na(path)) return(NA_integer_)
  first_line = readLines(path, n = 1)
  nchar(first_line)}

record_lengths = map_dfr(names(year_files), function(yr) {
  tibble(
    year = yr,
    us_path = year_files[[yr]]$us,
    us_length = get_record_length(year_files[[yr]]$us),
    ps_path = year_files[[yr]]$ps,
    ps_length = get_record_length(year_files[[yr]]$ps))})

print(record_lengths, n = Inf)

## Reference lists -------------------------------------------------------------------------
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

## Field positions: two harmonized schemes ------------------------------------------------
# 2014-2020: old race (bridged) / Hispanic-origin coding, no occupation-industry
# 2021-2024: new race (single-race) / Hispanic-origin coding, occupation-industry added

col_positions_old = fwf_positions(
  start = c(19, 20, 21, 29, 55, 59, 63, 64, 65, 69, 70, 71, 75, 77, 79,
            83, 84, 85, 102, 106, 107, 144, 145, 146, 154, 163, 165, 341, 344,
            445, 447, 448, 449, 450, 484, 488, 489),
  end   = c(19, 20, 22, 30, 56, 60, 63, 64, 66, 69, 70, 73, 76, 78, 80,
            83, 84, 85, 105, 106, 107, 144, 145, 149, 156, 164, 304, 342, 443,
            446, 447, 448, 449, 450, 486, 488, 490),
  col_names = c(
    "record_type", "resident_status", "state_occ_fips", "state_res_fips",
    "state_birth_fips", "state_country_birth_recode", "education_2003",
    "education_flag", "month_of_death", "sex", "detail_age_type", "detail_age",
    "age_recode52", "age_recode27", "age_recode12", "place_of_death",
    "marital_status", "day_of_week_death", "data_year", "injury_at_work",
    "manner_of_death", "activity_code", "place_of_injury", "icd10_code",
    "cause_recode113", "n_entity_axis", "entity_axis_raw", "n_record_axis",
    "record_axis_raw", "race", "bridged_race_flag", "race_imputation_flag",
    "race_recode3", "race_recode5", "hispanic_origin",
    "hispanic_origin_race_recode_bridged", "race_recode40"))

col_positions_new = fwf_positions(
  start = c(19, 20, 21, 29, 55, 59, 63, 64, 65, 69, 70, 71, 75, 77, 79,
            83, 84, 85, 102, 106, 107, 144, 145, 146, 154, 163, 165, 341, 344,
            448, 450, 484, 487, 489, 806, 810, 812, 816),
  end   = c(19, 20, 22, 30, 56, 60, 63, 64, 66, 69, 70, 73, 76, 78, 80,
            83, 84, 85, 105, 106, 107, 144, 145, 149, 156, 164, 304, 342, 443,
            448, 450, 486, 488, 490, 809, 811, 815, 817),
  col_names = c(
    "record_type", "resident_status", "state_occ_fips", "state_res_fips",
    "state_birth_fips", "state_country_birth_recode", "education_2003",
    "education_flag", "month_of_death", "sex", "detail_age_type", "detail_age",
    "age_recode52", "age_recode27", "age_recode12", "place_of_death",
    "marital_status", "day_of_week_death", "data_year", "injury_at_work",
    "manner_of_death", "activity_code", "place_of_injury", "icd10_code",
    "cause_recode113", "n_entity_axis", "entity_axis_raw", "n_record_axis",
    "record_axis_raw", "race_imputation_flag", "race_recode6", "hispanic_origin",
    "hispanic_origin_race_recode", "race_recode40", "occupation_code",
    "occupation_recode", "industry_code", "industry_recode"))

## Read data -----------------------------------------------------------------------------------
# non US state/DC residents removed (territories also removed)
read_homicides = function(path, col_positions) {
  read_fwf(path, col_positions = col_positions, col_types = cols(.default = "c")) |>
    filter(
      cause_recode113 %in% c("128", "129"),   # ICD-10 X85-Y09, Y87.1
      state_res_fips %in% us_states_dc)}

year_files = list(
  "2014" = list(us = "data/input/MULT2014.LmtGeo/MULT2014.USLmtGeo.txt",
                ps = "data/input/MULT2014.LmtGeo/MULT2014.PSLmtGeo.txt"),
  "2015" = list(us = "data/input/MULT2015.LmtGeo/MULT2015.USLmtGeo.txt",
                ps = "data/input/MULT2015.LmtGeo/MULT2015.PSLmtGeo.txt"),
  "2016" = list(us = "data/input/MULT2016.LmtGeo/MULT2016.USLmtGeo.txt",
                ps = "data/input/MULT2016.LmtGeo/MULT2016.PSLmtGeo.txt"),
  "2017" = list(us = "data/input/MULT2017.LimGeo/MULT2017US.LimGeo.txt",
                ps = "data/input/MULT2017.LimGeo/MULT2017PS.LimGeo.txt"),
  "2018" = list(us = "data/input/MULT2018.LimGeo/MULT2018US.LimGeo.txt",
                ps = "data/input/MULT2018.LimGeo/MULT2018PS.LimGeo.txt"),
  "2019" = list(us = "data/input/MULT2019.LimGeo/MULT2019.LimGeo.txt",
                ps = NA),  # no separate territories file provided for 2019
  "2020" = list(us = "data/input/MULT2020.LimGeo/Mort2020US.LimGeo.txt",
                ps = "data/input/MULT2020.LimGeo/Mort2020PS.LimGeo.txt"),
  "2021" = list(us = "data/input/MULT2021.LimGeo/MORT2021US.LimGeo.txt",
                ps = "data/input/MULT2021.LimGeo/MORT2021PS.LimGeo.txt"),
  "2022" = list(us = "data/input/MULT2022.LimGeo/MULT2022us.LimGeo.txt",
                ps = "data/input/MULT2022.LimGeo/MULT2022ps.LimGeo.txt"),
  "2023" = list(us = "data/input/MULT2023.LimGeo/MULT2023US.LimGeo.txt",
                ps = "data/input/MULT2023.LimGeo/MULT2023PS.LimGeo.txt"),
  "2024" = list(us = "data/input/MULT2024_LmtGeo_USPS/Y2024_MortLimGeo_US_r20251210",
                ps = "data/input/MULT2024_LmtGeo_USPS/Y2024_MortLimGeo_PS_r20251209"))

## Read every year (picking the right scheme), write per-year CSVs, keep combined ------------
homicides_by_year = list()

for (yr in names(year_files)) {
  paths = year_files[[yr]]
  col_positions = if (as.integer(yr) <= 2020) col_positions_old else col_positions_new

  parts = list(read_homicides(paths$us, col_positions))
  if (!is.na(paths$ps)) parts[[2]] = read_homicides(paths$ps, col_positions)

  homicides_yr = bind_rows(parts)
  cat(yr, ":", nrow(homicides_yr), "homicide records\n")

  write_csv(homicides_yr, paste0("data/output/homicides_", yr, ".csv"))
  homicides_by_year[[yr]] = homicides_yr}

homicides_all_years = bind_rows(homicides_by_year)
nrow(homicides_all_years)

write_csv(homicides_all_years, "data/output/homicides_2014_2024.csv")

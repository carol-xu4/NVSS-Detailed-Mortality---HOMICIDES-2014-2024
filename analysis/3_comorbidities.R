## PRELIMINARIES -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, usmap)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::count)
conflicted::conflicts_prefer(dplyr::lag)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

## READ IN HOMICIDES + ACS 
homicides = read_csv("data/output/homicides_2014_2024.csv")

acs = readRDS("data/output/acs")

## ----------------------------------------------------------------------------------------
# split raw records axis into 20 separate codes + pivot longer
for (i in 1:20) {
    start_pos = (i - 1) * 5 + 1
    end_pos = start_pos + 4
    homicides[[paste0("condition_", i)]] =
        trimws(substr(homicides$record_axis_raw, start_pos, end_pos))}

homicides_conditions_long = homicides %>%
    pivot_longer(
        cols = starts_with("condition_"),
        names_to = "condition_slot",
        values_to = "condition_code") %>%
    filter(condition_code != "")

# top conditions associated with homicide 
# total 1543 ICD-10 codes
top_conditions = homicides_conditions_long %>%
    count(condition_code, sort = TRUE) %>%
    mutate(pct_of_homicides = n / nrow(homicides) * 100)

print(top_conditions, n = 30)

# de-deplicate each person
homicides = homicides %>%
    mutate(death_id = row_number())

homicides_conditions_long = homicides %>%
    pivot_longer(
        cols = starts_with("condition_"),
        names_to = "condition_slot",
        values_to = "condition_code") %>%
    filter(condition_code != "")

n_by_nativity = homicides %>%
    filter(!is.na(nativity)) %>%
    count(nativity, name = "total_homicides")

conditions_by_nativity = homicides_conditions_long %>%
    filter(!is.na(nativity)) %>%
    distinct(death_id, nativity, condition_code) %>%
    count(nativity, condition_code, sort = TRUE) %>%
    left_join(n_by_nativity, by = "nativity") %>%
    mutate(pct_of_group = n / total_homicides * 100) %>%
    arrange(nativity, desc(n))

print(conditions_by_nativity, n = 60)

# firearms
gun_by_nativity = homicides %>%
    filter(!is.na(nativity), cause_recode113 %in% c("128", "129")) %>%
    mutate(method = if_else(cause_recode113 == "128", "Firearm", "Other assault")) %>%
    group_by(nativity, method) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(nativity) %>%
    mutate(pct = n / sum(n) * 100)

print(gun_by_nativity, n = Inf)

gun_by_race_nativity = homicides %>%
    filter(!is.na(nativity)) %>%
    mutate(race_group = case_when(
        race_recode40 == "01" ~ "White",
        race_recode40 == "02" ~ "Black",
        race_recode40 == "03" ~ "AIAN",
        race_recode40 %in% c("04","05","06","07","08","09","10","11","12","13","14") ~ "Asian or Pacific Islander",
        race_recode40 >= "15" ~ "More than one race",
        .default = NA_character_)) %>%
    filter(!is.na(race_group), cause_recode113 %in% c("128", "129")) %>%
    mutate(method = if_else(cause_recode113 == "128", "Firearm", "Other assault")) %>%
    group_by(race_group, nativity, method) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(race_group, nativity) %>%
    mutate(pct = n / sum(n) * 100) %>%
    arrange(race_group, nativity, method)

print(gun_by_race_nativity, n = Inf)

gun_white_ethnicity_nativity = homicides %>%
    filter(!is.na(nativity), cause_recode113 %in% c("128", "129")) %>%
    mutate(
        hispanic_origin_num = as.integer(hispanic_origin),
        race_group = case_when(
            race_recode40 == "01" ~ "White",
            race_recode40 == "02" ~ "Black",
            race_recode40 == "03" ~ "AIAN",
            race_recode40 %in% c("04","05","06","07","08","09","10","11","12","13","14") ~ "Asian or Pacific Islander",
            race_recode40 >= "15" ~ "More than one race",
            .default = NA_character_),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:299 ~ "Hispanic",
            .default = NA_character_),
        method = if_else(cause_recode113 == "128", "Firearm", "Other assault")) %>%
    filter(race_group == "White", !is.na(ethnicity_group)) %>%
    group_by(ethnicity_group, nativity, method) %>%
    summarize(n = n(), .groups = "drop") %>%
    group_by(ethnicity_group, nativity) %>%
    mutate(pct = n / sum(n) * 100) %>%
    arrange(ethnicity_group, nativity, method)

print(gun_white_ethnicity_nativity, n = Inf)

## Preliminaries -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

# read in cleaned 2024 data
homicides_2024 = read_csv("data/output/homicides_2024.csv")

nrow(homicides_2024)

names(homicides_2024)

# native vs foreign-born variable
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

homicides_2024 = homicides_2024 %>%
    mutate(nativity = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
        .default = NA_character_))

nativity_2024 = homicides_2024 %>% 
    group_by(nativity) %>%
    summarize(n = n())
print(nativity_2024)
write_csv(nativity_2024, "results/nativity_2024.csv")

homicides_2024 = homicides_2024 %>%
    mutate(birthplace_group = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode == "CC" ~ "Canada",
        state_country_birth_recode == "MX" ~ "Mexico",
        state_country_birth_recode == "CU" ~ "Cuba",
        state_country_birth_recode == "YY" ~ "Rest of world",
        .default = NA_character_))

birthplace_2024 = homicides_2024 %>% 
    group_by(birthplace_group) %>%
    summarize(n = n())
print(birthplace_2024)

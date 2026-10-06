## PRELIMINARIES -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, usmap)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::count)
conflicted::conflicts_prefer(dplyr::lag)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

##########################################################
ddi_acs = read_ipums_ddi("data/input/usa_00034.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>% rename_with(tolower) %>%
    select(year, serial, hhwt, statefip, gq, pernum,
    perwt, sex, age, marst, race, raced, hispan, hispand, 
    bpl, bpld, citizen, occ2010)

saveRDS(acs, "data/output/acs")
###########################################################

## READ IN HOMICIDES + ACS 
homicides = read_csv("data/output/homicides_2014_2024.csv")

acs = readRDS("data/output/acs")

## HOMICDE RATES BY NATIVITY -------------------------------------------------------------
# numerator: homicides - foreign-born vs us-born
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

homicides_by_nativity = homicides %>%
    mutate(
        data_year = as.integer(data_year),
        nativity = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
            .default = NA_character_)) %>%
    filter(!is.na(nativity)) %>%
    group_by(data_year, nativity) %>%
    summarize(n = n(), .groups = "drop")

# denominator: ACS populations - foreign-born vs us-born
acs_pop_by_nativity = acs %>%
    mutate(
        year = as.integer(year),
        bpl = as.integer(bpl),
        nativity = case_when(
            bpl %in% c(1:56, 90, 99, 100, 105, 110, 115, 120) ~ "native",
            bpl %in% 150:950                                  ~ "foreign",
            .default = NA_character_)) %>%
    filter(!is.na(nativity)) %>%
    group_by(year, nativity) %>%
    summarize(pop = sum(perwt), .groups = "drop")

# homicde rates by nativity
homicide_rates_by_nativity = homicides_by_nativity %>%
    left_join(acs_pop_by_nativity, by = c("data_year" = "year", "nativity")) %>%
    mutate(rate = n / pop * 100000)

print(homicide_rates_by_nativity, n = Inf)

write_csv(homicide_rates_by_nativity, "results/homicide_rates_by_nativity_2014_2024.csv")

colors_2 = c("native" = "#3043B4", "foreign" = "#C97703")

ggplot(homicide_rates_by_nativity,aes(x = data_year, y = rate, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Homicide Rate by Nativity, 2014-2024",
    subtitle = "Homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.3_homicide_rate_by_nativity_2014_2024.png", width = 15, height = 10)

# HOMICIDE RATES BY FOREIGN BIRTHPLACE
# numerator: foreign born homicide deaths (total, canada, cuba, mexico, rest of world)
birthplace = homicides %>%
    mutate(
        data_year = as.integer(data_year),
        birthplace_group = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode == "CC" ~ "Canada",
            state_country_birth_recode == "MX" ~ "Mexico",
            state_country_birth_recode == "CU" ~ "Cuba",
            state_country_birth_recode == "YY" ~ "Rest of world",
            .default = NA_character_)) %>%
    filter(!is.na(birthplace_group)) %>%
    group_by(data_year, birthplace_group) %>%
    summarize(n = n(), .groups = "drop")

foreign_total = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    group_by(data_year) %>%
    summarize(n = sum(n)) %>%
    mutate(birthplace_group = "Foreign total")

foreign_numerator = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    bind_rows(foreign_total)

# denominator: ACS foreign-born populations (total, canada, cuba, mexico, rest of world)
acs_birthplace = acs %>%
    mutate(
        year = as.integer(year),
        bpl = as.integer(bpl),
        birthplace_group = case_when(
            bpl == 150 ~ "Canada",
            bpl == 200 ~ "Mexico",
            bpl == 250 ~ "Cuba",
            bpl %in% setdiff(150:950, c(150, 200, 250)) ~ "Rest of world",
            .default = NA_character_)) %>%
    filter(!is.na(birthplace_group)) %>%
    group_by(year, birthplace_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

acs_foreign_total = acs_birthplace %>%
    group_by(year) %>%
    summarize(pop = sum(pop)) %>%
    mutate(birthplace_group = "Foreign total")

foreign_denominator = acs_birthplace %>%
    bind_rows(acs_foreign_total) %>%
    rename(data_year = year)

# foreign-born homicide rates, by birthplace
foreign_rates = foreign_numerator %>%
    left_join(foreign_denominator, by = c("data_year", "birthplace_group")) %>%
    mutate(
        rate = n / pop * 100000,
        birthplace_group = factor(birthplace_group,
            levels = c("Foreign total", "Canada", "Cuba", "Mexico", "Rest of world")))

print(foreign_rates, n = Inf)

write_csv(foreign_rates, "results/homicide_rates_by_foreign_birthplace_2014_2024.csv")

colors_5 = c(
    "Foreign total" = "#7C756D",
    "Canada"        = "#00847E",
    "Cuba"          = "#A6192E",
    "Mexico"        = "#D4A017",
    "Rest of world" = "#7B2D8B")

linetypes_5 = c(
    "Foreign total" = "dashed",
    "Canada"        = "solid",
    "Cuba"          = "solid",
    "Mexico"        = "solid",
    "Rest of world" = "solid")

ggplot(foreign_rates,aes(x = data_year, y = rate, color = birthplace_group, linetype = birthplace_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_5) +
  scale_linetype_manual(values = linetypes_5) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  labs(
    title = "Homicide Rate by Foreign Birthplace, 2014-2024",
    subtitle = "Homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.4_homicide_rate_by_foreign_birthplace_2014_2024.png", width = 15, height = 10)

# age-adjusted rates by state --------------------------------------------------------------------
# ACS age bins to match NVSS$age_recode12
acs = acs %>%
    mutate(age = as.integer(age),
        age_group = case_when(
            age <  1                ~ "01",   
            age >= 1  & age <= 4    ~ "02",  
            age >= 5  & age <= 14   ~ "03",   
            age >= 15 & age <= 24   ~ "04", 
            age >= 25 & age <= 34   ~ "05", 
            age >= 35 & age <= 44   ~ "06", 
            age >= 45 & age <= 54   ~ "07", 
            age >= 55 & age <= 64   ~ "08",
            age >= 65 & age <= 74   ~ "09",   
            age >= 75 & age <= 84   ~ "10", 
            age >= 85               ~ "11"))  

age_group_labels = c(
    "01" = "Under 1 year",
    "02" = "1-4 years",
    "03" = "5-14 years",
    "04" = "15-24 years",
    "05" = "25-34 years",
    "06" = "35-44 years",
    "07" = "45-54 years",
    "08" = "55-64 years",
    "09" = "65-74 years",
    "10" = "75-84 years",
    "11" = "85 years and over")

# numerator: homicides by year, nativity, age group
homicides = homicides %>%
    mutate(
        data_year = as.integer(data_year),
        nativity = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
            .default = NA_character_))

homicides_age_nativity = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    group_by(data_year, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

# denominator: ACS pop by year, nativity, age group 
acs = acs %>%
    mutate(year = as.integer(year),
        bpl = as.integer(bpl),
        nativity = case_when(
            bpl %in% c(1:56, 90, 99, 100, 105, 110, 115, 120) ~ "native",
            bpl %in% 150:950                                  ~ "foreign",
            .default = NA_character_))

acs_age_nativity = acs %>%
    filter(!is.na(nativity), !is.na(age_group)) %>%
    group_by(year, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

# add year 2000 census standard weights
standard_weights = c(
    "01" = 0.013818,  # Under 1 year
    "02" = 0.055317,  # 1-4 years
    "03" = 0.145565,  # 5-14 years
    "04" = 0.138646,  # 15-24 years
    "05" = 0.135573,  # 25-34 years
    "06" = 0.162613,  # 35-44 years
    "07" = 0.134834,  # 45-54 years
    "08" = 0.087247,  # 55-64 years
    "09" = 0.066037,  # 65-74 years
    "10" = 0.044842,  # 75-84 years
    "11" = 0.015508)  # 85 years and over

# age-specific rates + age-adjusted weights
age_rates = homicides_age_nativity %>%
    left_join(
        acs_age_nativity,
        by = c("data_year" = "year", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight)

print(age_rates, n = Inf)

age_adjusted_rates = age_rates %>%
    group_by(data_year, nativity) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

print(age_adjusted_rates, n = Inf)

write_csv(age_adjusted_rates, "results/age_adjusted_homicide_rates_nativity_year.csv")

ggplot(age_adjusted_rates,aes(x = data_year, y = age_adjusted_rate, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Age-Adjusted Homicide Rate by Nativity, 2014-2024",
    subtitle = "Age-adjusted to the year 2000 U.S. standard population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.5_age_adjusted_homicide_rate_by_nativity_2014_2024.png", width = 15, height = 10)

# age adjusted, foreign born deaths by bpl
homicides = homicides %>%
    mutate(birthplace_group = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode == "CC" ~ "Canada",
        state_country_birth_recode == "MX" ~ "Mexico",
        state_country_birth_recode == "CU" ~ "Cuba",
        state_country_birth_recode == "YY" ~ "Rest of world",
        .default = NA_character_))
        
homicides_age_birthplace = homicides %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world"),
           age_recode12 != "12") %>%
    group_by(data_year, birthplace_group, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs = acs %>%
    mutate(birthplace_group = case_when(
        bpl == 150 ~ "Canada",
        bpl == 200 ~ "Mexico",
        bpl == 250 ~ "Cuba",
        bpl %in% setdiff(150:950, c(150, 200, 250)) ~ "Rest of world",
        .default = NA_character_))

acs_age_birthplace = acs %>%
    filter(!is.na(birthplace_group), !is.na(age_group)) %>%
    group_by(year, birthplace_group, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_adjusted_birthplace = homicides_age_birthplace %>%
    left_join(
        acs_age_birthplace,
        by = c("data_year" = "year", "birthplace_group", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(data_year, birthplace_group) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

homicides_age_foreign_total = homicides_age_birthplace %>%
    group_by(data_year, age_recode12) %>%
    summarize(n = sum(n), .groups = "drop")

acs_age_foreign_total = acs_age_birthplace %>%
    group_by(year, age_group) %>%
    summarize(pop = sum(pop), .groups = "drop")

age_adjusted_foreign_total = homicides_age_foreign_total %>%
    left_join(acs_age_foreign_total, by = c("data_year" = "year", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(data_year) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(birthplace_group = "Foreign total")

print(age_adjusted_foreign_total)

age_adjusted_birthplace_plot_data = bind_rows(age_adjusted_birthplace, age_adjusted_foreign_total) %>%
    mutate(birthplace_group = factor(birthplace_group,
        levels = c("Foreign total", "Canada", "Cuba", "Mexico", "Rest of world")))

ggplot(age_adjusted_birthplace_plot_data, aes(x = data_year, y = age_adjusted_rate, color = birthplace_group, linetype = birthplace_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_5) +
  scale_linetype_manual(values = linetypes_5) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  labs(
    title = "Age-Adjusted Homicide Rate by Foreign Birthplace, 2014-2024",
    subtitle = "Age-adjusted to the year 2000 U.S. standard population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.6_age_adjusted_homicide_rate_by_foreign_birthplace_2014_2024.png", width = 15, height = 10)

stacked_rates = bind_rows(
    homicide_rates_by_nativity %>%
        select(data_year, nativity, rate) %>%
        mutate(rate_type = "Crude"),
    age_adjusted_rates %>%
        rename(rate = age_adjusted_rate) %>%
        mutate(rate_type = "Age-adjusted")) %>%
    mutate(rate_type = factor(rate_type, levels = c("Crude", "Age-adjusted")))

print(stacked_rates, n = Inf)

linetypes_ratetype = c("Crude" = "dashed", "Age-adjusted" = "solid")

ggplot(stacked_rates, aes(x = data_year, y = rate, color = nativity, linetype = rate_type)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_linetype_manual(values = linetypes_ratetype) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Homicide Rate by Nativity: Crude vs. Age-Adjusted, 2014-2024",
    subtitle = "Age-adjusted to the year 2000 U.S. standard population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.7_homicide_rate_by_nativity_crude_vs_adjusted_2014_2024.png", width = 15, height = 10)

# rates by age
age_specific_rates_birthplace = homicides_age_birthplace %>%
    left_join(
        acs_age_birthplace,
        by = c("data_year" = "year", "birthplace_group", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight)

print(age_specific_rates_birthplace, n = Inf)

age_specific_rates_pooled = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    group_by(nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop") %>%
    left_join(
        acs %>%
            filter(!is.na(nativity), !is.na(age_group)) %>%
            group_by(nativity, age_group) %>%
            summarize(pop = sum(perwt), .groups = "drop"),
        by = c("nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate = n / pop * 100000,
        age_group_label = age_group_labels[age_recode12])

age_specific_rates_wide = age_specific_rates_pooled %>%
    select(age_recode12, age_group_label, nativity, rate) %>%
    pivot_wider(names_from = nativity, values_from = rate) %>%
    arrange(age_recode12)

print(age_specific_rates_wide, n = Inf)

write_csv(age_specific_rates_wide, "results/age_specific_homicide_rates_native_vs_foreign_pooled_2014_2024.csv")

age_specific_rates_pooled = age_specific_rates_pooled %>%
    mutate(age_group_label = factor(age_group_label, levels = age_group_labels))

ggplot(age_specific_rates_pooled, aes(x = age_group_label, y = rate, color = nativity, group = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA), breaks = seq(0, 14, by = 2)) +
  labs(
    title = "Homicide Rate by Age and Nativity, 2014-2024",
    subtitle = "Pooled homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.8_homicide_rate_by_age_and_nativity_pooled_2014_2024.png", width = 15, height = 10)

# rest of world bucket -- race/ethnicity breakdown
rest_of_world_race = homicides %>%
    filter(birthplace_group == "Rest of world") %>%
    mutate(race_group = case_when(
        race_recode40 == "01" ~ "White",
        race_recode40 == "02" ~ "Black",
        race_recode40 == "03" ~ "AIAN",
        race_recode40 == "04" ~ "Asian Indian",
        race_recode40 == "05" ~ "Chinese",
        race_recode40 == "06" ~ "Filipino",
        race_recode40 == "07" ~ "Japanese",
        race_recode40 == "08" ~ "Korean",
        race_recode40 == "09" ~ "Vietnamese",
        race_recode40 == "10" ~ "Other or Multiple Asian",
        race_recode40 %in% c("11","12","13","14") ~ "Pacific Islander",
        race_recode40 >= "15" ~ "More than one race",
        .default = NA_character_)) %>%
    count(race_group, sort = TRUE)

print(rest_of_world_race)

rest_of_world_ethnicity = homicides %>%
    filter(birthplace_group == "Rest of world") %>%
    mutate(
        hispanic_origin_num = as.integer(hispanic_origin),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:209 ~ "Spaniard",
            hispanic_origin_num %in% 210:219 ~ "Mexican",
            hispanic_origin_num %in% 220:230 ~ "Central American",
            hispanic_origin_num %in% 231:249 ~ "South American",
            hispanic_origin_num %in% 250:259 ~ "Latin American",
            hispanic_origin_num %in% 260:269 ~ "Puerto Rican",
            hispanic_origin_num %in% 270:274 ~ "Cuban",
            hispanic_origin_num %in% 275:279 ~ "Dominican",
            hispanic_origin_num %in% 280:299 ~ "Other Hispanic",
            hispanic_origin_num %in% 996:999 ~ "Unknown",
            .default = NA_character_)) %>%
    count(ethnicity_group, sort = TRUE)

print(rest_of_world_ethnicity)

# smell check: race breakdown by foreign birthplace subgroup 
foreign_race_breakdown = homicides %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    mutate(race_group = case_when(
        race_recode40 == "01" ~ "White",
        race_recode40 == "02" ~ "Black",
        race_recode40 == "03" ~ "AIAN",
        race_recode40 == "04" ~ "Asian Indian",
        race_recode40 == "05" ~ "Chinese",
        race_recode40 == "06" ~ "Filipino",
        race_recode40 == "07" ~ "Japanese",
        race_recode40 == "08" ~ "Korean",
        race_recode40 == "09" ~ "Vietnamese",
        race_recode40 == "10" ~ "Other or Multiple Asian",
        race_recode40 %in% c("11","12","13","14") ~ "Pacific Islander",
        race_recode40 >= "15" ~ "More than one race",
        .default = NA_character_)) %>%
    count(birthplace_group, race_group) %>%
    arrange(birthplace_group, desc(n))

print(foreign_race_breakdown, n = Inf)

# smell check: hispanic-origin ethnicity breakdown by foreign birthplace subgroup 
foreign_ethnicity_breakdown = homicides %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    mutate(
        hispanic_origin_num = as.integer(hispanic_origin),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:209 ~ "Spaniard",
            hispanic_origin_num %in% 210:219 ~ "Mexican",
            hispanic_origin_num %in% 220:230 ~ "Central American",
            hispanic_origin_num %in% 231:249 ~ "South American",
            hispanic_origin_num %in% 250:259 ~ "Latin American",
            hispanic_origin_num %in% 260:269 ~ "Puerto Rican",
            hispanic_origin_num %in% 270:274 ~ "Cuban",
            hispanic_origin_num %in% 275:279 ~ "Dominican",
            hispanic_origin_num %in% 280:299 ~ "Other Hispanic",
            hispanic_origin_num %in% 996:999 ~ "Unknown",
            .default = NA_character_)) %>%
    count(birthplace_group, ethnicity_group) %>%
    arrange(birthplace_group, desc(n))

print(foreign_ethnicity_breakdown, n = Inf)

# states -- FIPS numeric code -> USPS abbreviation 
fips_to_usps = c(
    "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
    "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
    "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
    "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
    "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
    "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
    "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
    "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
    "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
    "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
    "56" = "WY")

# numerator: pooled homicide counts by state and nativity, 2014-2024
homicides_by_state = homicides %>%
    filter(!is.na(nativity)) %>%
    group_by(state_res_fips, nativity) %>%
    summarize(n = n(), .groups = "drop")

print(homicides_by_state, n = Inf)

write_csv(homicides_by_state, "results/homicide_counts_by_state_nativity_pooled_2014_2024.csv")

# denominator: pooled ACS population by state and nativity, 2014-2024 
acs_pop_by_state = acs %>%
    filter(!is.na(nativity)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, nativity) %>%
    summarize(pop = sum(perwt), .groups = "drop")

print(acs_pop_by_state, n = Inf)

write_csv(acs_pop_by_state, "results/acs_population_by_state_nativity_pooled_2014_2024.csv")

# rates by state
homicide_rates_by_state = homicides_by_state %>%
    left_join(acs_pop_by_state, by = c("state_res_fips" = "state_usps", "nativity")) %>%
    mutate(
        rate = n / pop * 100000,
        rate = if_else(n < 10, NA_real_, rate))  # NCHS suppression: counts <10 not reportable

print(homicide_rates_by_state, n = Inf)

write_csv(homicide_rates_by_state, "results/homicide_rates_by_state_nativity_pooled_2014_2024.csv")

# smell check: 2024 rates by state, crude and age-adjusted ---------------------------------------------
homicides_by_state_2024_total = homicides %>%
    filter(data_year == 2024) %>%
    group_by(state_res_fips) %>%
    summarize(n = n(), .groups = "drop")

homicides_age_state_2024 = homicides %>%
    filter(data_year == 2024, age_recode12 != "12") %>%
    group_by(state_res_fips, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_state_2024 = acs %>%
    filter(year == 2024, !is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_adjusted_by_state_2024 = homicides_age_state_2024 %>%
    left_join(
        acs_age_state_2024,
        by = c("state_res_fips" = "state_usps", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(state_res_fips) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

crude_by_state_2024 = homicides_by_state_2024_total %>%
    left_join(
        acs %>%
            filter(year == 2024) %>%
            mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
            group_by(state_usps) %>%
            summarize(pop = sum(perwt), .groups = "drop"),
        by = c("state_res_fips" = "state_usps")) %>%
    mutate(crude_rate = n / pop * 100000)

state_rates_2024 = crude_by_state_2024 %>%
    left_join(age_adjusted_by_state_2024, by = "state_res_fips") %>%
    select(state_res_fips, n, pop, crude_rate, age_adjusted_rate) %>%
    mutate(
        crude_rate = if_else(n < 10, NA_real_, crude_rate),
        age_adjusted_rate = if_else(n < 10, NA_real_, age_adjusted_rate))

print(state_rates_2024, n = Inf)

write_csv(state_rates_2024, "results/homicide_rates_by_state_crude_and_adjusted_2024.csv")

# age adjusted rates for 2024, by state and nativity
homicides_age_state_nativity_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
    group_by(state_res_fips, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_state_nativity_2024 = acs %>%
    filter(!is.na(nativity), year == 2024, !is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_specific_state_nativity_2024 = homicides_age_state_nativity_2024 %>%
    left_join(
        acs_age_state_nativity_2024,
        by = c("state_res_fips" = "state_usps", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight)

totals_state_nativity_2024 = age_specific_state_nativity_2024 %>%
    group_by(state_res_fips, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), .groups = "drop") %>%
    mutate(crude_rate = n / pop * 100000)

age_adjusted_state_nativity_2024 = age_specific_state_nativity_2024 %>%
    group_by(state_res_fips, nativity) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

state_rates_by_nativity_2024 = totals_state_nativity_2024 %>%
    left_join(age_adjusted_state_nativity_2024, by = c("state_res_fips", "nativity")) %>%
    select(state_res_fips, nativity, n, pop, crude_rate, age_adjusted_rate)

print(state_rates_by_nativity_2024, n = Inf)

write_csv(state_rates_by_nativity_2024, "results/age_adjusted_homicide_rates_by_state_nativity_2024.csv")

# maps, 2024
foreign_rates_2024 = state_rates_by_nativity_2024 %>%
    filter(nativity == "foreign") %>%
    mutate(age_adjusted_rate = if_else(n < 10, NA_real_, age_adjusted_rate)) %>%
    select(state = state_res_fips, age_adjusted_rate)

plot_usmap(data = foreign_rates_2024, values = "age_adjusted_rate", regions = "states") +
  scale_fill_gradient(
    low = "white", high = "#C97703",
    name = "Rate per 100,000",
    na.value = "gray90") +
  labs(
    title = "Age-Adjusted Homicide Rate, Foreign-Born, 2024",
    subtitle = "Resident deaths, 50 states and DC",
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "right",
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.9_age_adjusted_homicide_rate_map_foreign_2024.png", width = 12, height = 8)

native_rates_2024 = state_rates_by_nativity_2024 %>%
    filter(nativity == "native") %>%
    mutate(age_adjusted_rate = if_else(n < 10, NA_real_, age_adjusted_rate)) %>%
    select(state = state_res_fips, age_adjusted_rate)

plot_usmap(data = native_rates_2024, values = "age_adjusted_rate", regions = "states") +
  scale_fill_gradient(
    low = "white", high = "#3043B4",
    name = "Rate per 100,000",
    na.value = "gray90") +
  labs(
    title = "Age-Adjusted Homicide Rate, Native-Born, 2024",
    subtitle = "Resident deaths, 50 states and DC",
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "right",
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.10_age_adjusted_homicide_rate_map_native_2024.png", width = 12, height = 8)

# FOREIGN-BORN POPULATION VS HOMICIDES -----------------------------------------------------------------------
# Foreign-born population by year 
foreign_pop_by_year = acs %>%
    filter(nativity == "foreign") %>%
    group_by(year) %>%
    summarize(pop = sum(perwt), .groups = "drop")

native_pop_by_year = acs %>%
    filter(nativity == "native") %>%
    group_by(year) %>%
    summarize(pop = sum(perwt), .groups = "drop")

total_pop_by_year = acs %>%
    group_by(year) %>%
    summarize(pop = sum(perwt), .groups = "drop")

pop_by_year_combined = bind_rows(
    foreign_pop_by_year %>% mutate(series = "Foreign-born"),
    native_pop_by_year %>% mutate(series = "Native-born"),
    total_pop_by_year %>% mutate(series = "Total U.S.")) %>%
    mutate(series = factor(series, levels = c("Total U.S.", "Native-born", "Foreign-born")))

colors_pop = c(
    "Total U.S."   = "grey50",
    "Native-born"  = "#3043B4",
    "Foreign-born" = "#C97703")

linetypes_pop = c(
    "Total U.S."   = "dashed",
    "Native-born"  = "solid",
    "Foreign-born" = "solid")

ggplot(pop_by_year_combined, aes(x = year, y = pop, color = series, linetype = series)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_pop) +
  scale_linetype_manual(values = linetypes_pop) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1/1e6, suffix = "M"),
    expand = c(0.02, 0), limits = c(0, 350000000), breaks = seq(0, 350000000, by = 50000000)) +
  labs(
    title = "U.S. Population by Nativity, 2014-2024",
    subtitle = "Resident population, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.11_us_population_by_nativity_2014_2024.png", width = 15, height = 10)

# foreign born share
share_foreign_by_year = foreign_pop_by_year %>%
    rename(foreign_pop = pop) %>%
    left_join(total_pop_by_year %>% rename(total_pop = pop), by = "year") %>%
    mutate(share_foreign = foreign_pop / total_pop * 100)

print(share_foreign_by_year, n = Inf)

write_csv(share_foreign_by_year, "results/foreign_born_population_share_2014_2024.csv")

# age-adjusted rates, all
homicides_age_total = homicides %>%
    filter(age_recode12 != "12") %>%
    group_by(data_year, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_total = acs %>%
    filter(!is.na(age_group)) %>%
    group_by(year, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_adjusted_rate_total_by_year = homicides_age_total %>%
    left_join(acs_age_total, by = c("data_year" = "year", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(data_year) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

print(age_adjusted_rate_total_by_year, n = Inf)

write_csv(age_adjusted_rate_total_by_year, "results/age_adjusted_homicide_rate_total_2014_2024.csv")

age_adjusted_pct_change = age_adjusted_rates %>%
    group_by(nativity) %>%
    filter(data_year %in% c(2014, 2024)) %>%
    summarize(
        rate_2014 = age_adjusted_rate[data_year == 2014],
        rate_2024 = age_adjusted_rate[data_year == 2024],
        pct_change = (rate_2024 - rate_2014) / rate_2014 * 100,
        .groups = "drop")

print(age_adjusted_pct_change)

pct_change_helper = function(data, year_col, value_col, metric_name) {
    v2014 = data[[value_col]][data[[year_col]] == 2014]
    v2024 = data[[value_col]][data[[year_col]] == 2024]
    tibble(
        metric = metric_name,
        value_2014 = v2014,
        value_2024 = v2024,
        pct_change = (v2024 - v2014) / v2014 * 100)}

total_homicides_by_year = homicides %>%
    group_by(data_year) %>%
    summarize(n = n(), .groups = "drop")

pct_change_summary = bind_rows(
    pct_change_helper(share_foreign_by_year, "year", "share_foreign", "Foreign-born population share (%)"),
    pct_change_helper(foreign_pop_by_year, "year", "pop", "Foreign-born population (count)"),
    pct_change_helper(age_adjusted_rates %>% filter(nativity == "native"), "data_year", "age_adjusted_rate", "Native-born age-adjusted rate"),
    pct_change_helper(age_adjusted_rates %>% filter(nativity == "foreign"), "data_year", "age_adjusted_rate", "Foreign-born age-adjusted rate"),
    pct_change_helper(age_adjusted_rate_total_by_year, "data_year", "age_adjusted_rate", "Total age-adjusted rate"),
    pct_change_helper(homicides_by_nativity %>% filter(nativity == "native"), "data_year", "n", "Native-born homicide count"),
    pct_change_helper(homicides_by_nativity %>% filter(nativity == "foreign"), "data_year", "n", "Foreign-born homicide count"),
    pct_change_helper(total_homicides_by_year, "data_year", "n", "Total homicide count"))

print(pct_change_summary, n = Inf)
write_csv(pct_change_summary, "results/pct_change_summary_2014_2024.csv")

## ALL RATES 2014 and 2024 ----------------------------------------------------------------------------
homicides_age_state_year_total = homicides %>%
    filter(age_recode12 != "12") %>%
    group_by(state_res_fips, data_year, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_state_year_total = acs %>%
    filter(!is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_adjusted_state_year_total = homicides_age_state_year_total %>%
    left_join(
        acs_age_state_year_total,
        by = c("state_res_fips" = "state_usps", "data_year" = "year", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(state_res_fips, data_year) %>%
    summarize(adjusted_rate_total = sum(weighted_rate), .groups = "drop")

homicides_state_year_total = homicides %>%
    group_by(state_res_fips, data_year) %>%
    summarize(n = n(), .groups = "drop")

acs_state_year_total = acs %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year) %>%
    summarize(pop = sum(perwt), .groups = "drop")

crude_state_year_total = homicides_state_year_total %>%
    left_join(acs_state_year_total, by = c("state_res_fips" = "state_usps", "data_year" = "year")) %>%
    mutate(crude_rate_total = n / pop * 100000)

adjusted_wide_nativity = age_adjusted_state_year_nativity %>%
    pivot_wider(names_from = nativity, values_from = age_adjusted_rate, names_prefix = "adjusted_rate_")

crude_wide_nativity = crude_state_year_nativity %>%
    select(state_res_fips, data_year, nativity, crude_rate) %>%
    pivot_wider(names_from = nativity, values_from = crude_rate, names_prefix = "crude_rate_")

state_rates_2014_2024 = crude_state_year_total %>%
    select(state = state_res_fips, data_year, crude_rate_total) %>%
    left_join(age_adjusted_state_year_total %>% rename(state = state_res_fips),
              by = c("state", "data_year")) %>%
    left_join(crude_wide_nativity %>% rename(state = state_res_fips),
              by = c("state", "data_year")) %>%
    left_join(adjusted_wide_nativity %>% rename(state = state_res_fips),
              by = c("state", "data_year")) %>%
    filter(data_year %in% c(2014, 2024)) %>%
    select(state, data_year,
           crude_rate_total, adjusted_rate_total,
           crude_rate_native, adjusted_rate_native,
           crude_rate_foreign, adjusted_rate_foreign) %>%
    arrange(state, data_year)

print(state_rates_2014_2024, n = Inf)

write_csv(state_rates_2014_2024, "results/state_rates_all_2014_2024.csv")

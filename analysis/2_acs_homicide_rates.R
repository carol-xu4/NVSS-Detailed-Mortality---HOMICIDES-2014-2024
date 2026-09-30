## PRELIMINARIES -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

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

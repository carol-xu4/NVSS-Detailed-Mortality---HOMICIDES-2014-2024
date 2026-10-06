## PRELIMINARIES -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, usmap, fixest)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::count)
conflicted::conflicts_prefer(dplyr::lag)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

## READ IN HOMICIDES + ACS 
homicides = read_csv("data/output/homicides_2014_2024.csv")

acs = readRDS("data/output/acs")

# -----------------------------------------------------------------------------------------
# sex
sex_nativity_year = homicides %>%
    filter(!is.na(nativity)) %>%
    mutate(data_year = as.integer(data_year)) %>%
    group_by(data_year, sex, nativity) %>%
    summarise(n = n(), .groups = "drop")

print(sex_nativity_year, n = Inf)

colors_sex = c(
    "M" = "#2C5F7C",
    "F" = "#B8657A")

linetypes_nativity = c(
    "native"  = "solid",
    "foreign" = "dashed")

ggplot(sex_nativity_year, aes(x = data_year, y = n, color = sex, linetype = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_sex, labels = c("M" = "Male", "F" = "Female")) +
  scale_linetype_manual(values = linetypes_nativity, labels = c("native" = "Native-born", "foreign" = "Foreign-born")) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", shape = NA))) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 20000), breaks = seq(0, 20000, by = 5000)) +
  labs(
    title = "Homicide Deaths by Sex and Nativity, 2014-2024",
    subtitle = "Underlying cause of death: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
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

ggsave("results/fig.15_homicides_by_sex_nativity_2014_2024.png", width = 15, height = 10)

# race 
race_nativity_year = homicides %>%
    filter(!is.na(nativity)) %>%
    mutate(
        data_year = as.integer(data_year),
        race_group = case_when(
            race_recode40 == "01" ~ "White",
            race_recode40 == "02" ~ "Black",
            race_recode40 == "03" ~ "AIAN",
            race_recode40 %in% c("04","05","06","07","08","09","10","11","12","13","14") ~ "Asian or Pacific Islander",
            race_recode40 >= "15" ~ "More than one race",
            .default = NA_character_)) %>%
    filter(!is.na(race_group)) %>%
    group_by(data_year, race_group, nativity) %>%
    summarise(n = n(), .groups = "drop")

print(race_nativity_year, n = Inf)

colors_race = c(
    "White" = "#3043B4",
    "Black" = "#A6192E",
    "AIAN" = "#00847E",
    "Asian or Pacific Islander" = "#D4A017",
    "More than one race" = "#7B2D8B")

# native-born race
race_native_year = race_nativity_year %>%
    filter(nativity == "native")

ggplot(race_native_year, aes(x = data_year, y = n, color = race_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_race) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 15000), breaks = seq(0, 15000, by = 5000)) +
  labs(
    title = "Homicide Deaths by Race, Native-Born, 2014-2024",
    subtitle = "Underlying cause of death: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
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

ggsave("results/fig.16a_homicides_by_race_native_2014_2024.png", width = 15, height = 10)

# Foreign-born race 
race_foreign_year = race_nativity_year %>%
    filter(nativity == "foreign")

ggplot(race_foreign_year, aes(x = data_year, y = n, color = race_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_race) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1500), breaks = seq(0, 1500, by = 500)) +
  labs(
    title = "Homicide Deaths by Race, Foreign-Born, 2014-2024",
    subtitle = "Underlying cause of death: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
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

ggsave("results/fig.16b_homicides_by_race_foreign_2014_2024.png", width = 15, height = 10)

# ethnicity (hispanic origin)
ethnicity_nativity_year = homicides %>%
    filter(!is.na(nativity)) %>%
    mutate(
        data_year = as.integer(data_year),
        hispanic_origin_num = as.integer(hispanic_origin),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:299 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(ethnicity_group)) %>%
    group_by(data_year, ethnicity_group, nativity) %>%
    summarise(n = n(), .groups = "drop")

print(ethnicity_nativity_year, n = Inf)

colors_ethnicity = c(
    "Non-Hispanic" = "#3043B4",
    "Hispanic"     = "#C97703")

ethnicity_native_year = ethnicity_nativity_year %>%
    filter(nativity == "native")

ggplot(ethnicity_native_year, aes(x = data_year, y = n, color = ethnicity_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_ethnicity) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA)) +
  labs(
    title = "Homicide Deaths by Ethnicity, Native-Born, 2014-2024",
    subtitle = "Underlying cause of death: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
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

ggsave("results/fig.17a_homicides_by_ethnicity_native_2014_2024.png", width = 15, height = 10)

ethnicity_foreign_year = ethnicity_nativity_year %>%
    filter(nativity == "foreign")

ggplot(ethnicity_foreign_year, aes(x = data_year, y = n, color = ethnicity_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_ethnicity) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 1500), breaks = seq(0, 1500, by = 500)) +
  labs(
    title = "Homicide Deaths by Ethnicity, Foreign-Born, 2014-2024",
    subtitle = "Underlying cause of death: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
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

ggsave("results/fig.17b_homicides_by_ethnicity_foreign_2014_2024.png", width = 15, height = 10)

## RATES, sex race eth foreign-born vs native pops
homicides_sex_age_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
    group_by(sex, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_sex_age_2024 = acs %>%
    mutate(sex_label = case_when(
        as.integer(sex) == 1 ~ "M",
        as.integer(sex) == 2 ~ "F",
        .default = NA_character_)) %>%
    filter(!is.na(nativity), !is.na(sex_label), year == 2024, !is.na(age_group)) %>%
    group_by(sex = sex_label, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

sex_rates_2024 = homicides_sex_age_2024 %>%
    left_join(acs_sex_age_2024, by = c("sex", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(sex, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(
        crude_rate = n / pop * 100000,
        category = paste(if_else(sex == "M", "Male", "Female"), if_else(nativity == "native", "Native-born", "Foreign-born"), sep = ", ")) %>%
    select(category, n, pop, crude_rate, age_adjusted_rate)

homicides_race_age_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
    mutate(race_group = case_when(
        race_recode40 == "01" ~ "White",
        race_recode40 == "02" ~ "Black",
        race_recode40 == "03" ~ "AIAN",
        race_recode40 %in% c("04","05","06","07","08","09","10","11","12","13","14") ~ "Asian or Pacific Islander",
        race_recode40 >= "15" ~ "More than one race",
        .default = NA_character_)) %>%
    filter(!is.na(race_group)) %>%
    group_by(race_group, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

homicides %>%
    filter(is.na(race_recode40)) %>%
    count(data_year, sort = TRUE)

acs_race_age_2024 = acs %>%
    mutate(
        race_num = as.integer(race),
        race_group = case_when(
            race_num == 1 ~ "White",
            race_num == 2 ~ "Black",
            race_num == 3 ~ "AIAN",
            race_num %in% c(4, 5, 6) ~ "Asian or Pacific Islander",
            race_num %in% c(8, 9) ~ "More than one race",
            .default = NA_character_)) %>%  # race_num == 7 ("Other race, nec") excluded - no NVSS equivalent
    filter(!is.na(nativity), !is.na(race_group), year == 2024, !is.na(age_group)) %>%
    group_by(race_group, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

race_rates_2024 = homicides_race_age_2024 %>%
    left_join(acs_race_age_2024, by = c("race_group", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(race_group, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(
        crude_rate = n / pop * 100000,
        category = paste(race_group, if_else(nativity == "native", "Native-born", "Foreign-born"), sep = ", ")) %>%
    select(category, n, pop, crude_rate, age_adjusted_rate)

homicides_ethnicity_age_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
    mutate(
        hispanic_origin_num = as.integer(hispanic_origin),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:299 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(ethnicity_group)) %>%
    group_by(ethnicity_group, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_ethnicity_age_2024 = acs %>%
    mutate(
        hispan_num = as.integer(hispan),
        ethnicity_group = case_when(
            hispan_num == 0 ~ "Non-Hispanic",
            hispan_num %in% 1:4 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(nativity), !is.na(ethnicity_group), year == 2024, !is.na(age_group)) %>%
    group_by(ethnicity_group, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

ethnicity_rates_2024 = homicides_ethnicity_age_2024 %>%
    left_join(acs_ethnicity_age_2024, by = c("ethnicity_group", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(ethnicity_group, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(
        crude_rate = n / pop * 100000,
        category = paste(ethnicity_group, if_else(nativity == "native", "Native-born", "Foreign-born"), sep = ", ")) %>%
    select(category, n, pop, crude_rate, age_adjusted_rate)

demographic_rates_2024 = bind_rows(sex_rates_2024, race_rates_2024, ethnicity_rates_2024) %>%
    rename(homicides = n, acs_population = pop) %>%
    select(category, homicides, acs_population, crude_rate, age_adjusted_rate)

print(demographic_rates_2024, n = Inf)

write_csv(demographic_rates_2024, "results/demographic_rates_by_nativity_2024.csv")

# white nonhispanic????
homicides_white_ethnicity_age_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
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
            .default = NA_character_)) %>%
    filter(race_group == "White", !is.na(ethnicity_group)) %>%
    group_by(ethnicity_group, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_white_ethnicity_age_2024 = acs %>%
    mutate(
        race_num = as.integer(race),
        hispan_num = as.integer(hispan),
        race_group = case_when(
            race_num == 1 ~ "White",
            race_num == 2 ~ "Black",
            race_num == 3 ~ "AIAN",
            race_num %in% c(4, 5, 6) ~ "Asian or Pacific Islander",
            race_num %in% c(8, 9) ~ "More than one race",
            .default = NA_character_),
        ethnicity_group = case_when(
            hispan_num == 0 ~ "Non-Hispanic",
            hispan_num %in% 1:4 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(nativity), year == 2024, !is.na(age_group), race_group == "White", !is.na(ethnicity_group)) %>%
    group_by(ethnicity_group, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

white_ethnicity_rates_2024 = homicides_white_ethnicity_age_2024 %>%
    left_join(acs_white_ethnicity_age_2024, by = c("ethnicity_group", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(ethnicity_group, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(
        crude_rate = n / pop * 100000,
        category = paste("White", ethnicity_group, if_else(nativity == "native", "Native-born", "Foreign-born"), sep = ", ")) %>%
    select(category, n, pop, crude_rate, age_adjusted_rate)

print(white_ethnicity_rates_2024, n = Inf)

# education
homicides %>%
    filter(data_year < 2018) %>%
    count(data_year, education_flag)

# marital status
marital_labels = c(
    "S" = "Never married, single",
    "M" = "Married",
    "W" = "Widowed",
    "D" = "Divorced",
    "U" = "Unknown")

marital_by_nativity = homicides %>%
    filter(!is.na(nativity), marital_status %in% names(marital_labels)) %>%
    mutate(marital_group = marital_labels[marital_status]) %>%
    count(nativity, marital_group) %>%
    group_by(nativity) %>%
    mutate(pct = n / sum(n) * 100) %>%
    arrange(nativity, desc(n))

print(marital_by_nativity, n = Inf)

# mean/median age and age distribution (2024)
age_numeric_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, detail_age_type == "1") %>%
    mutate(age_years = as.integer(detail_age))

median_age_2024 = age_numeric_2024 %>%
    group_by(nativity) %>%
    summarize(mean_age = mean(age_years, na.rm = TRUE), median_age = median(age_years, na.rm = TRUE), n = n(), .groups = "drop")

print(median_age_2024)

ggplot(age_numeric_2024, aes(x = age_years, fill = nativity, color = nativity)) +
  geom_density(alpha = 0.4, linewidth = 1.2) +
  scale_fill_manual(values = colors_2) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 0.03),breaks = seq(0, 0.03, by = 0.01)) +
  labs(
    title = "Age Distribution of Homicide Victims by Nativity, 2024",
    subtitle = "Resident deaths, 50 states and DC",
    x = "Age at death",
    y = "Density",
    fill = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 20, color = "gray40"),
    axis.text.y = element_text(size = 20, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.19_age_density_by_nativity_2024.png", width = 15, height = 10)

## FEMALES -----------------------------------------------------------------------------------
# median age
median_age_sex_nativity_2024 = age_numeric_2024 %>%
    group_by(sex, nativity) %>%
    summarize(median_age = median(age_years, na.rm = TRUE), n = n(), .groups = "drop") %>%
    arrange(sex, nativity)

print(median_age_sex_nativity_2024, n = Inf)

# marital status by sex
marital_rate_labels_nvss = c(
    "S" = "Never married, single",
    "M" = "Married",
    "W" = "Widowed",
    "D" = "Divorced",
    "U" = "Unknown")

marital_rate_labels_acs = c(
    "1" = "Married",
    "2" = "Married",
    "3" = "Married",
    "4" = "Divorced",
    "5" = "Widowed",
    "6" = "Never married, single")

homicides_marital_age_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, marital_status %in% names(marital_rate_labels_nvss),
           age_recode12 != "12") %>%
    mutate(marital_group = marital_rate_labels_nvss[marital_status]) %>%
    group_by(marital_group, sex, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_marital_age_2024 = acs %>%
    mutate(
        marst_num = as.integer(marst),
        sex_label = case_when(
            as.integer(sex) == 1 ~ "M",
            as.integer(sex) == 2 ~ "F",
            .default = NA_character_),
        marital_group = marital_rate_labels_acs[as.character(marst_num)]) %>%
    filter(!is.na(nativity), !is.na(sex_label), !is.na(marital_group), year == 2024, !is.na(age_group)) %>%
    group_by(marital_group, sex = sex_label, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

marital_rates_2024 = homicides_marital_age_2024 %>%
    left_join(
        acs_marital_age_2024,
        by = c("marital_group", "sex", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(marital_group, sex, nativity) %>%
    summarize(
        n = sum(n),
        pop = sum(pop),
        age_adjusted_rate = sum(weighted_rate),
        .groups = "drop") %>%
    mutate(crude_rate = n / pop * 100000) %>%
    select(marital_group, sex, nativity, n, pop, crude_rate, age_adjusted_rate) %>%
    arrange(marital_group, sex, nativity)

print(marital_rates_2024, n = Inf)

write_csv(marital_rates_2024, "results/marital_rates_by_sex_nativity_2024.csv")

homicides_marital_age_2024 %>%
    left_join(
        acs_marital_age_2024,
        by = c("marital_group", "sex", "nativity", "age_recode12" = "age_group")) %>%
    filter(marital_group == "Widowed") %>%
    mutate(rate_i = n / pop * 100000) %>%
    arrange(desc(rate_i)) %>%
    print(n = Inf)


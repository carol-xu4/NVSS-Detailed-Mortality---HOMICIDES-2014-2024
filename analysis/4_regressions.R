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

## ----------------------------------------------------------------------------------------
# TWFE panel regression of each state's native-born homicide rate on its foreign-born population share
# Numerator: homicides by state, year, nativity, age group (all 11 years) 
homicides_age_state_year_nativity = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    group_by(state_res_fips, data_year, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

# Denominator: ACS population by state, year, nativity, age group (all years)
acs_age_state_year_nativity = acs %>%
    filter(!is.na(nativity), !is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

# Age-adjusted rate by state, year, nativity
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

age_adjusted_state_year_nativity = homicides_age_state_year_nativity %>%
    left_join(
        acs_age_state_year_nativity,
        by = c("state_res_fips" = "state_usps", "data_year" = "year", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(state_res_fips, data_year, nativity) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

# Native-born rate by state-year 
native_rate_by_state_year = age_adjusted_state_year_nativity %>%
    filter(nativity == "native") %>%
    select(state = state_res_fips, data_year, native_homicide_rate = age_adjusted_rate)

# Foreign-born population share by state-year 
foreign_share_by_state_year = acs %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year) %>%
    summarize(
        foreign_pop = sum(perwt[nativity == "foreign"], na.rm = TRUE),
        total_pop = sum(perwt),
        .groups = "drop") %>%
    mutate(foreign_share = foreign_pop / total_pop * 100) %>%
    select(state = state_usps, data_year = year, foreign_share)

# Combine into one panel 
panel_data = native_rate_by_state_year %>%
    left_join(foreign_share_by_state_year, by = c("state", "data_year"))

print(panel_data, n = Inf)

write_csv(panel_data, "results/panel_native_rate_foreign_share_by_state_year.csv")

# TWFE regression 
model = feols(native_homicide_rate ~ foreign_share | state + data_year, data = panel_data)

summary(model)

# clustered
model_clustered = feols(native_homicide_rate ~ foreign_share | state + data_year,
                         data = panel_data, cluster = ~state)

summary(model_clustered)

ggplot(panel_data, aes(x = foreign_share, y = native_homicide_rate)) +
  geom_point(color = "#3043B4", size = 2.5, alpha = 0.6) +
  labs(
    title = "Foreign-Born Share vs. Native-Born Homicide Rate, by State-Year",
    subtitle = "Raw observations, 50 states and DC, 2014-2024 (561 state-year points)",
    x = "Foreign-born population share (%)",
    y = "Native-born age-adjusted homicide rate",
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.13_raw_scatter_foreign_share_native_rate.png", width = 12, height = 8)

## Residualize both variables on state + year FE (removes everything the FE already absorbed) -
resid_native_rate = resid(feols(native_homicide_rate ~ 1 | state + data_year, data = panel_data))
resid_foreign_share = resid(feols(foreign_share ~ 1 | state + data_year, data = panel_data))

fwl_data = panel_data %>%
    mutate(
        resid_native_rate = resid_native_rate,
        resid_foreign_share = resid_foreign_share)

ggplot(fwl_data, aes(x = resid_foreign_share, y = resid_native_rate)) +
  geom_point(color = "#3043B4", size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1.2) +
  labs(
    title = "Within-State Relationship: Foreign-Born Share and Native-Born Homicide Rate",
    subtitle = "Partial regression plot, state and year fixed effects removed \n Slope = -0.45 (clustered SE, p = 0.011)",
    x = "Foreign-born share (residualized)",
    y = "Native-born age-adjusted homicide rate (residualized)",
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.14_twfe_partial_regression_plot.png", width = 12, height = 8)


# crude rates, state, year
homicides_state_year_nativity = homicides %>%
    filter(!is.na(nativity)) %>%
    group_by(state_res_fips, data_year, nativity) %>%
    summarize(n = n(), .groups = "drop")

acs_state_year_nativity = acs %>%
    filter(!is.na(nativity)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year, nativity) %>%
    summarize(pop = sum(perwt), .groups = "drop")

crude_state_year_nativity = homicides_state_year_nativity %>%
    left_join(
        acs_state_year_nativity,
        by = c("state_res_fips" = "state_usps", "data_year" = "year", "nativity")) %>%
    mutate(crude_rate = n / pop * 100000)

native_rate_by_state_year = crude_state_year_nativity %>%
    filter(nativity == "native") %>%
    select(state = state_res_fips, data_year, native_homicide_rate = crude_rate)

foreign_share_by_state_year = acs %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, year) %>%
    summarize(
        foreign_pop = sum(perwt[nativity == "foreign"], na.rm = TRUE),
        total_pop = sum(perwt),
        .groups = "drop") %>%
    mutate(foreign_share = foreign_pop / total_pop * 100) %>%
    select(state = state_usps, data_year = year, foreign_share)


panel_data_crude = native_rate_by_state_year %>%
    left_join(foreign_share_by_state_year, by = c("state", "data_year"))

print(panel_data_crude)

write_csv(panel_data_crude, "results/panel_data_crude.csv")

# illegal immigrant populations
state_immig_2024 = read_csv("data/input/acs_state_populations_2024.csv")

illegal_pop_2024 = state_immig_2024 %>%
    filter(immig_status == "Illegal immigrants") %>%
    select(state_abb, illegal_immigrant_pop = population)

total_pop_2024 = state_immig_2024 %>%
    group_by(state_abb) %>%
    summarize(total_pop = sum(population), .groups = "drop")

illegal_share_2024 = illegal_pop_2024 %>%
    left_join(total_pop_2024, by = "state_abb") %>%
    mutate(illegal_share = illegal_immigrant_pop / total_pop * 100)

native_homicides_2024 = homicides %>%
    filter(nativity == "native", data_year == 2024) %>%
    count(state_res_fips, name = "n")

native_pop_2024 = acs %>%
    filter(nativity == "native", year == 2024) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps) %>%
    summarize(pop = sum(perwt), .groups = "drop")

native_rate_2024 = native_homicides_2024 %>%
    left_join(native_pop_2024, by = c("state_res_fips" = "state_usps")) %>%
    mutate(native_rate = n / pop * 100000)

plot_data_2024 = native_rate_2024 %>%
    left_join(illegal_share_2024, by = c("state_res_fips" = "state_abb"))

ggplot(plot_data_2024, aes(x = illegal_immigrant_pop, y = n)) +
  geom_point(color = "#3043B4", size = 3.5, alpha = 0.8) +
  scale_x_continuous(labels = scales::comma, expand = c(0.02, 0)) +
  scale_y_continuous(labels = scales::comma, expand = c(0.02, 0), limits = c(0, 1600), breaks = seq(0, 1600, by = 400)) +
  labs(
    title = "Illegal Immigrant Population vs. Native-Born Homicides \nby State, 2024",
    subtitle = "Raw counts - not population-adjusted",
    x = "Estimated illegal immigrant population",
    y = "Native-born homicide count",
    caption = "Source: NCHS restricted-use mortality files; ACS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.20_illegal_immigrant_pop_vs_native_homicides_raw_2024.png", width = 14, height = 9)

native_homicides_age_2024 = homicides %>%
    filter(nativity == "native", data_year == 2024, age_recode12 != "12") %>%
    count(state_res_fips, age_recode12, name = "n")

native_pop_age_2024 = acs %>%
    filter(nativity == "native", year == 2024, !is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

# age adjusted rates
native_rate_2024 = native_homicides_age_2024 %>%
    left_join(native_pop_age_2024, by = c("state_res_fips" = "state_usps", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(state_res_fips) %>%
    summarize(
        n = sum(n),
        pop = sum(pop),
        age_adjusted_rate = sum(weighted_rate),
        .groups = "drop") %>%
    mutate(crude_rate = n / pop * 100000) %>%
    select(state_res_fips, n, pop, crude_rate, age_adjusted_rate)

print(native_rate_2024, n = Inf)

plot_data_2024 = native_rate_2024 %>%
    left_join(state_immig_2024 %>% filter(immig_status == "Illegal immigrants") %>%
                  select(state_abb, illegal_n = n, illegal_pop = population),
              by = c("state_res_fips" = "state_abb")) %>%
    mutate(illegal_share = illegal_pop / pop * 100)

ggplot(plot_data_2024, aes(x = illegal_pop, y = n)) +
    geom_point(color = colors_2[1], size = 2.5) +
    ggrepel::geom_text_repel(aes(label = state_res_fips), size = 3) +
    labs(
        title = "Illegal Immigrant Population vs. Native-Born Homicides by State",
        subtitle = "2024, raw counts",
        x = "Estimated illegal immigrant population",
        y = "Native-born homicide count") +
    theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggplot(plot_data_2024, aes(x = illegal_share, y = age_adjusted_rate)) +
    geom_point(color = colors_2[1], size = 2.5) +
    ggrepel::geom_text_repel(aes(label = state_res_fips), size = 3) +
    labs(
        title = "Illegal Immigrant Share vs. Native-Born Homicide Rate by State",
        subtitle = "2024, age-adjusted rate per 100,000 native-born population",
        x = "Illegal immigrants as % of state population",
        y = "Age-adjusted native-born homicide rate per 100,000") +
    theme_minimal() +
    theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title.x = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16, color = "gray40"),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.21_illegal_immigrant_share_vs_native_homicide_rate_2024.png", width = 14, height = 9)

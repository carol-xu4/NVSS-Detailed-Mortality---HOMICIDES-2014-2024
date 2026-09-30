## Preliminaries -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/NVSS Homicides 2014-2024")

# read in joined csv
homicides = read_csv("data/output/homicides_2014_2024.csv")

names(homicides)

# homicides per year
homicides_year = homicides %>%
    group_by(data_year) %>%
    summarise(n = n())

homicides_year

# homicides by nativity
# native vs foreign-born variable
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

homicides = homicides %>%
    mutate(nativity = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
        .default = NA_character_))

homicides_by_nativity = homicides %>% 
    group_by(data_year, nativity) %>%
    summarize(n = n())

print(homicides_by_nativity, n = Inf)

write_csv(homicides_by_nativity, "results/homicides_by_nativity_year.csv")

colors_2 = c("native" = "#3043B4", "foreign" = "#C97703")

ggplot(homicides_by_nativity %>% filter(!is.na(nativity)),
  aes(x = data_year, y = n, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 25000)) +
  labs(
    title = "Homicide Deaths by Nativity, 2014-2024",
    subtitle = "UCOD: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS Detailed Mortality - Limited Geography files, restricted use") +
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

ggsave("results/fig.1_homicides_by_nativity_year.png", width = 15, height = 10)

homicides = homicides %>%
    mutate(birthplace_group = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode == "CC" ~ "Canada",
        state_country_birth_recode == "MX" ~ "Mexico",
        state_country_birth_recode == "CU" ~ "Cuba",
        state_country_birth_recode == "YY" ~ "Rest of world",
        .default = NA_character_))

birthplace = homicides %>% 
    group_by(data_year, birthplace_group) %>%
    summarize(n = n())

print(birthplace)

write_csv(birthplace, "results/homicides_by_nativity_2024.csv")

foreign_total = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    group_by(data_year) %>%
    summarize(n = sum(n)) %>%
    mutate(birthplace_group = "Foreign total")

foreign_plot_data = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    bind_rows(foreign_total) %>%
    mutate(birthplace_group = factor(birthplace_group,
        levels = c("Foreign total", "Canada", "Cuba", "Mexico", "Rest of world")))

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

ggplot(foreign_plot_data, aes(x = data_year, y = n, color = birthplace_group, linetype = birthplace_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_5) +
  scale_linetype_manual(values = linetypes_5) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 2500)) +
  labs(
    title = "Homicide Deaths by Foreign Birthplace, 2014-2024",
    subtitle = "UCOD: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS Detailed Mortality - Limited Geography files, restricted use") +
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

ggsave("results/fig.2_foreign_homicides_year.png", width = 15, height = 10)

# 2024 homicides by state (smell check w NCHS)
homicides %>%
    filter(data_year == 2024) %>%
    group_by(state_occ_fips) %>%
    summarise(n = n()) %>%
    print(n = Inf)

homicides %>%
    filter(data_year == 2024) %>%
    group_by(state_res_fips) %>%
    summarise(n = n()) %>%
    print(n = Inf)

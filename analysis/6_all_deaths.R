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
## WHAT PERCENT OF ALL DEATHS ARE HOMICIDES, BY NATIVITY

# read in all deaths data
# homicides 113 causes recode 128 & 129


# col_positions 
col_positions_all_deaths = fwf_positions(
  start = c(29, 59, 102, 154),
  end   = c(30, 60, 105, 156),
  col_names = c("state_res_fips", "state_country_birth_recode", "data_year", "cause_recode113"))

read_all_deaths = function(path) {
  read_fwf(path, col_positions = col_positions_all_deaths, col_types = cols(.default = "c")) %>%
    filter(state_res_fips %in% us_states_dc)}

## File paths per year (same list used throughout) -------------------------------------------
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
                ps = NA),
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

# TOTAL mortality file 
all_deaths_by_year = list()

for (yr in names(year_files)) {
  paths = year_files[[yr]]
  parts = list(read_all_deaths(paths$us))
  if (!is.na(paths$ps)) parts[[2]] = read_all_deaths(paths$ps)

  all_deaths_yr = bind_rows(parts)
  cat(yr, ":", nrow(all_deaths_yr), "total deaths (50 states + DC)\n")

  all_deaths_by_year[[yr]] = all_deaths_yr}

all_deaths = bind_rows(all_deaths_by_year)

# nativity
all_deaths = all_deaths %>%
    mutate(
        data_year = as.integer(data_year),
        nativity = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
            .default = NA_character_))

# homicide share
pct_homicide_by_nativity = all_deaths %>%
    filter(!is.na(nativity)) %>%
    group_by(data_year, nativity) %>%
    summarize(
        total_deaths = n(),
        homicide_deaths = sum(cause_recode113 %in% c("128", "129")),
        pct_homicide = homicide_deaths / total_deaths * 100,
        .groups = "drop")

print(pct_homicide_by_nativity, n = Inf)

write_csv(pct_homicide_by_nativity, "results/pct_deaths_homicide_by_nativity_2014_2024.csv")

colors_2 = c("native" = "#3043B4", "foreign" = "#C97703")

ggplot(pct_homicide_by_nativity, aes(x = data_year, y = pct_homicide, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.02, 0), limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(
    title = "Homicide as a Share of All Deaths, by Nativity, 2014-2024",
    subtitle = "Homicide deaths as a percent of all deaths from any cause \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
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

ggsave("results/fig.18_pct_homicide_by_nativity_2014_2024.png", width = 15, height = 10)

# leading causes
cause113_labels = c(
  "001" = "Salmonella infections", "002" = "Shigellosis and amebiasis",
  "003" = "Certain other intestinal infections", "005" = "Respiratory tuberculosis",
  "006" = "Other tuberculosis", "007" = "Whooping cough",
  "008" = "Scarlet fever and erysipelas", "009" = "Meningococcal infection",
  "010" = "Septicemia", "011" = "Syphilis", "012" = "Acute poliomyelitis",
  "013" = "Arthropod-borne viral encephalitis", "014" = "Measles",
  "015" = "Viral hepatitis", "016" = "HIV disease", "017" = "Malaria",
  "018" = "Other/unspecified infectious and parasitic diseases",
  "020" = "Cancer: lip, oral cavity and pharynx", "021" = "Cancer: esophagus",
  "022" = "Cancer: stomach", "023" = "Cancer: colon, rectum and anus",
  "024" = "Cancer: liver and intrahepatic bile ducts", "025" = "Cancer: pancreas",
  "026" = "Cancer: larynx", "027" = "Cancer: trachea, bronchus and lung",
  "028" = "Malignant melanoma of skin", "029" = "Cancer: breast",
  "030" = "Cancer: cervix uteri", "031" = "Cancer: corpus uteri/uterus NOS",
  "032" = "Cancer: ovary", "033" = "Cancer: prostate",
  "034" = "Cancer: kidney and renal pelvis", "035" = "Cancer: bladder",
  "036" = "Cancer: meninges, brain, other CNS", "038" = "Hodgkin disease",
  "039" = "Non-Hodgkin lymphoma", "040" = "Leukemia",
  "041" = "Multiple myeloma and immunoproliferative neoplasms",
  "042" = "Other lymphoid/hematopoietic cancer", "043" = "All other malignant neoplasms",
  "044" = "In situ/benign neoplasms, uncertain behavior", "045" = "Anemias",
  "046" = "Diabetes mellitus", "047" = "Nutritional deficiencies",
  "048" = "Malnutrition", "049" = "Other nutritional deficiencies",
  "050" = "Meningitis", "051" = "Parkinson disease", "052" = "Alzheimer disease",
  "055" = "Acute rheumatic fever and chronic rheumatic heart disease",
  "056" = "Hypertensive heart disease", "057" = "Hypertensive heart and renal disease",
  "059" = "Acute myocardial infarction", "060" = "Other acute ischemic heart disease",
  "062" = "Atherosclerotic cardiovascular disease", "063" = "Other chronic ischemic heart disease",
  "065" = "Acute and subacute endocarditis", "066" = "Diseases of pericardium/acute myocarditis",
  "067" = "Heart failure", "068" = "All other heart disease",
  "069" = "Essential hypertension and hypertensive renal disease",
  "070" = "Cerebrovascular diseases (stroke)", "071" = "Atherosclerosis",
  "073" = "Aortic aneurysm and dissection", "074" = "Other diseases of arteries/arterioles/capillaries",
  "075" = "Other disorders of circulatory system", "077" = "Influenza",
  "078" = "Pneumonia", "080" = "Acute bronchitis and bronchiolitis",
  "081" = "Other acute lower respiratory infections", "083" = "Chronic bronchitis",
  "084" = "Emphysema", "085" = "Asthma", "086" = "Other chronic lower respiratory disease",
  "087" = "Pneumoconioses and chemical effects", "088" = "Pneumonitis due to solids/liquids",
  "089" = "Other diseases of respiratory system", "090" = "Peptic ulcer",
  "091" = "Diseases of appendix", "092" = "Hernia", "094" = "Alcoholic liver disease",
  "095" = "Other chronic liver disease and cirrhosis", "096" = "Cholelithiasis/gallbladder disorders",
  "098" = "Acute/rapidly progressive nephritic and nephrotic syndrome",
  "099" = "Chronic glomerulonephritis/nephritis/nephropathy", "100" = "Renal failure",
  "101" = "Other disorders of kidney", "102" = "Infections of kidney",
  "103" = "Hyperplasia of prostate", "104" = "Inflammatory diseases of female pelvic organs",
  "106" = "Pregnancy with abortive outcome",
  "107" = "Other complications of pregnancy/childbirth/puerperium",
  "108" = "Conditions originating in the perinatal period",
  "109" = "Congenital malformations/deformations/chromosomal abnormalities",
  "110" = "Symptoms, signs, abnormal findings NEC", "111" = "All other diseases (residual)",
  "114" = "Motor vehicle accidents", "115" = "Other land transport accidents",
  "116" = "Water/air/space and other transport accidents", "118" = "Falls",
  "119" = "Accidental discharge of firearms", "120" = "Accidental drowning and submersion",
  "121" = "Accidental exposure to smoke, fire and flames",
  "122" = "Accidental poisoning/exposure to noxious substances",
  "123" = "Other/unspecified nontransport accidents",
  "125" = "Suicide by discharge of firearms", "126" = "Suicide by other/unspecified means",
  "128" = "Homicide by discharge of firearms", "129" = "Homicide by other/unspecified means",
  "130" = "Legal intervention", "132" = "Discharge of firearms, undetermined intent",
  "133" = "Other/unspecified events of undetermined intent",
  "134" = "Operations of war and their sequelae",
  "135" = "Complications of medical and surgical care")

top_causes_by_nativity = all_deaths %>%
    filter(!is.na(nativity)) %>%
    count(nativity, cause_recode113) %>%
    group_by(nativity) %>%
    mutate(pct = n / sum(n) * 100) %>%
    slice_max(n, n = 10) %>%
    ungroup() %>%
    mutate(cause_label = coalesce(cause113_labels[cause_recode113], paste0("Code ", cause_recode113, " (not in lookup - check codebook)"))) %>%
    select(nativity, cause_recode113, cause_label, n, pct) %>%
    arrange(nativity, desc(n))

print(top_causes_by_nativity, n = Inf)

write_csv(top_causes_by_nativity, "results/top10_causes_of_death_by_nativity_2014_2024.csv")


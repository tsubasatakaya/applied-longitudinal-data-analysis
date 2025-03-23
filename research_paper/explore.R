source("research_paper/setup.R")

################################################
# Data preparation
################################################
data <- read_dta("data/PAIRFAM.dta")

# Research questions
# 1. Effect of transition to different partnership status (entry, exit, stay; multi-category)
# 2. Effect of transition to having a partner (dummy)
# 3. Focus on dissolution of partnership 

# Sample restrictions
# 1. Only focus on those who were single before status change?
# 2. Age at first round?
# 3. Years of staying in the panel

transition_type <- c(
  "stay_signle",
  "stay_LAT",
  "stay_coh",
  "stay_married",
  "single_to_LAT",
  "single_to_coh",
  "single_to_married",
  "LAT_to_coh",
  "LAT_to_married",
  "coh_to_married",
  "LAT_to_single",
  "coh_to_single",
  "married_to_single",
  "coh_to_LAT",
  "married_to_LAT",
  "married_to_coh"
)


data_processed <- data |> 
  mutate(n_wave = n_distinct(wave), .by = id) |> 
  filter(n_wave >= 2) |>  # stay in the panel for at least two waves
  filter(AGE >= 25) |>  # Remove age under 25
  arrange(id, wave) |> 
  mutate(FAM_BEFORE = dplyr::lag(FAM_NOW), .by = id) |> 
  mutate(fam_transition = case_when(
    # Stay
    FAM_NOW == 0 & FAM_BEFORE == 0 ~ "stay_signle",
    FAM_NOW == 1 & FAM_BEFORE == 1 ~ "stay_LAT",
    FAM_NOW == 2 & FAM_BEFORE == 2 ~ "stay_coh",
    FAM_NOW == 3 & FAM_BEFORE == 3 ~ "stay_married",
    # Move up
    FAM_NOW == 1 & FAM_BEFORE == 0 ~ "single_to_LAT",
    FAM_NOW == 2 & FAM_BEFORE == 0 ~ "single_to_coh",
    FAM_NOW == 3 & FAM_BEFORE == 0 ~ "single_to_married",
    FAM_NOW == 2 & FAM_BEFORE == 1 ~ "LAT_to_coh",
    FAM_NOW == 3 & FAM_BEFORE == 1 ~ "LAT_to_married",
    FAM_NOW == 3 & FAM_BEFORE == 2 ~ "coh_to_married",
    # Move down
    FAM_NOW == 0 & FAM_BEFORE == 1 ~ "LAT_to_single",
    FAM_NOW == 0 & FAM_BEFORE == 2 ~ "coh_to_single",
    FAM_NOW == 0 & FAM_BEFORE == 3 ~ "married_to_single",
    FAM_NOW == 1 & FAM_BEFORE == 2 ~ "coh_to_LAT",
    FAM_NOW == 1 & FAM_BEFORE == 3 ~ "married_to_LAT",
    FAM_NOW == 2 & FAM_BEFORE == 3 ~ "married_to_coh",
    .default = NA)
    ) |> 
  mutate(fam_transition = factor(fam_transition, levels = transition_type)) |> 
  filter(!(fam_transition %in% c("coh_to_LAT", "married_to_LAT", "married_to_coh")))

table1(~ fam_transition | factor(SEX), data = data_processed |> drop_na(SEX))



################################################
# Model fitting
################################################
# CASMIN (education factor)
# hhincoecd (income)
# DEPRESSION
# AGE (continuous -> category)
# EMP (categorical)
# nkids (continuous -> dummy)




















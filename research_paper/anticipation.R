source("research_paper/setup.R")
################################################
# Data preparation
################################################
data <- read_dta("data/PAIRFAM.dta") |> 
  arrange(id, wave)

data_filtered <- data |> 
  # derive transition point
  # and calculate nth of transition point
  mutate(transition = FAM_NOW != dplyr::lag(FAM_NOW, 1, default = first(FAM_NOW)),
         transition_cum = cumsum(transition),
         first_transition_wave = ifelse(all(transition_cum == 0),
                                        0,
                                        min(wave[transition_cum == 1])),
         .by = id) |> 
  # keep only up to transition_cum <=1 (keep only the first episode of transition)
  filter(transition_cum <= 1) |> 
  mutate(partner_duration = cumsum(transition_cum), .by = id)


################################################
# Create variables
################################################
data_cleaned <- data_filtered |> 
  mutate(sex = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female",
                         .default = NA)) |> 
  mutate(emp = case_when(EMP == 1 ~ "Full-time",
                         EMP == 2 ~ "Part-time",
                         EMP == 3 ~ "Not working",
                         .default = NA)) |> 
  mutate(edu = case_when(CASMIN == 0 ~ "In school",
                         CASMIN == 1 ~ "Low",
                         CASMIN == 2 ~ "Medium",
                         CASMIN == 3 ~ "High")) |> 
  mutate(has_kid = case_when(nkids > 0 ~ "Has kid",
                             nkids == 0 ~ "No kid",
                             .default = NA)) |> 
  mutate(partnership = case_when(FAM_NOW == 0 ~ "Single",
                                 FAM_NOW == 1 ~ "LAT",
                                 FAM_NOW == 2 ~ "Cohabiting",
                                 FAM_NOW == 3 ~ "Married",
                                 .default = NA)) |> 
  mutate(log_income = log(hhincoecd)) |> 
  rename(age = "AGE",
         depression = "DEPRESSION",
         life_sat = "sat6") |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         emp = factor(emp, levels = c("Not working", "Part-time", "Full-time")),
         edu = factor(edu, levels = c("In school", "Low", "Medium", "High")),
         partnership = factor(partnership, levels = c("Single", "LAT", 
                                                      "Cohabiting", "Married")),
         has_kid = factor(has_kid, levels = c("No kid", "Has kid"))) |> 
  drop_na(id, inty, wave, sex, life_sat, partnership, emp,
          edu, age, has_kid, log_income, depression) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)


library(sjPlot)
controls <- c("age", "edu", "has_kid", "emp", "log_income", "depression")
anticipation_formula <- as.formula(
  paste0("life_sat ~ ", "factor(TIME_MARRIAGE) +", paste0(controls, collapse = "+"))
)
model_ant <- lm(anticipation_formula, data = data_cleaned)
plot_model(model_ant, type = "pred", terms = c("TIME_MARRIAGE"))


















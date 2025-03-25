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
  # which partnership status one ends up in
  mutate(partnership_group = max(FAM_NOW), .by = id) |> 
  # treated group or never treated
  mutate(treated = as.integer(partnership_group != 0)) |> 
  # keep only those who were single at first
  filter(first(FAM_NOW) == 0, .by = id) |> 
  mutate(relative_time = ifelse(treated == 1, 
                                wave - first_transition_wave + 1, 0)) |> 
  mutate(relative_time = factor(relative_time, 
                                levels = c(0, unique(relative_time[relative_time != 0]))))

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
  mutate(log_income = log(hhincoecd)) |> 
  rename(age = "AGE",
         depression = "DEPRESSION",
         life_sat = "sat6") |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         emp = factor(emp, levels = c("Not working", "Part-time", "Full-time")),
         edu = factor(edu, levels = c("In school", "Low", "Medium", "High")),
         has_kid = factor(has_kid, levels = c("No kid", "Has kid"))) |> 
  drop_na(id, inty, wave, sex, life_sat, relative_time, partnership_group, emp,
          edu, age, has_kid, log_income, depression) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)

lat_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 1))
coh_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 2))
marry_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 3))

################################################
# Fit dynamic two-way fixed effects model
################################################
controls <- c("age", "edu", "has_kid", "emp", "log_income", "depression")
twfe_formula <- as.formula(
  paste0("life_sat ~ ", "relative_time +", paste0(controls, collapse = "+"))
)

dyn_twfe_lat_male = plm(twfe_formula,
                        data = marry_data |> filter(sex == "Male"),
                        index = c("id", "wave"),
                        model = "within",
                        effect = "twoways")
dyn_twfe_lat_female = plm(twfe_formula,
                        data = marry_data |> filter(sex == "Female"),
                        index = c("id", "wave"),
                        model = "within",
                        effect = "twoways")
summary(dyn_twfe_lat_female)








source("research_paper/setup.R")
################################################
# Load data
################################################
data <- read_dta("data/PAIRFAM.dta") |> 
  arrange(id, wave) |> 
  # derive transition point
  # and calculate nth of transition point
  mutate(transition = FAM_NOW != dplyr::lag(FAM_NOW, 1, default = first(FAM_NOW)),
         transition_cum = cumsum(transition),
         first_transition_wave = ifelse(all(transition_cum == 0),
                                        0,
                                        min(wave[transition_cum == 1])),
         .by = id) |> 
  # keep only up to transition_cum <=1 (keep only the first episode of transition)
  filter(transition_cum <= 1)


################################################
# Create variables
################################################
# Treatment-related variables
data_processed <- data  |> 
  # duration of partnership
  mutate(partner_duration = cumsum(transition_cum), .by = id) |>  
  # which partnership status one ends up in
  mutate(partnership_group = max(FAM_NOW), .by = id) |> 
  # treated or never treated
  mutate(treated = as.integer(partnership_group != 0)) |> 
  # single in the first wave
  mutate(single_first_wave = first(FAM_NOW) == 0, .by = id)

# Controls
data_processed <- data_processed |> 
  mutate(sex = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female",
                         .default = NA)) |> 
  mutate(emp = case_when(EMP == 1 ~ "Full-time",
                         EMP == 2 ~ "Part-time",
                         EMP == 3 ~ "Not working",
                         .default = NA)) |> 
  mutate(has_kid = case_when(nkids > 0 ~ "Has child",
                             nkids == 0 ~ "No child",
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
         partnership = factor(partnership, levels = c("Single", "LAT", 
                                                      "Cohabiting", "Married")),
         has_kid = factor(has_kid, levels = c("No child", "Has child"))) |> 
  drop_na(id, inty, wave, sex, life_sat, partnership, emp,
          age, has_kid, log_income, depression)
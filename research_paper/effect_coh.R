source("research_paper/setup.R")

################################################
# Data preparation
################################################
data <- read_dta("data/PAIRFAM.dta") |> 
  mutate(n_wave = n_distinct(wave), .by = id) |> 
  filter(n_wave >= 2) |>  # stay in the panel for at least two waves
  filter(AGE >= 18) |>  # Remove age under 18
  arrange(id, wave) |> 
  mutate(FAM_BEFORE = dplyr::lag(FAM_NOW),
         fam_first = dplyr::first(FAM_NOW), .by = id) |> 
  mutate(sex = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female",
                         .default = NA)) |> 
  mutate(has_kid = case_when(nkids > 0 ~ 1,
                             nkids == 0 ~ 0,
                             .default = NA)) |> 
  mutate(log_income = log(hhincoecd)) |> 
  rename(edu = "CASMIN",
         age = "AGE",
         depression = "DEPRESSION",
         life_sat = "sat6") |> 
  mutate(across(c(sex, edu), factor))

# data <- data |> 
#   mutate(cohabitation = case_when(FAM_NOW == 2 ~ 1,
#                                  .default = 0)) |>
#   group_by(id) |> 
#   mutate(year_marriage = case_when(any(marriage == 1) ~ min(inty[marriage == 1],
#                                                             na.rm = TRUE),
#                                    .default = 0)) |> 
#   ungroup()
# 
# s <- data |> 
#   filter(year_marriage != YEAR_MARRIAGE) |> 
#   select(id, inty, FAM_NOW, FAM_BEFORE, marriage, YEAR_MARRIAGE, year_marriage)

coh_data <- data |> 
  # Restict sample to singles at the first wave
  filter(fam_first == 0) |> 
  # Drop respondents who are in LAT or marriage at any time
  filter(!any(FAM_NOW %in% c(1, 3)), .by = id) |> 
  # Cohabitation treatment (switches to 1 only in posttreatment periods)
  mutate(cohabitation = as.integer(FAM_NOW == 2)) |> 
  # Dummy for treated group vs. never treated group
  mutate(treated = as.integer(any(cohabitation == 1)), .by = id) |> 
  # Relative period from the first interview year
  mutate(period = inty - min(inty) + 1, .by = id) |> 
  group_by(id) |> 
  # First year of the posttreatment period
  mutate(first_year_cohabitation = ifelse(treated == 1, 
                                          min(inty[cohabitation == 1]),
                                          0)) |> 
  # Defines group according to the treatment timing (period, not calendar year = stacked)
  mutate(group = ifelse(treated == 1,
                        min(period[cohabitation == 1]),
                        0)) |> 
  ungroup() |> 
  mutate(coh_rel_time = ifelse(treated == 1,
                               inty - first_year_cohabitation + 1,
                               0)) |> 
  mutate(coh_rel_time = factor(coh_rel_time, levels = c(0, unique(coh_rel_time[coh_rel_time != 0]))))

coh_data |> 
  drop_na(coh_rel_time) |> 
  ggplot(aes(x = coh_rel_time)) +
  geom_histogram()

coh_data |> 
  group_by(id, inty) |> 
  filter(n() > 1)
  


event_model <- lm(life_sat ~ factor(coh_rel_time) + age + edu + log_income +
                    has_kid + depression, data = coh_data)
summary(event_model)

static_twfe_model <- plm(life_sat ~ cohabitation + age + edu + log_income +
                           has_kid + depression, data = coh_data,
                         index = c("id", "wave"),
                         model = "within",
                         effect = "twoways")
summary(static_twfe_model)

dynamic_twfe_model <- plm(life_sat ~ factor(coh_rel_time) + age
                          + edu + log_income + has_kid + depression,
                          data = coh_data,
                          index = c("id", "wave"),
                          model = "within",
                          effect = "twoways")
summary(dynamic_twfe_model)


library(did)
did_data <-  coh_data |> 
  drop_na(id, inty, life_sat, period, group, edu, log_income, has_kid,
          depression) |> 
  group_by(group, period) |> 
  mutate(group_period_count = n()) |> 
  ungroup() |> 
  filter(group_period_count >= 5)

table1(~ factor(period) | factor(group), data = did_data)

did_data |> 
  filter(group > 0) |> 
  ggplot(aes(group)) +
  geom_histogram()

attgt_model <- att_gt(yname = "life_sat",
                      tname = "period",
                      idname = "id",
                      gname = "group",
                      xformla = ~ log_income + depression,
                      data = did_data,
                      control_group = "notyettreated",
                      allow_unbalanced_panel = TRUE)
dynamic_att <- aggte(attgt_model, type = "dynamic", na.rm = TRUE)
summary(dynamic_att)
ggdid(dynamic_att)




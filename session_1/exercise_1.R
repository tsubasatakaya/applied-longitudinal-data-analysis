library(tidyverse)
library(haven)
library(survival)
library(survminer)

lfp_data <- read_csv("data/oecd_labor_force_participation_rate.csv",)
lfp_data |> 
  ggplot(aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line(aes(color = factor(Sex)), linewidth = 1) +
  theme_minimal() +
  scale_color_manual(name = "",
                     labels = c("Female", "Male"),
                     values = c("#F8766D", "#00BFC4")) +
  theme(legend.position = "bottom")

data <- read_dta("data/ESS9e03_2.dta") |> 
  as_tibble() |> 
  select(lvpntyr, yrbrn, cntry, inwyys, gndr) |> 
  filter(cntry == "FR") |> 
  filter(yrbrn >= 1920 & yrbrn <= 1999) |> 
  filter((lvpntyr == 0) | (lvpntyr >= 1920 & lvpntyr <= 2020)) |> 
  drop_na()

data <- data |> 
  mutate(event = case_when(lvpntyr == 0 ~ 0,
                           lvpntyr > 0 ~ 1,
                           .default = NA)) |> 
  mutate(age_1 = lvpntyr - yrbrn,
         age_2 = inwyys - yrbrn) |> 
  mutate(time = case_when(event == 1 ~ age_1,
                          event == 0 ~ age_2,
                          .default = NA)) |> 
  mutate(gender = as.factor(case_when(gndr == 1 ~ "male",
                                      gndr == 2 ~ "female",
                                      .default = NA))) |> 
  mutate(cohort = cut(yrbrn,
                      breaks = c(1919, 1945, 1960, 1980, 1999),
                      labels = c("1920-1945", "1946-1960", "1961-1980", "1981-1999"),
                      include.lowest = TRUE,
                      ))


fit_gender <- survfit(Surv(data$time, data$event) ~ data$gender)
ggsurvplot(fit_gender, data = data, surv.median.line = "hv")

fit_cohort <- survfit(Surv(data$time, data$event) ~ data$cohort)
ggsurvplot(fit_cohort, data = data,)



















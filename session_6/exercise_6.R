library(tidyverse)
library(haven)
library(forcats)
library(lmtest)
library(gt)
library(table1)
library(gtsummary)
library(modelsummary)
library(did)
library(plm)

output_path <- "session_6/output"

data <- read_dta("data/SHARE.dta")

##############################################################################
# Data cleaning
##############################################################################
# age, isced1997_r, mar_stat, ch001_, chronic_mod, eurod, bfi10_extra_mod,
# bfi10_neuro_mod, ep005_


data_cleaned <- data |> 
  mutate(id = as.integer(as.factor(mergeid)),
         wave = as.factor(wave)) |> 
  filter(age >= 0) |> 
  mutate(years_edu = case_when(eduyears_mod < 0 ~ NA,
                               .default = eduyears_mod)) |> 
  mutate(marry = case_when(mar_stat == 1 ~ "married and living",
                           mar_stat == 3 ~ "married and separate",
                           mar_stat %in% c(2, 4, 5, 6) ~ "not married",
                           .default = NA)) |> 
  mutate(have_child = case_when(ch001_ > 0 ~ 1,
                                ch001_ == 0 ~ 0,
                                .default = NA),
         have_chronic = case_when(chronic_mod > 0 ~ 1,
                                  chronic_mod == 0 ~ 0,
                                  .default = NA),
         extraversion = case_when(bfi10_extra_mod < 0 ~ NA,
                                  .default = bfi10_extra_mod),
         neuroticism = case_when(bfi10_neuro_mod < 0 ~ NA,
                                 .default = bfi10_neuro_mod),
         employment = case_when(ep005_ == 3 ~ "unemployed",
                                ep005_ == 1 ~ "retired",
                                ep005_ == 2 ~ "employed",
                                ep005_ %in% c(4, 5, 97) ~ "other",
                                .default = NA)) |> 
  mutate(depress = case_when(eurod < 0 ~ NA,
                             .default = eurod)) |> 
  mutate(smoking = case_when(smoking == 1 ~ 1,
                             smoking == 5 ~ 0,
                             .default = NA)) |> 
  mutate(across(c(marry, employment), factor)) |> 
  select(id, wave, depress, smoking, age, years_edu, marry,
         have_child, have_chronic, extraversion, neuroticism, employment) |> 
  drop_na()

ols <- lm(depress ~ smoking + age + years_edu + marry + have_child
          + have_chronic + extraversion + neuroticism + employment, data = data_cleaned)  
modelsummary(ols)

twfe <- plm(depress ~ smoking + age + years_edu + marry + have_child
            + have_chronic + extraversion + neuroticism + employment,
            data = data_cleaned,
            index = c("id", "wave"),
            model = "within", effect = "twoways")
modelsummary(twfe)


















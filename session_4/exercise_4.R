library(tidyverse)
library(forcats)
library(haven)
library(survival)
library(survminer)
library(lmtest)
library(gt)
library(table1)
library(gtsummary)
library(forestmodel)

output_path <- "session_4/output"


#-------------------------------------------------------------------
# Exercise 4.2
#-------------------------------------------------------------------
fix_data <- read_dta("data/FIX.dta")
varying_data <- read_dta("data/VARYING.dta") |> 
  arrange(ID, SYEAR)
calen_data <- read_dta("data/CALEN.dta") |> 
  arrange(ID, SYEAR, BEGIN)

merged_data <- fix_data |> 
  inner_join(varying_data, by = "ID") |>
  inner_join(calen_data, by = c("ID", "SYEAR")) |> 
  arrange(ID, SYEAR)

merged_data <- merged_data |> 
  mutate(mig = case_when(MIGBACK == 1 ~ "Native",
                         MIGBACK == 2 ~ "Migrants",
                         .default = NA)) |> 
  mutate(gender = case_when(SEX == 1 ~ "Men",
                            SEX == 2 ~ "Women",
                            .default = NA)) |> 
  mutate(edu = case_when(PGISCED97 == 1 ~ "Low",
                         PGISCED97 == 2 ~ "Low",
                         PGISCED97 == 3 ~ "Medium",
                         PGISCED97 == 4 ~ "Medium",
                         PGISCED97 == 5 ~ "Medium",
                         PGISCED97 == 6 ~ "High",
                         .default = NA)) |> 
  mutate(fam = case_when(PGFAMSTD == 1 ~ "Married",
                         PGFAMSTD == 2 ~ "Single",
                         PGFAMSTD == 3 ~ "Single",
                         PGFAMSTD == 4 ~ "Single",
                         .default = "Other")) |> 
  mutate(age = case_when(AGE >= 50 ~ "50-64",
                         AGE >= 40 ~ "40-49",
                         AGE >= 30 ~ "30-39",
                         AGE >= 25 ~ "25-29",
                         AGE >= 17 ~ "17-24",
                         .default = NA)) |> 
  mutate(across(c(mig, gender, fam, age), as.factor)) |> 
  mutate(edu = factor(edu, levels = c("Low", "Medium", "High"))) |> 
  drop_na(c(ID, BEGIN, END, EVENT, mig, gender, edu, fam, edu)) |> 
  mutate(surv = Surv(BEGIN, END, EVENT == 1))

table1::label(merged_data$edu) <- "Education"
tab_1 <- table1(~ edu | age, data = merged_data)


fit_1 <- survfit(merged_data$surv ~ edu + age, data = merged_data)
fig_1 <- ggsurvplot_facet(fit_1, data = merged_data,
                          facet.by = "edu",
                          short.panel.labs = TRUE,
                          title = paste0("Figure 1: Survival function of leaving ",
                                         "unemployment by education and age"),
                          xlab = "Unemployment duration in month",
                          # xlim = c(15, 50),
                          palette = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#000000"),
                          censor = FALSE,
) + 
  theme_bw() +
  theme(
    text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )
ggsave(file.path(output_path, "figure_1.png"), fig_1, width = 8, height = 6)


#-------------------------------------------------------------------
# Exercise 4.3
#-------------------------------------------------------------------
merged_data <- merged_data |> 
  mutate(int = interaction(merged_data$edu, merged_data$age))

model_interaction <- coxph(merged_data$surv ~ int + gender + mig + fam,
                           data = as.data.frame(merged_data))
title_tab2 <- paste0("Table 2: Cox model estimates of hazard ratios of ",
                     "leaving unemployment")
tab_2 <- tbl_regression(model_interaction, exponentiate = TRUE) |> 
  as_gt() |> 
  tab_header(title = title_tab2) |> 
  tab_options(table.width = pct(70)) |> 
  tab_options(
    table.border.top.style = "none"
  )
gtsave(tab_2, "table_2.html", path = output_path)

fig_2 <- ggforest(model_interaction, data = NULL, fontsize = 0.9)
ggsave(file.path(output_path, "figure_2.png"), fig_2, width = 8, height = 8)


model_no_interaction <- coxph(merged_data$surv ~  edu + age + gender + mig + fam,
                              data = as.data.frame(merged_data))
lrtest(model_no_interaction, model_interaction)


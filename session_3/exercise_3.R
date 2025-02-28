library(tidyverse)
library(haven)
library(survival)
library(gt)
library(table1)
library(modelsummary)

output_path <- "session_3/output"

dhs_data <- read_dta("data/PHIR71FL_SMALL.DTA") |> 
  filter(!is.na(TIME) & !is.na(EVENT))

#-------------------------------------------------------------------
# Exercise 3.1
#-------------------------------------------------------------------
dhs_data <- dhs_data |> 
  mutate(cohort = case_when(v010 <= 1980 ~ "1967 - 1980",
                            v010 > 1980 ~ "1981 - 2000",
                            .default = NA)) |> 
  mutate(genkid = case_when(GENKIDS == 1 ~ "Boys",
                            GENKIDS == 2 ~ "Girls",
                            GENKIDS == 3 ~ "Mix",
                            .default = NA)) |> 
  mutate(edu = case_when(v106 == 0 ~ "No education",
                         v106 %in% c(1, 2, 3) ~ "Some education",
                         .default = NA)) |> 
  mutate(urban = case_when(v025 == 1 ~ "Urban",
                           v025 == 2 ~ "Rural",
                           .default = NA)) |> 
  mutate(age_first_birth = cut(v212,
                               breaks = c(10, 19, 29, 40),
                               labels = c("11-19", "20-29", "30-40"),
                               include.lowest = TRUE,)) |> 
  mutate(across(c(cohort, genkid, edu, urban, age_first_birth), as.factor))

table1::label(dhs_data$edu) <- "Education"
table1::label(dhs_data$urban) <- "Urbanity"
table1::label(dhs_data$cohort) <- "Cohort"
table1(~ urban + edu | cohort , data = dhs_data,
       caption = "Table 1: Sample statistics by education and urbanity")

#-------------------------------------------------------------------
# Exercise 3.3
#-------------------------------------------------------------------
cm <- c(
  "genkidGirls" = "Girls",
  "genkidMix" = "Mix",
  "eduSome education" = "Some education",
  "cohort1981 - 2000" = "1981-2000",
  "age_first_birth20-29" = "20-29",
  "age_first_birth30-40" = "30-40"
)
model_total <- coxph(Surv(TIME, EVENT) ~ genkid + edu + cohort + age_first_birth,
                     data = dhs_data)

title_tab2 <- paste0("Table 2: Cox model estimates of hazard ratios of ",
                     "having third child")
summary_total <- modelsummary(list("Overall" = model_total), 
                              exponentiate = TRUE,
                              fmt = 2,
                              statistic = "conf.int",
                              conf_level = .95,
                              coef_map = cm,
                              gof_map = c("nobs"),
                              output = "gt",
                              ) |> 
  tab_row_group(rows = 1:4, label = "Gender of first two kids (Ref: Boys)") |> 
  tab_row_group(rows = 5:6, label = "Education (Ref: no education)") |>
  tab_row_group(rows = 7:8, label = "Cohort (Ref: 1967-1980)") |>
  tab_row_group(rows = 9:12, label = "Age at first birth (Ref: 11-19)") |>
  row_group_order(groups = c("Gender of first two kids (Ref: Boys)",
                             "Education (Ref: no education)",
                             "Cohort (Ref: 1967-1980)",
                             "Age at first birth (Ref: 11-19)")) |> 
  tab_options(table.width = pct(70)) |> 
  tab_header(
    title = title_tab2
  ) |> 
  tab_options(
    table.border.top.style = "none"
  )
gtsave(summary_total, "table_2.html", path = output_path)


#-------------------------------------------------------------------
# Exercise 3.4
#-------------------------------------------------------------------
model_urban <- coxph(Surv(TIME, EVENT) ~ genkid + edu + cohort + age_first_birth,
                     data = dhs_data |> filter(urban == "Urban"))
model_rural <- coxph(Surv(TIME, EVENT) ~ genkid + edu + cohort + age_first_birth,
                     data = dhs_data |> filter(urban == "Rural"))

model_list <- list(
  "Urban" = model_urban,
  "Rural" = model_rural
)

title_tab3 <- paste0("Table 3: Cox model estimates of hazard ratios of ",
                     "having third child by urbanity")
summary_urban <- modelsummary(model_list, 
                              exponentiate = TRUE,
                              fmt = 2,
                              statistic = "conf.int",
                              conf_level = .95,
                              coef_map = cm,
                              gof_map = c("nobs"),
                              output = "gt",
) |> 
  tab_row_group(rows = 1:4, label = "Gender of first two kids (Ref: Boys)") |> 
  tab_row_group(rows = 5:6, label = "Education (Ref: no education)") |>
  tab_row_group(rows = 7:8, label = "Cohort (Ref: 1967-1980)") |>
  tab_row_group(rows = 9:12, label = "Age at first birth (Ref: 11-19)") |>
  row_group_order(groups = c("Gender of first two kids (Ref: Boys)",
                             "Education (Ref: no education)",
                             "Cohort (Ref: 1967-1980)",
                             "Age at first birth (Ref: 11-19)")) |> 
  tab_options(table.width = pct(70)) |> 
  tab_header(
    title = title_tab3
  ) |> 
  tab_options(
    table.border.top.style = "none"
  )
gtsave(summary_urban, "table_3.html", path = output_path)





















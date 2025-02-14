library(tidyverse)
library(haven)
library(survival)
library(survminer)
library(broom)
library(gt)
library(glue)

output_path <- "session_1/output"

lfp_data <- read_csv("data/oecd_labor_force_participation_rate.csv",)
lfp_plot <- lfp_data |> 
  ggplot(aes(x = TIME_PERIOD, y = OBS_VALUE)) +
  geom_line(aes(color = factor(Sex)), linewidth = 1) +
  theme_minimal() +
  scale_color_manual(name = "",
                     labels = c("Female", "Male"),
                     values = c("#F8766D", "#00BFC4")) +
  labs(x = "Year", y = "Participation rate (%)",
       title = "Figure 1: Labor force participation rate by gender (OECD)") +
  theme(legend.position = "bottom",
        )
ggsave(file.path(output_path, "figure_1.png"), lfp_plot, width = 8, height = 6)

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
  mutate(gender = as.factor(case_when(gndr == 1 ~ "Male",
                                      gndr == 2 ~ "Female",
                                      .default = NA))) |> 
  mutate(cohort = cut(yrbrn,
                      breaks = c(1919, 1945, 1960, 1980, 1999),
                      labels = c("1920-1945", "1946-1960", "1961-1980", "1981-1999"),
                      include.lowest = TRUE,
                      ))

#-------------------------------------------------------------------------------
# Fit Kaplan-Meier estimator
#-------------------------------------------------------------------------------
# Gender
fit_gender <- survfit(Surv(data$time, data$event) ~ data$gender)
gender_plot <- ggsurvplot(fit_gender, data = data,
                          title = "Figure 2: Survival probability by gender",
                          xlab = "Age"
                          )
ggsave(file.path(output_path, "figure_2.png"), gender_plot$plot, width = 8, height = 6)

# Cohort
fit_cohort <- survfit(Surv(data$time, data$event) ~ data$cohort)
cohort_plot <- ggsurvplot(fit_cohort, data = data,
                          title = "Figure 3: Survival probability by age cohort",
                          xlab = "Age")
ggsave(file.path(output_path, "figure_3.png"), cohort_plot$plot, width = 8, height = 6)


# Gender * cohort
fit_gender_cohort <- survfit(Surv(time, event) ~ gender + cohort, data = data)
gender_cohort_plot <- ggsurvplot_facet(fit_gender_cohort, data = data,
                                       facet.by = "gender",
                                       short.panel.labs = TRUE,
                                       title = "Figure 4: Survival probability by age and cohort",
                                       xlab = "Age"
                                       )
ggsave(file.path(output_path, "figure_4.png"), gender_cohort_plot, 
       width = 8, height = 6)

# Save median table
median_tab <- as.data.frame(summary(fit_gender_cohort)$table) |> 
  rownames_to_column(var = "Strata") |> 
  rename("Median" = median) |> 
  select(Strata, Median) |> 
  gt() |> 
  tab_header("Table 1: Median age of nest-leaving by gender and cohort")
gtsave(median_tab, file.path(output_path, "table_1.html"))  

# Compare survival probabilities at 20 and 28
gender_cohort_fitted <- broom::tidy(fit_gender_cohort)

estimate_gender_cohort_20 <- gender_cohort_fitted |> 
  filter(time == 20) |> 
  mutate(gender = str_extract(strata, "(?<=gender=)\\w+"),
         cohort = str_extract(strata, "(?<=cohort=)\\d{4}-\\d{4}")) |> 
  select(estimate, gender, cohort) |> 
  pivot_wider(names_from = gender, values_from = estimate) |> 
  mutate(diff = Female - Male)
comp_table_20 <- estimate_gender_cohort_20 |> 
  rename("Cohort" = cohort,
         "Difference" = diff) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  gt::gt() |> 
  tab_header("Table 2: Survival probability at age 20 by gender and cohort")
gtsave(comp_table_20, file.path(output_path, "table_2.html"))  


estimate_gender_cohort_28 <- gender_cohort_fitted |> 
  filter(time == 28) |> 
  mutate(gender = str_extract(strata, "(?<=gender=)\\w+"),
         cohort = str_extract(strata, "(?<=cohort=)\\d{4}-\\d{4}")) |> 
  select(estimate, gender, cohort) |> 
  pivot_wider(names_from = gender, values_from = estimate) |> 
  mutate(diff = Female - Male)
comp_table_28 <- estimate_gender_cohort_28 |> 
  rename("Cohort" = cohort,
         "Difference" = diff) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  gt::gt() |> 
  tab_header("Table 3: Survival probability at age 28 by gender and cohort")
gtsave(comp_table_28, file.path(output_path, "table_3.html"))  


#-------------------------------------------------------------------------------
# Appendix table
#-------------------------------------------------------------------------------
tab_a1 <- tidy(fit_gender) |> 
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  mutate(strata = str_replace(strata, "data\\$gender=", "")) |> 
  gt() |> 
  tab_header("Table A1: Survival function by gender") |> 
  tab_options(table.width = pct(100))
tab_a2 <- tidy(fit_cohort) |> 
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  mutate(strata = str_replace(strata, "data\\$cohort=", "")) |> 
  gt() |> 
  tab_header("Table A2: Survival function by cohort") |> 
  tab_options(table.width = pct(100))
tab_a3 <- tidy(fit_gender_cohort) |> 
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> 
  mutate(strata = str_replace_all(strata, c("gender=" = "",
                                            "cohort=" = "",
                                            ", " = " * "))) |> 
  gt() |> 
  tab_header("Table A3: Survival function by gender and cohort") |> 
  tab_options(table.width = pct(100))

all_tabs <- list(tab_a1, tab_a2, tab_a3)
map(seq_along(all_tabs), \(x) {
  gtsave(all_tabs[[x]], file.path(output_path, glue("table_{x}.html")))
})






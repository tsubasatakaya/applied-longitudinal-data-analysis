library(tidyverse)
library(haven)
library(survival)
library(survminer)
library(broom)
library(gt)
library(glue)

output_path <- "session_2/output"
base_data <- read_dta("data/ESS9e03_2.dta") |> 
  as_tibble() |> 
  filter(cntry == "FR") |> 
  filter(yrbrn >= 1930 & yrbrn <= 1989) |> 
  mutate(gender = as.factor(case_when(gndr == 1 ~ "Male",
                                      gndr == 2 ~ "Female",
                                      .default = NA))) |> 
  mutate(cohort = cut(yrbrn,
                      breaks = c(1929, 1949, 1969, 1989),
                      labels = c("1930-1949", "1950-1969", "1970-1989"),
                      include.lowest = TRUE,)
         )
  
#-------------------------------------------------------------------
# Codebook
# lvpntyr: year of leaving home for 2 months or more
# pdempyr: year first started in paied employment or apprenticeship 
# maryr: year first married
# fcldbrn: year first child was born
#-------------------------------------------------------------------

data <- base_data |> 
  select(fcldbrn, bthcld, yrbrn, cntry, inwyys, gndr) |> 
  filter(!is.na(bthcld))

data <- data |> 
  mutate(event = case_when(bthcld == 2 ~ 0,
                           .default = 1)) |>
  mutate(time = case_when(is.na(fcldbrn) ~ inwyys - yrbrn,
                          .default = fcldbrn - yrbrn)) |> 
  mutate(gender = as.factor(case_when(gndr == 1 ~ "Male",
                                      gndr == 2 ~ "Female",
                                      .default = NA))) |>
  mutate(cohort = cut(yrbrn,
                      breaks = c(1929, 1949, 1969, 1989),
                      labels = c("1930-1949", "1950-1969", "1970-1989"),
                      include.lowest = TRUE,)
         ) |> 
  filter(time < 60)


fit <- survfit(Surv(time, event) ~ gender + cohort, data = data)
fit

ggsurvplot_facet(fit, data = data,
                 facet.by = "gender",
                 short.panel.labs = TRUE,
                 # title = "Figure 4: Survival probability by age and cohort",
                 # xlab = "Age"
)



#-------------------------------------------------------------------
# Exercise 2.3: marriage
#-------------------------------------------------------------------
# a
data <- base_data |> 
  select(maryr, evmar, yrbrn, cntry, inwyys, gender, cohort) |> 
  filter(!is.na(evmar))

data <- data |> 
  mutate(event = case_when(evmar == 2 ~ 0,
                           .default = 1)) |> 
  mutate(time = case_when(is.na(maryr) ~ inwyys - yrbrn,
                          .default = maryr - yrbrn))
fit_marriage <- survfit(Surv(time, event) ~ gender + cohort, data = data)
fig_1 <- ggsurvplot_facet(fit_marriage, data = data,
                          facet.by = "gender",
                          short.panel.labs = TRUE,
                          title = paste0("Figure 1: Survival function of marriage ",
                                         "by gender and cohort, France"),
                          xlab = "Age",
                          xlim = c(15, 70),
                          palette = c("#E69F00", "#56B4E9", "#009E73"),
                          censor.shape = "|",
                          censor.size = 3,
                          legend.title = "Cohort: ",
                          legend.labs = c("1930-1949", "1950-1969", "1970-1989"),
                          ) + 
  theme_bw() +
  theme(
    text = element_text(size = 14),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )
ggsave(file.path(output_path, "figure_1.png"), fig_1, width = 8, height = 6)

# b
data_young <- data |> 
  filter(cohort == "1930-1949") |> 
  filter(time < 60)

fit_young <- survfit(Surv(time, event) ~ gender, data = data_young)
ggsurvplot(fit_young, data = data_young)


survdiff(Surv(time, event) ~ gender, data = data_young)















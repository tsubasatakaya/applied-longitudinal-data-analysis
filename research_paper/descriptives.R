source("research_paper/setup.R")
source("research_paper/process_data.R")

#===============================================================================
# Static TWFE
#===============================================================================
################################################
# Sample statistics (on person-year level)
################################################
table1::label(data_processed$sex) <- "Sex"
table1::label(data_processed$age) <- "Age"
table1::label(data_processed$edu) <- "Education"
table1::label(data_processed$has_kid) <- "Child status"
table1::label(data_processed$emp) <- "Employment status"
table1::label(data_processed$log_income) <- "Log income"
table1::label(data_processed$depression) <- "Depression"
desc_tab <- table1(~ sex + age + edu + has_kid + emp + log_income 
                   + depression | partnership,
                   data = data_processed)
desc_tab

print(paste0("Number of person years: ", nrow(data_processed)))
print(paste0("Number of persons: ", length(unique(data_processed$id))))

################################################
# Within variation
################################################
variables <- c("age", "edu", "has_kid", "emp", "log_income", "depression")

within_summary_data <- data_processed |> 
  mutate(across(all_of(variables), ~ as.numeric(.x) - mean(as.numeric(.x), na.rm = TRUE),
                .names = "{.col}_demeaned"),
         .by = id) |> 
  select(dplyr::ends_with("demeaned")) |> 
  pivot_longer(dplyr::ends_with("demeaned"), names_to = "variable", values_to = "value") |> 
  mutate(variable = str_replace(variable, "_demeaned", "")) |> 
  group_by(variable) |> 
  summarize(within_sd = sd(value),
            within_min = min(value),
            within_max = max(value))


#===============================================================================
# Dynamic TWFE
#===============================================================================
data_processed_dynamic <- data_processed |> 
  filter(single_first_wave) |> 
  mutate(relative_time = ifelse(treated == 1, 
                                wave - first_transition_wave + 1, 0)) |> 
  mutate(relative_time = factor(relative_time, 
                                levels = c(0, unique(relative_time[relative_time != 0])))) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)

################################################
# Sample statistics (on person-year level)
################################################
table1::label(data_processed_dynamic$sex) <- "Sex"
table1::label(data_processed_dynamic$age) <- "Age"
table1::label(data_processed_dynamic$edu) <- "Education"
table1::label(data_processed_dynamic$has_kid) <- "Child status"
table1::label(data_processed_dynamic$emp) <- "Employment status"
table1::label(data_processed_dynamic$log_income) <- "Log income"
table1::label(data_processed_dynamic$depression) <- "Depression"
desc_tab_dynamic <- table1(~ sex + age + edu + has_kid + emp + log_income 
                           + depression | partnership,
                           data = data_processed_dynamic)
desc_tab_dynamic

print(paste0("Number of person years: ", nrow(data_processed_dynamic)))
print(paste0("Number of persons: ", length(unique(data_processed_dynamic$id))))

################################################
# Within variation
################################################
variables <- c("age", "edu", "has_kid", "emp", "log_income", "depression")

within_summary_data_dynamic <- data_processed_dynamic |> 
  mutate(across(all_of(variables), ~ as.numeric(.x) - mean(as.numeric(.x), na.rm = TRUE),
                .names = "{.col}_demeaned"),
         .by = id) |> 
  select(dplyr::ends_with("demeaned")) |> 
  pivot_longer(dplyr::ends_with("demeaned"), names_to = "variable", values_to = "value") |> 
  mutate(variable = str_replace(variable, "_demeaned", "")) |> 
  group_by(variable) |> 
  summarize(within_sd = sd(value),
            within_min = min(value),
            within_max = max(value))

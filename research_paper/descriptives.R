source("research_paper/setup.R")
source("research_paper/process_data.R")
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
calculate_within_statistics <- function(x) {
  
}
data_processed |> 
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

source("research_paper/setup.R")

################################################
# Data preparation
################################################
data <- read_dta("data/PAIRFAM.dta") |> 
  mutate(n_wave = n_distinct(wave), .by = id) |> 
  filter(n_wave >= 2) |>   # stay in the panel for at least two waves
  arrange(id, wave)

data_filtered <- data |> 
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
data_cleaned <- data_filtered |> 
  mutate(sex = case_when(SEX == 1 ~ "Male",
                         SEX == 2 ~ "Female",
                         .default = NA)) |> 
  mutate(emp = case_when(EMP == 1 ~ "Full-time",
                         EMP == 2 ~ "Part-time",
                         EMP == 3 ~ "Not working",
                         .default = NA)) |> 
  mutate(edu = case_when(CASMIN == 0 ~ "In school",
                         CASMIN == 1 ~ "Low",
                         CASMIN == 2 ~ "Medium",
                         CASMIN == 3 ~ "High")) |> 
  mutate(has_kid = case_when(nkids > 0 ~ "Has kid",
                             nkids == 0 ~ "No kid",
                             .default = NA)) |> 
  mutate(partnership = case_when(FAM_NOW == 0 ~ "Single",
                                 FAM_NOW == 1 ~ "LAT",
                                 FAM_NOW == 2 ~ "Cohabitation",
                                 FAM_NOW == 3 ~ "Married",
                                 .default = NA)) |> 
  mutate(log_income = log(hhincoecd)) |> 
  rename(age = "AGE",
         depression = "DEPRESSION",
         life_sat = "sat6") |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         emp = factor(emp, levels = c("Not working", "Part-time", "Full-time")),
         edu = factor(edu, levels = c("In school", "Low", "Medium", "High")),
         partnership = factor(partnership, levels = c("Single", "LAT", 
                                                      "Cohabitation", "Married")),
         has_kid = factor(has_kid, levels = c("No kid", "Has kid"))) |> 
  drop_na(id, inty, wave, sex, life_sat, partnership, emp,
          edu, age, has_kid, log_income, depression)


################################################
# Sample statistics
################################################
table1::label(data_cleaned$sex) <- "Sex"
table1::label(data_cleaned$age) <- "Age"
table1::label(data_cleaned$edu) <- "Education"
table1::label(data_cleaned$has_kid) <- "Child status"
table1::label(data_cleaned$emp) <- "Employment status"
table1::label(data_cleaned$log_income) <- "Log income"
table1::label(data_cleaned$depression) <- "Depression"
desc_tab <- table1(~ sex + age + edu + has_kid + emp + log_income 
                   + depression | partnership,
                   data = data_cleaned)
desc_tab

################################################
# Fit static two-way fixed effects model
################################################
controls <- c("age", "edu", "has_kid", "emp", "log_income", "depression")
twfe_formula <- as.formula(
  paste0("life_sat ~ ", "partnership +", paste0(controls, collapse = "+"))
)


static_twfe_male <- plm(twfe_formula, 
                        data = data_cleaned |> filter(sex == "Male"),
                        index = c("id", "wave"),
                        model = "within",
                        effect = "twoways")
static_twfe_female <- plm(twfe_formula,
                          data = data_cleaned |> filter(sex == "Female"),
                          index = c("id", "wave"),
                          model = "within",
                          effect = "twoways")  


################################################
# Plot coefficients
################################################
single_coef <- tibble(
  term = "Single (ref.)",
  estimate = 0,
  conf.low = 0,
  conf.high = 0
)
coef_data <- tidy(static_twfe_male, conf.int = TRUE) |> 
  mutate(sex = "male") |> 
  bind_rows(tidy(static_twfe_female, conf.int = TRUE) |> 
              mutate(sex = "female")) |> 
  mutate(term = str_replace(term, "partnership", "")) |> 
  filter(term %in% c("LAT", "Cohabitation", "Married")) |> 
  bind_rows(single_coef) |> 
  mutate(term = factor(term, levels = c("Single (ref.)", "LAT", "Cohabitation", "Married")))

sta_twfe_coef_plot <- ggplot(coef_data, 
                             aes(x = term)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = sex), linewidth = 1,
                 position = position_dodge(width = 0.15)) +
  geom_point(aes(x = term, y = estimate, color = sex), size = 3,
             position = position_dodge(width = 0.15)) +
  theme_minimal() +
  labs(x = "", y = "Coefficient") +
  scale_color_manual(name = "",
                     values = c("#c00000", "#5488be"),
                     labels = c("Female", "Male"),
                     breaks = c("female", "male")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12,),
        axis.title.y = element_text(margin = margin(0,7,0,0)),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 10))
sta_twfe_coef_plot
ggsave(file.path(output_path, "static_twfe_coef_plot.png"),
       sta_twfe_coef_plot, width = 7, height = 5, units = "in", dpi = 300)


################################################
# Summary table
################################################
cm <- c(
  "partnershipLAT" = "LAT",
  "partnershipCohabitation" = "Cohabitation",
  "partnershipMarried" = "Married",
  "eduLow" = "Low",
  "eduMedium" = "Medium",
  "eduHigh" = "High",
  "has_kidHas kid" = "Has kid",
  "empPart-time" = "Part-time",
  "empFull-time" = "Full-time",
  "age" = "Age",
  "log_income" = "Log income",
  "depression" = "Depression"
)
gof_f <- function(x) format(round(x, 2), big.mark = ",")
gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = gof_f),
  list("raw" = "r.squared", "clean" = "R\U00B2", "fmt" = gof_f)
  )
modelsummary(list("Male" = static_twfe_male,
                  "Female" = static_twfe_female), fmt = 2,
             coef_map = cm, gof_map = gm,
             output = "gt") |> 
  tab_row_group(
    label = "Partnership type (Ref: single)",
    rows = 1:6
  ) |> 
  tab_row_group(
    label = "Education (Ref: in school)",
    rows = 9:14
  ) |> 
  tab_row_group(
    label = "Child status (Ref: no kid)",
    rows = 15:16
  ) |> 
  tab_row_group(
    label = "Employment status (Ref: not working)",
    rows = 17:20
  ) |> 
  tab_row_group(
    label = "Continuous scale",
    rows = c(7, 8, 21:24)
  ) |>
  row_group_order(groups = c("Partnership type (Ref: single)",
                             "Education (Ref: in school)",
                             "Child status (Ref: no kid)",
                             "Employment status (Ref: not working)",
                             "Continuous scale")) |> 
  tab_options(heading.align = "left",
              table.font.size = "10pt",
              table.width = pct(60),)


  
  
  
  
  
  
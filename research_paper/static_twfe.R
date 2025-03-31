source("research_paper/setup.R")
source("research_paper/process_data.R")

data_full <- data_processed
data_rest <- data_processed |> 
  filter(single_first_wave) |> 
  mutate(relative_time = ifelse(treated == 1, 
                                wave - first_transition_wave + 1, 0)) |> 
  mutate(relative_time = factor(relative_time, 
                                levels = c(0, unique(relative_time[relative_time != 0])))) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)

# Modelsummary configuration
cm <- c(
  "partnershipLAT" = "LAT",
  "partnershipCohabiting" = "Cohabiting",
  "partnershipMarried" = "Married",
  "has_kidHas child" = "Has child",
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

#===============================================================================
# Baseline pooled OLS
#===============================================================================
controls <- c("age", "has_kid", "emp", "log_income", "depression")
formula <- as.formula(
  paste0("life_sat ~ ", "partnership +", paste0(controls, collapse = "+"))
)
pooled_ols_male_full <- lm(formula,
                           data = data_full |> filter(sex == "Male"))
pooled_ols_female_full <- lm(formula,
                             data = data_full |> filter(sex == "Female"))
pooled_ols_male_rest <- lm(formula,
                           data = data_rest |> filter(sex == "Male"))
pooled_ols_female_rest <- lm(formula,
                             data = data_rest |> filter(sex == "Female"))


################################################
# Summary table
################################################
modelsummary(list("(1)" = pooled_ols_male_full,
                  "(2)" = pooled_ols_female_full,
                  "(3)" = pooled_ols_male_rest,
                  "(4)" = pooled_ols_female_rest), 
             fmt = 2,
             coef_map = cm, gof_map = gm,
             output = "gt") |> 
  tab_spanner(
    label = "Male",
    columns = c(2, 4),
    gather = FALSE
  ) |> 
  tab_spanner(
    label = "Female",
    columns = c(3, 5),
    gather = FALSE
  ) |> 
  tab_spanner(
    label = "Full sample",
    columns = 2:3
  ) |> 
  tab_spanner(
    label = "Restricted sample",
    columns = 4:5
  ) |> 
  tab_row_group(
    label = "Partnership type (Ref: single)",
    rows = 1:6
  ) |> 
  tab_row_group(
    label = "Child status (Ref: no child)",
    rows = 7:8
  ) |> 
  tab_row_group(
    label = "Employment status (Ref: not working)",
    rows = 9:12
  ) |> 
  tab_row_group(
    label = "Continuous scale",
    rows = c(13:18)
  ) |>
  row_group_order(groups = c("Partnership type (Ref: single)",
                             "Child status (Ref: no child)",
                             "Employment status (Ref: not working)",
                             "Continuous scale")) |> 
  tab_options(
    table.width = pct(60)
  )


#===============================================================================
# Baseline static two-way fixed effects by gender
#===============================================================================

################################################
# Fit static two-way fixed effects model in loop
################################################
formula <- as.formula(
  paste0("life_sat ~ ", "partnership +", paste0(controls, collapse = "+"))
)

static_twfe_all <- list()
gender_tag <- rep(c("Male", "Female"), 4)

for (i in seq_along(gender_tag)) {
  if ((i - 1) %/% 2 == 0) {
    selected_controls <- controls[!controls %in% c("has_kid", "emp")]
  } else if ((i - 1) %/% 2 == 1) {
    selected_controls <- controls[!controls %in% c("has_kid")]
  } else if ((i - 1) %/% 2 == 2) {
    selected_controls <- controls[!controls %in% c("emp")]
  } else {
    selected_controls <- controls
  }
  
  formula <- as.formula(
    paste0("life_sat ~ ", "partnership +", paste0(selected_controls, collapse = "+"))
  )
  
  twfe <- plm(formula,
              data = data_full |>  filter(sex == gender_tag[i]),
              index = c("id", "wave"),
              model = "within",
              effect = "twoways")
  
  static_twfe_all[[paste0("(", i, ")")]] <- twfe
}

# Restricted sample
formula <- as.formula(
  paste0("life_sat ~ ", "partnership +", paste0(controls[!controls %in% c("has_kid", "emp")], 
                                                collapse = "+"))
)
static_twfe_all[["(9)"]] <- plm(formula,
                                data = data_rest |>  filter(sex == "Male"),
                                index = c("id", "wave"),
                                model = "within",
                                effect = "twoways")
static_twfe_all[["(10)"]] <- plm(formula,
                                 data = data_rest |>  filter(sex == "Female"),
                                 index = c("id", "wave"),
                                 model = "within",
                                 effect = "twoways")


################################################
# Summary table
################################################
modelsummary(static_twfe_all, 
             fmt = 2,
             coef_map = cm, gof_map = gm,
             output = "gt") |> 
  tab_spanner(
    label = "Male",
    columns = seq(2, 10, 2),
    gather = FALSE
  ) |> 
  tab_spanner(
    label = "Female",
    columns = seq(3, 11, 2),
    gather = FALSE
  ) |> 
  tab_spanner(
    label = "Full sample",
    columns = 2:9
  ) |> 
  tab_spanner(
    label = "Restricted sample",
    columns = 10:11
  ) |>
  tab_row_group(
    label = "Partnership type (Ref: single)",
    rows = 1:6
  ) |> 
  tab_row_group(
    label = "Child status (Ref: no child)",
    rows = 7:8
  ) |> 
  tab_row_group(
    label = "Employment status (Ref: not working)",
    rows = 9:12
  ) |> 
  tab_row_group(
    label = "Continuous scale",
    rows = c(13:18)
  ) |>
  row_group_order(groups = c("Partnership type (Ref: single)",
                             "Child status (Ref: no child)",
                             "Employment status (Ref: not working)",
                             "Continuous scale")) |> 
  tab_options(
    table.width = pct(80)
  )

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
  filter(term %in% c("LAT", "Cohabiting", "Married")) |> 
  bind_rows(single_coef) |> 
  mutate(term = factor(term, levels = c("Single (ref.)", "LAT", "Cohabiting", "Married")))

sta_twfe_coef_plot <- ggplot(coef_data, 
                             aes(x = term)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, color = sex), linewidth = 1,
                 position = position_dodge(width = 0.15)) +
  geom_point(aes(x = term, y = estimate, color = sex), size = 3,
             position = position_dodge(width = 0.15)) +
  theme_minimal() +
  labs(x = "", y = "Effect of partnership",
       title = "Effect of partnership status on life satisfaction by partnership type (relative to single)") +
  scale_color_manual(name = "",
                     values = c("#c00000", "#5488be"),
                     labels = c("Female", "Male"),
                     breaks = c("female", "male")) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12,),
        axis.title.y = element_text(margin = margin(0,7,0,0)),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 10))
sta_twfe_coef_plot
ggsave(file.path(output_path, "static_twfe_coef_plot.png"),
       sta_twfe_coef_plot, width = 9, height = 6, units = "in", dpi = 300)



#===============================================================================
# Moderated by partnership spell
#===============================================================================
twfe_int_duration_formula <- as.formula(
  paste0("life_sat ~ ", "partner_duration * partnership +", 
         paste0(controls, collapse = "+"))
)


static_twfe_int_duration_male <- plm(twfe_int_duration_formula, 
                                     data = data_processed |> filter(sex == "Male"),
                                     index = c("id", "wave"),
                                     model = "within",
                                     effect = "twoways")
static_twfe_int_duration_female <- plm(twfe_int_duration_formula,
                                       data = data_processed |> filter(sex == "Female"),
                                       index = c("id", "wave"),
                                       model = "within",
                                       effect = "twoways")  
modelsummary(list("Male" = static_twfe_int_duration_male,
                  "Female" = static_twfe_int_duration_female), 
             fmt = 2,
             output = "gt")

partial_slope_data <- tibble()
sex_vec <- c("male", "female")
model_list <- list(static_twfe_int_duration_male, static_twfe_int_duration_female)
for (i in seq_along(sex_vec)) {
  slope_data <- avg_slopes(model_list[[i]],
                           variables = "partnership",
                           by = "partner_duration") |> 
    mutate(partnership = str_extract(contrast, "(?<=mean\\().+?(?=\\))"),
           .before = contrast) |> 
    select(partnership, partner_duration, estimate, conf.low, conf.high) |> 
    mutate(sex = sex_vec[i])
  partial_slope_data <- bind_rows(partial_slope_data, slope_data)
}
partial_slope_data <- partial_slope_data |> 
  mutate(partnership = factor(partnership, levels = c("LAT", "Cohabiting", "Married")))


sta_twfe_duration_slope_plot <- partial_slope_data |> 
  mutate(sex = factor(sex, levels = c("female", "male")),
         partnership = factor(partnership, levels = c("LAT", "Cohabiting", "Married"))) |> 
  ggplot(aes(x = partner_duration, y = estimate,)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#7D7D7D") +
  geom_line(aes(color = sex,)) +
  geom_ribbon(aes(x = partner_duration, ymin = conf.low, ymax = conf.high,
                  fill = sex),
              alpha = 0.2) +
  theme_bw() +
  labs(x = "Partnership duration in waves", 
       y = "Effect of parternship",
       title = "Effect of each partnership status on life satisfaction by duration") +
  scale_x_continuous(breaks = seq(0, 10, by = 2),
                     limits = c(0, 10)) +
  scale_color_manual(name = "",
                     values = c("#c00000", "#5488be"),
                     labels = c("Female", "Male"),
                     breaks = c("female", "male")) +
  guides(fill = "none") +
  facet_grid(partnership ~ sex,
             labeller = labeller(
               sex = c("female" = "Female",
                       "male" = "Male"),
             )) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        panel.border = element_rect(color = "grey", fill = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 12,),
        axis.title.x = element_text(margin = margin(7,0,0,0)),
        axis.title.y = element_text(margin = margin(0,7,0,0)),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 10))
sta_twfe_duration_slope_plot
ggsave(file.path(output_path, "static_twfe_duration_interation_plot.png"),
       sta_twfe_duration_slope_plot, width = 8, height = 8, units = "in", dpi = 300)












  
  
  
  
  
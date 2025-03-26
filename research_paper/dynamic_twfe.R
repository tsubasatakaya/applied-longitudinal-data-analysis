source("research_paper/setup.R")
################################################
# Data preparation
################################################
data <- read_dta("data/PAIRFAM.dta") |> 
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
  filter(transition_cum <= 1) |> 
  # which partnership status one ends up in
  mutate(partnership_group = max(FAM_NOW), .by = id) |> 
  # treated group or never treated
  mutate(treated = as.integer(partnership_group != 0)) |> 
  # keep only those who were single at first
  filter(first(FAM_NOW) == 0, .by = id) |> 
  mutate(relative_time = ifelse(treated == 1, 
                                wave - first_transition_wave + 1, 0)) |> 
  mutate(relative_time = factor(relative_time, 
                                levels = c(0, unique(relative_time[relative_time != 0]))))

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
  mutate(log_income = log(hhincoecd)) |> 
  rename(age = "AGE",
         depression = "DEPRESSION",
         life_sat = "sat6") |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
         emp = factor(emp, levels = c("Not working", "Part-time", "Full-time")),
         edu = factor(edu, levels = c("In school", "Low", "Medium", "High")),
         has_kid = factor(has_kid, levels = c("No kid", "Has kid"))) |> 
  drop_na(id, inty, wave, sex, life_sat, relative_time, partnership_group, emp,
          edu, age, has_kid, log_income, depression) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)

lat_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 1))
coh_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 2))
marry_data <- data_cleaned |> 
  filter(partnership_group %in% c(0, 3))

all_data <- list(
  "lat" = lat_data,
  "coh" = coh_data,
  "marry" = marry_data
)

################################################
# Fit dynamic two-way fixed effects model for each partnership status
################################################
controls <- c("age", "edu", "has_kid", "emp", "log_income", "depression")
twfe_formula <- as.formula(
  paste0("life_sat ~ ", "relative_time +", paste0(controls, collapse = "+"))
)

dyn_twfe_all <- list()
partnership_vec <- c("lat", "coh", "marry")
for (i in seq_along(partnership_vec)) {
  print(nrow(all_data[[parnership_vec[i]]]))
  dyn_male <- plm(twfe_formula,
                  data = all_data[[parnership_vec[i]]] |> filter(sex == "Male"),
                  index = c("id", "wave"),
                  effect = "twoways")
  dyn_female <- plm(twfe_formula,
                    data = all_data[[parnership_vec[i]]] |> filter(sex == "Female"),
                    index = c("id", "wave"),
                    effect = "twoways")
  dyn_twfe_all[[parnership_vec[i]]] <- list(
    "male" = dyn_male,
    "female" = dyn_female
  )
}

################################################
# Plot event-study coefficients
################################################
plot_dynamic_effects <- function(model_list, xmin, xmax, partnership_label) {
  # Extract coefficients for relative time from plm model for male and female
  # and save number of observations
  coef_df <- tibble()
  nobs_df <- tibble()
  sex_vec <-  names(model_list)
  for (i in seq_along(sex_vec)) {
    # Tidy model results to tibble dataframe
    coef <- tidy(model_list[[sex_vec[i]]], conf.int = TRUE) |> 
      # Only keep effects for relative time
      filter(str_starts(term, "relative_time")) |> 
      # Remove unnecessary variable name prefix
      mutate(term = str_replace(term, "relative_time", "")) |> 
      bind_rows(
        tibble(
          term = "0",
          estimate = 0,
          conf.high = NA,
          conf.low = NA
        )
      ) |> 
      # Add sex type variable
      mutate(sex = sex_vec[i])
    
    coef_df <- bind_rows(coef_df, coef)
    
    # extract number of observations for text in the plot
    nobs_df <- bind_rows(nobs_df, tibble(
      term = xmax + 0.5,
      estimate = -Inf,
      nobs_lab = paste0("N = ", nobs(model_list[[sex_vec[i]]])),
      sex = sex_vec[i]
    ))
  }
  
  # keep only relevant relative time
  coef_df <- coef_df |> 
    mutate(term = as.integer(term)) |> 
    filter(term >= xmin & term <= xmax)
  
  # Plot dynamic effects
  dyn_effect_plot <- coef_df |> 
    ggplot(aes(x = term, color = sex)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(aes(y = estimate)) +
    # geom_line(aes(y = estimate), linewidth = 1) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.3) +
    geom_text(data = nobs_df, 
              aes(x = term, y = estimate, label = nobs_lab),
              hjust = 1, vjust = -1,
              size = 4, color = "black",) +
    theme_bw() +
    labs(x = "Relative wave (0 = one wave before treatment)", 
         y = "Coefficient",
         title = paste0("Dynamic effects of ", partnership_label, 
                        " on life satisfaction (baseline: one wave before partnership start)")) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = 1),
                       limits = c(xmin-0.5, xmax+0.5)) +
    scale_color_manual(name = "",
                       values = c("#c00000", "#5488be"),
                       labels = c("Female", "Male"),
                       breaks = c("female", "male")) +
    facet_wrap(~ sex,
               labeller = labeller(
                 sex = c("female" = "Female",
                         "male" = "Male"),
               )) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          plot.title = element_text(size = 14),
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
 return (dyn_effect_plot) 
}

plot_dynamic_effects(dyn_twfe_all[["marry"]],
                     xmin = -3, xmax = 5,
                     partnership_label = "marriage")



















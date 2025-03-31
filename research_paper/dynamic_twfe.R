source("research_paper/setup.R")
source("research_paper/process_data.R")
################################################
# Data preparation
################################################
data_rest <- data_processed |> 
  filter(single_first_wave) |> 
  mutate(relative_time = ifelse(treated == 1, 
                                wave - first_transition_wave + 1, 0)) |> 
  mutate(relative_time = factor(relative_time, 
                                levels = c(0, unique(relative_time[relative_time != 0])))) |> 
  # Keep only those who remain in at least two waves
  filter(n_distinct(wave) >= 2, .by = id)

################################################
# Subset data
################################################
lat_data <- data_rest |> 
  filter(partnership_group %in% c(0, 1))
coh_data <- data_rest |> 
  filter(partnership_group %in% c(0, 2))
marry_data <- data_rest |> 
  filter(partnership_group %in% c(0, 3))

all_data <- list(
  "lat" = lat_data,
  "coh" = coh_data,
  "marry" = marry_data
)

################################################
# Fit dynamic two-way fixed effects model for each partnership status
################################################
controls <- c("age", "log_income", "depression")
twfe_formula <- as.formula(
  paste0("life_sat ~ ", "relative_time +", paste0(controls, collapse = "+"))
)

dyn_twfe_all <- list()
partnership_vec <- c("lat", "coh", "marry")
for (i in seq_along(partnership_vec)) {
  dyn_male <- plm(twfe_formula,
                  data = all_data[[partnership_vec[i]]] |> filter(sex == "Men"),
                  index = c("id", "wave"),
                  effect = "twoways")
  dyn_female <- plm(twfe_formula,
                    data = all_data[[partnership_vec[i]]] |> filter(sex == "Women"),
                    index = c("id", "wave"),
                    effect = "twoways")
  dyn_twfe_all[[partnership_vec[i]]] <- list(
    "men" = dyn_male,
    "women" = dyn_female
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
    labs(x = "Relative wave (0 = one wave before partnership start)", 
         y = "Effect of partnership",
         title = paste0("Dynamic effects of ", partnership_label, 
                        " on life satisfaction")) +
    scale_x_continuous(breaks = seq(xmin, xmax, by = 1),
                       limits = c(xmin-0.5, xmax+0.5)) +
    scale_color_manual(name = "",
                       values = c("#c00000", "#5488be"),
                       labels = c("Women", "Men"),
                       breaks = c("women", "men")) +
    facet_wrap(~ sex,
               labeller = labeller(
                 sex = c("men" = "Men",
                         "women" = "Women"),
               )) +
    theme(legend.position = "none",
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
 return (dyn_effect_plot) 
}

dyn_lat_plot <- plot_dynamic_effects(dyn_twfe_all[["lat"]],
                                     xmin = -3, xmax = 5,
                                     partnership_label = "LAT")
dyn_coh_plot <- plot_dynamic_effects(dyn_twfe_all[["coh"]],
                                     xmin = -3, xmax = 5,
                                     partnership_label = "cohabitation")
dyn_marry_plot <- plot_dynamic_effects(dyn_twfe_all[["marry"]],
                                       xmin = -1, xmax = 5,
                                       partnership_label = "marriage")

ggsave(file.path(output_path, "dynamic_twfe_coef_plot_lat.png"),
       dyn_lat_plot, width = 7, height = 5, units = "in", dpi = 300)
ggsave(file.path(output_path, "dynamic_twfe_coef_plot_coh.png"),
       dyn_coh_plot, width = 7, height = 5, units = "in", dpi = 300)
ggsave(file.path(output_path, "dynamic_twfe_coef_plot_marriage.png"),
       dyn_marry_plot, width = 7, height = 5, units = "in", dpi = 300)










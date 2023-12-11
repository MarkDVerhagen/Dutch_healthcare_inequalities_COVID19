# Load libraries
library(tidyverse)
library(patchwork)
library(data.table)
library(glmmTMB)
library(texreg)

# source functions
source("./src/functions.R")
source("./plot_functions.R")

data_dir <- file.path("data", "timeseries")

data_inc_pred <- fetch_data_plots(name = "all_all", linear = F, treat_year = 2020,
                                  version = "_v3", dir = data_dir,
                                  var_groups = c("full_interact"))

## Generate covariates
df <- make_covs(data_inc_pred)

## Standard covariates
controls <- paste0(
  c("(1|var_name)", ## Random effects
    "as.factor(age)", "as.factor(female)", "as.factor(poverty)", "as.factor(native)", ## Demographics
    "as.factor(holidays)", "as.factor(week)", "year_fe"), ## Temporal controls
  collapse = "+")

outcome <- "n_person_s"

overall_pandemic <- "pandemic"

## Pandemic and demographic interactions
overall_pandemic_int <- paste0(
  overall_pandemic, "+",
  overall_pandemic, "*as.factor(poverty)", "+",
  overall_pandemic, "*as.factor(native)", "+",
  overall_pandemic, "*as.factor(female)", "+",
  overall_pandemic, "*as.factor(age)"
)

waves_pandemic <- c("wave_1", "wave_2", "wave_3",
                    "interwave_1", "interwave_2")

## Pandemic waves and demographic interactions
waves_pandemic_int <- paste0(
  paste0(waves_pandemic, collapse = "+"), "+",
  paste0(paste0(waves_pandemic, "*as.factor(poverty)"), collapse ="+"), "+",
  paste0(paste0(waves_pandemic, "*as.factor(native)"), collapse ="+"), "+",
  paste0(paste0(waves_pandemic, "*as.factor(female)"), collapse ="+"), "+",
  paste0(paste0(waves_pandemic, "*as.factor(age)"), collapse ="+"))

## Model 1: overall treatment effect since COVID-19 started
model_1 <- as.formula(
  paste0(outcome, "~", controls, "+", overall_pandemic))

model_1_int <- as.formula(
  paste0(outcome, "~", controls, "+", overall_pandemic_int
        ))

## Model 2: wave-by-wave treatment effect since COVID-19 started
model_2 <- as.formula(
  paste0(outcome, "~", controls, "+",
        paste0(waves_pandemic, collapse = "+")))

model_2_int <- as.formula(
  paste0(outcome, "~", controls, "+", waves_pandemic_int))

#' Function to make a regression plot
#'
#' @param data Analysis dataset
#' @param scale Boolean whether to scale the effect by pre-pandemic levels
plot_reg <- function(data, scale = T) {
  ## Interacted
  model1_int <- glmmTMB(model_1_int, data = data)
  model2_int <- glmmTMB(model_2_int, data = data)        
  
  # model1_int_nb <- glmmTMB(model_1_int, data = data, family = nbinom2)
  # model2_int_nb <- glmmTMB(model_2_int, data = data, family = nbinom2)        

  coefficients1 <- as.data.frame(summary(model1_int)$coefficients$cond) %>%
    rownames_to_column(var = "variable") %>%
    filter(grepl(":", variable))
  coefficients2 <- as.data.frame(summary(model2_int)$coefficients$cond) %>%
    rownames_to_column(var = "variable") %>%
    filter(grepl(":", variable))

  total_coefs <- bind_rows(coefficients1, coefficients2)

  coeff_df <- total_coefs %>%
    mutate(Treatment = gsub(".*:", "", variable),  # Extract treatment variable name from 'variable'
          Demographic = gsub(":.*", "", variable),
          Group = gsub("\\).*|.*\\(", "", variable)) %>% # Extract demographic variable name from 'variable'
    rename(SE = `Std. Error`)

  coeff_df$Group <- case_when(
    grepl("18", coeff_df$Demographic) ~ "Age: 18-29",
    grepl("30", coeff_df$Demographic) ~ "Age: 30-65",
    grepl("66", coeff_df$Demographic) ~ "Age: 66-75",
    grepl("76", coeff_df$Demographic) ~ "Age: 76+",
    TRUE ~ coeff_df$Group
  )
  
  ## Standardize by pre-treatment mean
  control_means <- bind_rows(lapply(c("age", "poverty", "native",
                          "female"), function(x) {
    var_name <- "merge"
    temp <- df %>%
      filter(treat == "Control") %>%
      group_by_at(x) %>%
      summarise(control_mean = mean(n_person_s)) %>%
      rename_with(~var_name, all_of(x))
    return(temp)}))
  
  coeff_df$Treatment_num <- case_when(
    coeff_df$Treatment == "wave_1TRUE" ~ 1,
    coeff_df$Treatment == "interwave_1TRUE" ~ 2,
    coeff_df$Treatment == "wave_2TRUE" ~ 3,
    coeff_df$Treatment == "interwave_2TRUE" ~ 4,
    coeff_df$Treatment == "wave_3TRUE" ~ 5,
    coeff_df$Treatment == "pandemicTRUE" ~ 0,
  )
  
  if (scale) {
    coeff_df <- coeff_df %>%
    mutate(merge = gsub(".*\\)", "", Demographic)) %>%
    left_join(control_means, by = "merge")
  
    coeff_df$Estimate <- coeff_df$Estimate / coeff_df$control_mean
    coeff_df$SE <- coeff_df$SE / coeff_df$control_mean  
  }
  
  coeff_df$Group <- case_when(
    coeff_df$Group == "poverty" ~ "Poverty",
    coeff_df$Group == "native" ~ "Migrant Background",
    coeff_df$Group == "female" ~ "Sex",
    TRUE ~ coeff_df$Group
  )
  
  coeff_df$group_label <- gsub(".*\\)", "", coeff_df$Demographic)
  coeff_df$group_label <- case_when(
    coeff_df$group_label == "Poor" ~ "Poverty (No poverty)",
    coeff_df$group_label == "Non-Dutch" ~ "Non-Dutch (Dutch)",
    coeff_df$group_label == "Female" ~ "Female (Male)",
    TRUE ~ coeff_df$group_label
  )
  p_non_age <- ggplot(coeff_df[!grepl("Age", coeff_df$Group), ],
                aes(x = Treatment_num, y = Estimate, ymin = Estimate - SE,
                    ymax = Estimate + SE, color = group_label)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2)

  p_age <- ggplot(coeff_df[grepl("Age", coeff_df$Group), ],
                  aes(x = Treatment_num, y = Estimate, ymin = Estimate - SE,
                      ymax = Estimate + SE, color = merge)) +
    geom_point() +
    geom_errorbar(width = 0.2)

  plots <- lapply(list(p_age, p_non_age), function(p) {
    p + geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0.5) +
      facet_grid(. ~ group_label) +
      labs(x = "Treatment", y = "Additional decline relative to reference\n(% of pre-pandemic levels)") +
      scale_x_continuous(
        breaks = 0:5,
        labels = c("Overall", "Wave 1", "Inter-\nwave 1", "Wave 2", "Inter\n-wave 2", "Wave 3")
      ) + 
      theme(text = element_text(size = 12)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Pandemic wave") +
      theme_reg})
    return(plots)
}

theme_reg <- theme(
  axis.title.x = element_text(
    size = 25,
    margin = margin(r = 5),
    color = "grey10",
    face = "bold"
  ),
  # panel.grid.major = element_blank(),  # Removes major gridlines
  panel.grid.minor = element_blank(),  # Removes minor gridlines
  panel.border = element_rect(colour = "black", fill=NA, size = 1),
  text = element_text(size = 15),
  axis.text.x = element_text(size = 15, margin = margin(t = 5)),
  axis.text.y = element_text(size = 15, margin = margin(r = 5)),
  axis.title.y = element_text(size = 25, margin = margin(r = 5)),
  # Facet labels
  strip.text = element_text(color = "grey10", face = "bold"),
  strip.text.x = element_text(size = 20, margin = margin(b = 5)),
  strip.text.y = element_text(size = 20, margin = margin(l = 5)),
  panel.spacing = unit(2, "lines"),
  # Legend text
  legend.text = element_text(size = 20),
  legend.title = element_blank())

ggsave(plot_reg(df[df$urgency_type == "all", ])[[2]] + theme_reg, filename = paste0(
  "figs/SI_1_multivariate_reg.png"
), width = 15, height = 10)

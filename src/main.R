####################################################################################
# Script: Healthcare Activity Analysis Plotting
# Description: This script generates a range of plots to analyze healthcare activities,
#              including deviation plots, cumulative inequality plots, and robustness
#              checks.
#
# Input Files:
#   1. Functions: 'plot_functions.R', 'src/functions.R'
#   2. COVID Timeseries Data: 
#      - 'data/timeseries/covid_by_week_v3.xlsx'
#   3. Additional Data Files:
#      - Data files generated from 'fetch_data_plots' function
#
# Output Files:
#   The script generates various figures (e.g., deviation plots, cumulative inequality plots)
#   and saves them in the 'figs' directory. The specific filenames include:
#   - 'figs/fig_1.png'
#   - 'figs/fig_2.png'
#   - 'figs/fig_4.png'
#   - Supplementary figures saved in 'figs' with filenames such as 'SI_dev_n_person_s_Onc.png', etc.
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - ggrepel
#   - ggtext
#   - patchwork
#   - showtext
#   - readxl
#   - glmmTMB
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################


library(tidyverse)
library(data.table)
library(ggrepel)
library(ggtext)
library(patchwork)
library(showtext)
library(glmmTMB)

font_add_google("Lato")
showtext_auto()
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

## Timeseries folder
data_dir <- file.path('data', 'timeseries')

# source functions
source("plot_functions.R")
source("src/functions.R")

dvs <- c("n_s", "n_person_s")
treat_year <- 2020
exclude_covid <- T


## specify facet_labels
facet_labels <- c(
    "age_group" = "Age",
    "female" = "Sex",
    "poverty" = "Poverty",
    "background_group" = "Migrant background",
    "total" = "Total",
    "high" = "High urgency",
    "low" = "Low urgency",
    "rest" = "Middle urgency",
    "none" = "No urgency",
    "all" = "All urgency",
    "Diagnostic" = "Diagnostic",
    "Operational" = "Operational",
    "2020" = "Jan-2020 to Dec-2020",
    "2021" = "Jan-2020 to Dec-2021"
)

var_name_labels <- c(
  "18-29" = "18 to 29",
  "30-65" = "30 to 65", 
  "66-75" = "66 to 75",
  "76+" = "76 or older", 
  "cit_dutch" = "Natives", 
  "cit_non_dutch" = "Migrants", 
  "Male" = "Men",
  "Female" = "Women", 
  "Above poverty line" = "Not poor", 
  "Below poverty line" = "Poor"
)

# load covid data
covid_timeseries_n_person <- readxl::read_xlsx(
  file.path(data_dir, "covid_by_week_v3.xlsx"), sheet = "n_persons") %>%
  filter(week > 9 | year > 2020)
covid_timeseries_n <-readxl::read_xlsx(
  file.path(data_dir, "covid_by_week_v3.xlsx"), sheet = "n_activities") %>%
  filter(week > 9 | year > 2020)

## Reshape covid data
covid_long_n_person <- covid_timeseries_n_person %>%
  reshape2::melt(id.var = c("week", "year")) %>%
  filter(variable == "Total")

covid_long_n <- covid_timeseries_n %>%
  reshape2::melt(id.var = c("week", "year")) %>%
  filter(variable == "Total")

## Specify covid data
cdata <- covid_long_n_person

## Specify outcome
y <- "n_person_s"

## Load data
data_inc_pred <- fetch_data_plots("all_all", linear = F, treat_year = treat_year,
                                  version = "_v3",
                                  dir = data_dir, nb = F)

## Define covid wave start and end points
start_w1 <- data_inc_pred$week_date[data_inc_pred$week == 12 &
                        data_inc_pred$year == 2020][1]
end_w1 <- data_inc_pred$week_date[data_inc_pred$week == 22 &
                        data_inc_pred$year == 2020][1]
start_w2 <- data_inc_pred$week_date[data_inc_pred$week == 39 &
                        data_inc_pred$year == 2020][1]
end_w2 <- data_inc_pred$week_date[data_inc_pred$week == 25 &
                        data_inc_pred$year == 2021][1]
start_w3 <- data_inc_pred$week_date[data_inc_pred$week == 42 &
                        data_inc_pred$year == 2021][1]
end_w3 <- data_inc_pred$week_date[data_inc_pred$week == 52 &
                        data_inc_pred$year == 2021][1]

## Figure 1, prediction plot
plt <- gen_pred_plot(data_inc_pred, "n_person_s", total_only  =T) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 800000)) +
  xlab("")

ggsave(plt, filename = paste0(
  "figs/fig_1_pred_n_person_s.png"
), width = 14, height = 9)

## Figure 2, deviation plot
plt <- gen_dev_plot(data_inc_pred, y, cdata)

ggsave(plt, filename = paste0(
  "figs/fig_2_dev_n_person_s.png"
), width = 20, height = 15)

## Figure 3, cumulative inequality plot
plt <- gen_cum_plot(data_inc_pred[urgency_type != "none", ], y)

ggsave(plt, filename = paste0(
  "figs/fig_3_cum_n_person_s.png"
), width = 20, height = 15)


## Figure 4, cumulative inequality, oncology and trauma

theme_cum_sub <- theme(
  axis.text.x = element_text(size = 35, margin = margin(t = 5)),
  axis.text.y = element_text(size = 50, margin = margin(r = 5)),
  legend.text = element_text(size = 50),
  axis.title.y = element_text(
    size = 70,
    margin = margin(r = 5),
    color = "grey10",
    face = "bold"
  ),
  plot.title = element_text(size = 100)
)

data_onc <- fetch_data_plots("all_Onc", linear = F, treat_year = treat_year,
                             version = "_v3")
data_trauma <- fetch_data_plots("all_Trauma", linear = F, treat_year = treat_year,
                             version = "_v3")

plt_1 <- gen_cum_plot(data_onc[(urgency_type %in% c("all", "high", "low")) &
                                  (var_group %in% c("total", "age_group", "female")), ], 
                       y = "n_person_s", overwrite_geom = T, y_min = -0.24, y_max = 0.005) +
  ggtitle("Oncology") + theme_cum_sub

plt_2 <- gen_cum_plot(data_trauma[(urgency_type %in% c("all", "high", "low")) &
                                     (var_group %in% c("total", "age_group", "female")), ], 
                       y = "n_person_s", overwrite_geom = T, y_min = -0.24, y_max = 0.005) +
  ggtitle("Trauma") + theme_cum_sub


plt <- plt_1 + plt_2

ggsave(plt, filename = paste0(
  "figs/fig_4_cum_n_person_s_trauma_onc.png"
), width = 20, height = 15)


# Appendix plots ----------------------------------------------------------

## SI-1 Multivariate regression plot
data_interact <- fetch_data_plots(name = "all_all", linear = F, treat_year = 2020,
                                  version = "_v3", dir = data_dir,
                                  var_groups = c("full_interact"))
## Generate covariates
df <- make_covs(data_interact)

## Standard covariates
reg_controls <- paste0(
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
  paste0(outcome, "~", reg_controls, "+", overall_pandemic))

model_1_int <- as.formula(
  paste0(outcome, "~", reg_controls, "+", overall_pandemic_int
        ))

## Model 2: wave-by-wave treatment effect since COVID-19 started
model_2 <- as.formula(
  paste0(outcome, "~", reg_controls, "+",
        paste0(waves_pandemic, collapse = "+")))

model_2_int <- as.formula(
  paste0(outcome, "~", reg_controls, "+", waves_pandemic_int))

ggsave(plot_reg(df[df$urgency_type == "all", ], model_1_int, model_2_int)[[2]], filename = paste0(
  "figs/SI_1_multivariate_reg.png"
), width = 15, height = 10)

## Load alternative datasets for SI figures
data_placebo <- fetch_data_plots("all_all", linear = F, treat_year = 2019,
                                 version = "_v3") %>%
  filter(year <= 2019)
data_RelDiag_all <- fetch_data_plots("RelDiag_all", linear = F, treat_year = treat_year,
                                     version = "_v3")
data_Intense_all <- fetch_data_plots("Intense_all", linear = F, treat_year = treat_year,
                                     version = "_v3")

# DEVIATION PLOTS

## SI-2a Dev plot, Oncology
plt <- gen_dev_plot(data_onc, y, cdata, include_covid = F) +
ggtitle("Oncology") + theme(plot.title = element_text(size = 100))

ggsave(plt, filename = paste0(
  "figs/SI_2a_dev_n_person_s_Onc.png"
), width = 20, height = 15)

## SI-2b Dev plot, Trauma
plt <- gen_dev_plot(data_trauma, y, cdata, include_covid = F) +
ggtitle("Trauma") + theme(plot.title = element_text(size = 100))

ggsave(plt, filename = paste0(
  "figs/SI_2b_dev_n_person_s_Trauma.png"
), width = 20, height = 15)

## SI-3a: Cum plot, Oncology
plt <- gen_cum_plot(data_onc[data_onc$urgency %in% c("all", "high", "low"), ],
                    y = "n_person_s", y_min = -0.07, y_max = 0.005) +
ggtitle("Oncology") + theme(plot.title = element_text(size = 100))

ggsave(plt, filename = paste0(
  "figs/SI_3a_cum_n_person_s_Onc.png"
), width = 20, height = 15)

## SI-3b: Cum plot, Trauma
plt <- gen_cum_plot(data_trauma[data_trauma$urgency %in% c("all", "high", "low"), ],
                    y = "n_person_s", y_min = -0.07, y_max = 0.005) +
ggtitle("Trauma") + theme(plot.title = element_text(size = 100))

ggsave(plt, filename = paste0(
  "figs/SI_3b_cum_n_person_s_Trauma.png"
), width = 20, height = 15)

## SI 4-a Deviation plot, RelDiag
plt <- gen_dev_plot(data_RelDiag_all,
                    y = "n_person_s",
                    cdata = covid_long_n_person)

ggsave(plt, filename = paste0(
  "figs/SI_4a_dev_RelDiag_n_person_s.png"
), width = 20, height = 15)

## SI-4b Cumulative inequality plot, RelDiag

plt <- gen_cum_plot(data_RelDiag_all[urgency_type != "none", ],
                    y = "n_person_s")

ggsave(plt, filename = paste0(
  "figs/SI_4b_cum_RelDiag_n_person_s.png"
), width = 20, height = 15)

# SI 5-a Deviation plot, Intense
plt <- gen_dev_plot(data_Intense_all,
                    y = "n_person_s",
                    cdata = covid_long_n_person)

ggsave(plt, filename = paste0(
  "figs/SI_5a_dev_Intense_n_person_s.png"
), width = 20, height = 15)

## SI-5b Cumulative inequality plot, Intense

plt <- gen_cum_plot(data_Intense_all[urgency_type != "none", ],
                    y = "n_person_s")

ggsave(plt, filename = paste0(
  "figs/SI_5b_cum_Intense_n_person_s.png"
), width = 20, height = 15)

## SI-6a Deviation plot, activities
plt <- gen_dev_plot(data_inc_pred,
                    y = "n_s",
                    cdata = covid_long_n)

ggsave(plt, filename = paste0(
  "figs/SI_6a_dev_n_s_2020.png"
), width = 20, height = 15)

## SI-6b Cumulative inequality plot, Activities
plt <- gen_cum_plot(data_inc_pred[urgency_type != "none", ],
                    y = "n_s")

ggsave(plt, filename = paste0(
  "figs/SI_6b_cum_n_s.png"
), width = 20, height = 15)


## SI-7a Deviation plot, placebo year
plt <- gen_dev_plot(data_placebo,
                        y = "n_person_s",
                        cdata = covid_long_n,
                        treat_year = 2019,
                        include_covid = FALSE
) + scale_y_continuous(limits = c(-300000, 100000), labels = scales::comma) +
  xlim(c(as.Date("2019-01-01"), as.Date("2019-12-31")))

ggsave(plt, filename = paste0(
  "figs/SI_7a_dev_n_person_s_2019.png"
), width = 20, height = 15)


## SI-7b Cumulative inequality plot, placebo year
plt <- gen_cum_plot(data_placebo[urgency_type != "none" & between(year, 2017, 2019), ],
                    y = "n_person_s",
                    treat_year = 2019, overwrite_geom = T, y_min = -0.12, y_max = 0.005)

ggsave(plt, filename = paste0(
  "figs/SI_7b_cum_n_person_s_2019.png"
), width = 20, height = 15)

data_inc_pred_nb <- fetch_data_plots("all_all", linear = F, treat_year = treat_year,
                                     version = "_v3", nb = T)

## SI-8a: Negative binomial robustness, Deviation plot
plt <- gen_dev_plot(data_inc_pred_nb, y, cdata)

ggsave(plt, filename = paste0(
  "figs/SI_8a_dev_plot_nb.png"
), width = 20, height = 15)

## SI-8b: Negative binomial robustness, Cumulative inequality plot
plt <- gen_cum_plot(data_inc_pred_nb[urgency_type != "none", ], y)

ggsave(plt, filename = paste0(
  "figs/SI_8b_cum_plot_nb.png"
), width = 20, height = 15)

# Script to generate plots

# Load libraries
library(tidyverse)
library(data.table)
library(ggrepel)
library(ggtext)
library(patchwork)
library(showtext)

font_add_google("Lato")
showtext_auto()
Sys.setlocale("LC_ALL", "en_GB.UTF-8")

# source functions
source("plot_fn.R")
source("src/functions.R")

# specify var groups
var_groups <- c(
    "age_group",
    "female",
    "poverty",
    "background_group",
    "Total"
)

# specify facet_labels
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
    "Operational" = "Operational"
)

# specify name of labels
var_name_labels <- c(
    "18-29" = "18 to 29",
    "30-65" = "30 to 65",
    "66-75" = "66 to 75",
    "76+" = "76 or older",
    "other" = "Other income",
    "cit_dutch" = "Natives",
    "cit_non_dutch" = "Migrants",
    "Male" = "Male",
    "Female" = "Female",
    "Above poverty line" = "Not poor",
    "Below poverty line" = "Poor",
    "Total" = "Total"
)

# specify controls in regression
controls <- c(
    "as.factor(week)",
    "holidays",
    "ndays",
    "year_lin"
)

dvs <- c("n_s", "n_person_s")

treat_year <- 2020

exclude_covid <- T

# load covid data
covid_timeseries_n_person <- readxl::read_xlsx("./data/covid_by_week.xlsx",
                                               sheet = "n_persons")
covid_timeseries_n <- readxl::read_xlsx("./data/covid_by_week.xlsx",
                                        sheet = "n_activities")

# reshape covid data
covid_long_n_person <- covid_timeseries_n_person %>%
  reshape2::melt(id.var = c("week")) %>%
  filter(variable == "Total")

covid_long_n <- covid_timeseries_n %>%
  reshape2::melt(id.var = c("week")) %>%
  filter(variable == "Total")

# specify covid data
cdata <- covid_long_n_person
y <- "n_person_s"

data_inc_pred <- fetch_data_plots("all_all", linear = F, treat_year = treat_year, controls = controls)


## Figure 1, deviation plot
plt <- gen_dev_plot(data_inc_pred, y, cdata)

ggsave(plt, filename = paste0(
  "figs/manuscript/fig_1.png"
), width = 20, height = 15)


## Figure 2, cumulative inequality plot
plt <- gen_cum_plot(data_inc_pred[urgency_type != "none", ], y)

ggsave(plt, filename = paste0(
  "figs/manuscript/fig_2.png"
), width = 20, height = 15)


## Figure 3, relative inequality plot
plt <- gen_rel_plot(data_inc_pred[!(urgency_type %in% c("rest", "none")), ], y)

ggsave(plt, filename = paste0(
  "figs/manuscript/fig_3.png"
), width = 20, height = 15)


## Figure 4, cumulative inequality, oncology and trauma

data_onc <- fetch_data_plots("all_Onc", linear = F, treat_year = treat_year, controls = controls)
data_trauma <- fetch_data_plots("all_Trauma", linear = F, treat_year = treat_year, controls = controls)

plt_1 <- gen_cum_plot(data_onc[(urgency_type %in% c("all", "high", "low")) &
                                  (var_group %in% c("total", "age_group", "female")), ], 
                       y = "n_person_s", overwrite_geom = T, y_min = -0.24, y_max = 0.005) +
  ggtitle("Oncology")

plt_2 <- gen_cum_plot(data_trauma[(urgency_type %in% c("all", "high", "low")) &
                                     (var_group %in% c("total", "age_group", "female")), ], 
                       y = "n_person_s", overwrite_geom = T, y_min = -0.24, y_max = 0.005) +
  ggtitle("Trauma")


plt <- plt_1 + plt_2

ggsave(plt, filename = paste0(
  "figs/manuscript/fig_4.png"
), width = 20, height = 15)


# Appendix plots ----------------------------------------------------------

data_placebo <- fetch_data_plots("all_all", linear = F, treat_year = 2019, controls = controls)
data_RelDiag_all <- fetch_data_plots("RelDiag_all", linear = F, treat_year = treat_year, controls = controls)
data_Intense_all <- fetch_data_plots("Intense_all", linear = F, treat_year = treat_year, controls = controls)

# DEVIATION PLOTS

## SI: Deviation plot, placebo year
plt <- gen_dev_plot(data_placebo,
                        y = "n_person_s",
                        cdata = covid_long_n,
                        treat_year = 2019,
                        include_covid = FALSE
) + scale_y_continuous(limits = c(-300000, 100000), labels = scales::comma)

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_dev_n_person_s_2019.png"
), width = 20, height = 15)

## SI: Deviation plot, activities
plt <- gen_dev_plot(data_inc_pred,
                    y = "n_s",
                    cdata = covid_long_n)

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_dev_n_s_2020.png"
), width = 20, height = 15)

## SI: Deviation plot, RelDiag
plt <- gen_dev_plot(data_RelDiag_all,
                    y = "n_person_s",
                    cdata = covid_long_n_person)

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_dev_RelDiag_n_person_s_2020.png"
), width = 20, height = 15)

# SI: Deviation plot, Intense
plt <- gen_dev_plot(data_Intense_all,
                    y = "n_person_s",
                    cdata = covid_long_n_person)

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_dev_Intense_n_person_s_2020.png"
), width = 20, height = 15)


# CUMULATIVE PLOTS

## SI: Cumulative inequality plot, placebo year
plt <- gen_cum_plot(data_placebo[urgency_type != "none", ],
                    y = "n_person_s",
                    treat_year = 2019, overwrite_geom = T, y_min = -0.12, y_max = 0.005)

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_cum_n_person_s_2019.png"
), width = 20, height = 15)


## SI: Cumulative inequality plot, Activities
plt <- gen_cum_plot(data_inc_pred[urgency_type != "none", ],
                    y = "n_s")

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_cum_n_s.png"
), width = 20, height = 15)


## SI: Cumulative inequality plot, RelDiag

plt <- gen_cum_plot(data_RelDiag_all[urgency_type != "none", ],
                    y = "n_person_s")

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_cum_RelDiag_n_person_s.png"
), width = 20, height = 15)


## SI: Cumulative inequality plot, Intense

plt <- gen_cum_plot(data_Intense_all[urgency_type != "none", ],
                    y = "n_person_s")

ggsave(plt, filename = paste0(
  "figs/manuscript/SI_cum_Intense_n_person_s.png"
), width = 20, height = 15)


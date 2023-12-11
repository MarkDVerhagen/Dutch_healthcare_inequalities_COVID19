####################################################################################
# Script: Data Processing for Healthcare Activity Analysis
# Description: This script is designed for generating descriptive tables based on
#              counts of healthcare activities.
#
# Input Files:
#   1. Functions: './src/functions.R', './plot_functions.R'
#   2. Data Files: 
#      - 'data/Rel_all.xlsx', 'data/Rel_Trauma.xlsx', 'data/Rel_Onc.xlsx', ...
#      - './data/final_v3/sets_v3.rda', './data/final_v3/low_sets_v3.rda', ...
#      - './data/final_v3/high_sets_v3.rda', './data/final_v3/rest_sets_v3.rda', ...
#      - './data/final_v3/none_sets_v3.rda'
#
# Output Files:
#   The script generates a variety of descriptive tables and writes them to CSV files,
#   with the specific paths and names based on the treatment year, data type, and other
#   parameters. Some general counts are also calculated within the script but not saved.
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - readxl
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################


library(tidyverse)
library(data.table)

# set treatment year
treat_year <- 2020

## Load data
source("./src/functions.R")
source("./plot_functions.R")

## Specify table parameters
var_groups <- c('total', 'female', 'age_group', 'background_group', 'poverty')
dvs <- c("n_s", "n_person_s")

## Specify files
all_files <- c("Rel_all", "Rel_Trauma", "Rel_Onc",
               "RelDiag_all", "RelDiag_Trauma", "RelDiag_Onc",
               "Intense_all", "Intense_Trauma", "Intense_Onc",
               "Diag_all", "Diag_Trauma", "Diag_Onc",
               "all_all", "all_Trauma", "all_Onc")

for(dataname in all_files) {
  data <- readxl::read_xlsx(paste0("data/", dataname, ".xlsx"))
  
  ## remove all but relevant var_groups (overall)
  data <- data %>% 
    filter(var_group %in% c(var_groups, "total"))
  
  for (y in dvs) {
    
    if (y %in% c("n", "n_s")) {
      y_name <- "activities"
    } else {
      y_name <- "individuals"
    }
    
    for (t in unique(data$urgency_type)) {
      tab <- count_table(data, y, subtype = t, var_groups, percent = percent)
      write_csv(tab, 
                file = paste0("tables", "/", 
                              "main", "/",
                              y_name, "/", 
                              treat_year, "/", 
                              dataname, "/",
                              "table_",
                              t, "_",
                              if (percent) {
                                paste0("percent")
                              } else {
                                paste0("count")
                              },
                              ".csv"))
    }
  }
}

## Counts in manuscript
data_inc_pred <- fetch_data_plots("all_all", linear = F, treat_year = treat_year,
                                  version = "_v3")

calc_total_loss <- function(
  data, urgency = "all", var = "Total") {
  data <- data[data$week != 53, ]
  tot_2020 <- sum(
  data[data$urgency_type == urgency &
       data$var_name == var &
       data$year == "2020", "n_person_s"])
  pred_2020 <- sum(
    data[data$urgency_type == urgency &
         data$var_name == var &
         data$year == "2020", "n_person_s_pred"])


  tot_2021 <- sum(
    data[data$urgency_type == urgency &
         data$var_name == var &
         data$year == "2021", "n_person_s"])
  pred_2021 <- sum(
    data[data$urgency_type == urgency &
         data$var_name == var &
         data$year == "2021", "n_person_s_pred"])
  print(paste("Absolute 2020:", pred_2020 - tot_2020))
  print(paste("Absolute 2021:", pred_2021 - tot_2021))
  print(paste("Overall relative:", round(1 - (tot_2020 + tot_2021) / (pred_2020 + pred_2021), 3)))
  }

## Total predicted versus realized by year
tot_2020 <- sum(
  data_inc_pred[urgency_type == "all" &
                var_name == "Total" &
                year == "2020", "n_person"])
pred_2020 <- sum(
  data_inc_pred[urgency_type == "all" &
                var_name == "Total" &
                year == "2020", "n_person_pred"])

tot_2021 <- sum(
  data_inc_pred[urgency_type == "all" &
                var_name == "Total" &
                year == "2021", "n_person"])
pred_2021 <- sum(
  data_inc_pred[urgency_type == "all" &
                var_name == "Total" &
                year == "2021", "n_person_pred"])

tot_2020 - pred_2020
tot_2021 - pred_2021

(tot_2020 + tot_2021) / (pred_2020 + pred_2021)

## Cumulative decline by urgency
tot_2020_low <- sum(
  data_inc_pred[urgency_type == "low" &
                var_name == "Total" &
                year == "2020", "n_person"])
pred_2020_low <- sum(
  data_inc_pred[urgency_type == "low" &
                var_name == "Total" &
                year == "2020", "n_person_pred"])

tot_2021_low <- sum(
  data_inc_pred[urgency_type == "low" &
                var_name == "Total" &
                year == "2021", "n_person"])
pred_2021_low <- sum(
  data_inc_pred[urgency_type == "low" &
                var_name == "Total" &
                year == "2021", "n_person_pred"])

(tot_2020_low + tot_2021_low) / (pred_2020_low + pred_2021_low)

tot_2020_high <- sum(
  data_inc_pred[urgency_type == "high" &
                var_name == "Total" &
                year == "2020", "n_person"])
pred_2020_high <- sum(
  data_inc_pred[urgency_type == "high" &
                var_name == "Total" &
                year == "2020", "n_person_pred"])


tot_2021_high <- sum(
  data_inc_pred[urgency_type == "high" &
                var_name == "Total" &
                year == "2021", "n_person"])
pred_2021_high <- sum(
  data_inc_pred[urgency_type == "high" &
                var_name == "Total" &
                year == "2021", "n_person_pred"])

(tot_2020_high + tot_2021_high) / (pred_2020_high + pred_2021_high)


## Declines by urgency and demographic
calc_total_loss(data_inc_pred,
                urgency = "low", var = "Below poverty line")
calc_total_loss(data_inc_pred,
                urgency = "rest", var = "Below poverty line")
calc_total_loss(data_inc_pred,
                urgency = "low", var = "Above poverty line")
calc_total_loss(data_inc_pred,
                urgency = "rest", var = "Above poverty line")

calc_total_loss(data_inc_pred,
                urgency = "low", var = "cit_non_dutch")
calc_total_loss(data_inc_pred,
                urgency = "rest", var = "cit_non_dutch")
calc_total_loss(data_inc_pred,
                urgency = "low", var = "Above poverty line")
calc_total_loss(data_inc_pred,
                urgency = "rest", var = "Above poverty line")

calc_total_loss(data_inc_pred,
                urgency = "high", var = "76+")
calc_total_loss(data_inc_pred,
                urgency = "high", var = "18-29")

calc_total_loss(data_inc_pred,
                urgency = "low", var = "Above poverty line")
calc_total_loss(data_inc_pred,
                urgency = "rest", var = "Above poverty line")
## Produce low level weekly series to be fetched pre-plotting

## Load libraries
library(tidyverse)
library(data.table)
source("./src/functions.R")

## Load data
load("data/edit/full_analysis_sets.rda")

## Load data but exclude full procedures
rm(procedures)
gc()

var_groups <- c("background_group", "poverty", "age_group",
                "female", "income_group", "all")
activity_names <- c("RelDiag", "Rel", "Diag", "Intense")

## low urgency
load("./data/final/rest_sets.rda")

lapply(var_groups, function(x) {
  apply_count(df_low, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "low", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_2, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Onc", urgency_sel = "all", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_15, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Trauma", urgency_sel = "all", sub_folder = "full")
})

lapply(1:4, function(x) {
  data <- df_low_act[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "all", urgency_sel = "low", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_low_act_2[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Onc", urgency_sel = "low", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_low_act_15[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Trauma", urgency_sel = "low", sub_folder = "full")
  })
})

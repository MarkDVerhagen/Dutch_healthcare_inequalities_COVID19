####################################################################################
# Script: Generate weekly counts per medical and demographic subset
# Description: This script generates counts by demographic and medical subsets
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Full Demographic Data (v3): 'data/edit/dem_full_v3.rda'
#   3. Activity Data Sets for All Urgency Levels: 
#       - './data/final_v3/sets_v3.rda'
#       - './data/final_v3/low_sets_v3.rda'
#       - './data/final_v3/high_sets_v3.rda'
#       - './data/final_v3/rest_sets_v3.rda'
#       - './data/final_v3/none_sets_v3.rda'
#
# Output Files:
#   The script generates several datasets based on the combinations of urgency levels,
#   medical types, and demographic groups. These datasets are used for downstream
#   analysis and are not explicitly saved within this script.
#
# Libraries Used:
#   - tidyverse
#   - data.table
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################


## Load libraries
library(tidyverse)
library(data.table)
source('./src/functions.R')

## Load demographic data
load("data/edit/dem_full_v3.rda")

dem_full <- add_interacted_var_group(dem_full)

## Define groups
var_groups <- c("background_group", "poverty", "age_group",
                "female", "income_group", "all",
                "full_interact", "gih_interact", "ih_interact")

## Define activity sets
activity_names <- c("RelDiag", "Rel", "Diag", "Intense")

## Load activity data
load("./data/final_v3/sets_v3.rda")

df <- add_interacted_var_group(df)
df_2 <- add_interacted_var_group(df_2)
df_15 <- add_interacted_var_group(df_15)
df_act <- lapply(df_act, add_interacted_var_group)
df_act_2 <- lapply(df_act_2, add_interacted_var_group)
df_act_15 <- lapply(df_act_15, add_interacted_var_group)

## All urgency
lapply(var_groups, function(x) {
  apply_count(df, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "all", sub_folder = "full")
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
  data <- df_act[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "all", urgency_sel = "all", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_act_2[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Onc", urgency_sel = "all", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_act_15[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Trauma", urgency_sel = "all", sub_folder = "full")
  })
})

rm(df, df_2, df_15, df_act, df_act_2, df_act_15)

## Low urgency

load("./data/final_v3/low_sets_v3.rda")

df_low <- add_interacted_var_group(df_low)
df_low_2 <- df_low[df_low$medical_type == 2, ]
df_low_15 <- df_low[df_low$medical_type == 15, ]
df_low_2 <- add_interacted_var_group(df_low_2)
df_low_15 <- add_interacted_var_group(df_low_15)
df_low_act <- lapply(df_low_act, add_interacted_var_group)
df_low_act_2 <- lapply(df_low_act_2, add_interacted_var_group)
df_low_act_15 <- lapply(df_low_act_15, add_interacted_var_group)

lapply(var_groups, function(x) {
  apply_count(df_low, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "low", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_low[medical_type == 2, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Onc", urgency_sel = "low", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_low[medical_type == 15, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Trauma", urgency_sel = "low", sub_folder = "full")
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

rm(df_low, df_low_act, df_low_act_2, df_low_act_15)
gc()

## High urgency

load("./data/final_v3/high_sets_v3.rda")

df_high <- add_interacted_var_group(df_high)
df_high_2 <- df_high[df_high$medical_type == 2, ]
df_high_15 <- df_high[df_high$medical_type == 15, ]
df_high_2 <- add_interacted_var_group(df_high_2)
df_high_15 <- add_interacted_var_group(df_high_15)
df_high_act <- lapply(df_high_act, add_interacted_var_group)
df_high_act_2 <- lapply(df_high_act_2, add_interacted_var_group)
df_high_act_15 <- lapply(df_high_act_15, add_interacted_var_group)

lapply(var_groups, function(x) {
  apply_count(df_high, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "high", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_high[medical_type == 2, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Onc", urgency_sel = "high", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_high[medical_type == 15, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Trauma", urgency_sel = "high", sub_folder = "full")
})

lapply(1:4, function(x) {
  data <- df_high_act[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "all", urgency_sel = "high", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_high_act_2[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Onc", urgency_sel = "high", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_high_act_15[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Trauma", urgency_sel = "high", sub_folder = "full")
  })
})

rm(df_high, df_high_act, df_high_act_2, df_high_act_15)
gc()

## Rest urgency

load("./data/final_v3/rest_sets_v3.rda")

df_rest <- add_interacted_var_group(df_rest)
df_rest_2 <- df_rest[df_rest$medical_type == 2, ]
df_rest_15 <- df_rest[df_rest$medical_type == 15, ]
df_rest_2 <- add_interacted_var_group(df_rest_2)
df_rest_15 <- add_interacted_var_group(df_rest_15)
df_rest_act <- lapply(df_rest_act, add_interacted_var_group)
df_rest_act_2 <- lapply(df_rest_act_2, add_interacted_var_group)
df_rest_act_15 <- lapply(df_rest_act_15, add_interacted_var_group)

lapply(var_groups, function(x) {
  apply_count(df_rest, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "rest", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_rest[medical_type == 2, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Onc", urgency_sel = "rest", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_rest[medical_type == 15, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Trauma", urgency_sel = "rest", sub_folder = "full")
})

lapply(1:4, function(x) {
  data <- df_rest_act[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "all", urgency_sel = "rest", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_rest_act_2[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Onc", urgency_sel = "rest", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_rest_act_15[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Trauma", urgency_sel = "rest", sub_folder = "full")
  })
})

rm(df_rest, df_rest_act, df_rest_act_2, df_rest_act_15)
gc()

## No urgency

load("./data/final_v3/none_sets_v3.rda")

df_none <- add_interacted_var_group(df_none)
df_none_2 <- df_none[df_none$medical_type == 2, ]
df_none_15 <- df_none[df_none$medical_type == 15, ]
df_none_2 <- add_interacted_var_group(df_none_2)
df_none_15 <- add_interacted_var_group(df_none_15)
df_none_act <- lapply(df_none_act, add_interacted_var_group)
df_none_act_2 <- lapply(df_none_act_2, add_interacted_var_group)
df_none_act_15 <- lapply(df_none_act_15, add_interacted_var_group)

lapply(var_groups, function(x) {
  apply_count(df_none, dem_full, var_group = x, activity_sel = "all",
              medical_sel = "all", urgency_sel = "none", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_none[medical_type == 2, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Onc", urgency_sel = "none", sub_folder = "full")
})

lapply(var_groups, function(x) {
  apply_count(df_none[medical_type == 15, ], dem_full, var_group = x, activity_sel = "all",
              medical_sel = "Trauma", urgency_sel = "none", sub_folder = "full")
})

lapply(1:4, function(x) {
  data <- df_none_act[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "all", urgency_sel = "none", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_none_act_2[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Onc", urgency_sel = "none", sub_folder = "full")
  })
})

lapply(1:4, function(x) {
  data <- df_none_act_15[[x]]
  lapply(var_groups, function(y) {
    apply_count(data, dem_full, var_group = y, activity_sel = activity_names[[x]],
                medical_sel = "Trauma", urgency_sel = "none", sub_folder = "full")
  })
})

rm(df_none, df_none_act, df_none_act_2, df_none_act_15)
gc()
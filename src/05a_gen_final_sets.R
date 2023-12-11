####################################################################################
# Script: Make relevant sets of activities for counting
# Description: This scripts subsets the total universe of activities such that efficient
#              counting can be done
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Procedures Data (v3): 'data/edit/procedures_v3.rda'
#
# Output Files:
#   1. High Urgency Activities: './data/final_v3/df_high_v3.csv.gz'
#   2. Low Urgency Activities: './data/final_v3/df_low_v3.csv.gz'
#   3. Medium Urgency (Rest) Activities: './data/final_v3/df_rest_v3.csv.gz'
#   4. No Urgency Activities: './data/final_v3/df_none_v3.csv.gz'
#   5. Full Activities Data: './data/final_v3/df_v3.csv.gz'
#   6. Subset Data for Various Urgency Levels: './data
#       - './data/final_v3/high_sets_v3.rda'
#       - './data/final_v3/low_sets_v3.rda'
#       - './data/final_v3/rest_sets_v3.rda'
#       - './data/final_v3/none_sets_v3.rda'
#       - './data/final_v3/sets_v3.rda'
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

load("data/edit/procedures_v3.rda")

## All activities by urgency
df_high <- procedures[urgency_num %in% c(1, 2), ] 
fwrite(df_high, "./data/final_v3/df_high_v3.csv.gz")

df_low <- procedures[urgency_num %in% c(6, 7), ] 
fwrite(df_low, "./data/final_v3/df_low_v3.csv.gz")

df_rest <- procedures[urgency_num %in% c(3, 4, 5), ] 
fwrite(df_rest, "./data/final_v3/df_rest_v3.csv.gz")

df_none <- procedures[urgency_num %in% c(0), ]
fwrite(df_none, "./data/final_v3/df_none_v3.csv.gz")

fwrite(procedures, "./data/final_v3/df_v3.csv.gz")

## Define activity classifications
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4), ## All excluding lab
                       c(1, 79, 5, 3, 19, 2), ## In-house and diagnostic
                       c(19, 3, 5),  ## Diagnostic
                       c(7, 4)) ## Intensive

## High Urgency sets

df_high <- fread("./data/final_v3/df_high_v3.csv.gz")

df_high_2 <- df_high[medical_type == 2,]
df_high_15 <- df_high[medical_type == 15,]

df_high_act <- lapply(activity_types, function(x) {
  df_high[activity_type %in% x, ]
})
df_high_act_2 <- lapply(df_high_act, function(x) {
  x[medical_type == 2,]
})
df_high_act_15 <- lapply(df_high_act, function(x) {
  x[medical_type == 15,]
})
save(df_high, df_high_act, df_high_act_2, df_high_act_15,
     df_high_2, df_high_15,
     file = "./data/final_v3/high_sets_v3.rda")

## Low urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_low <- fread("./data/final_v3/df_low_v3.csv.gz")

df_low_2 <- df_low[medical_type == 2,]
df_low_15 <- df_low[medical_type == 15,]

df_low_act <- lapply(activity_types, function(x) {
  df_low[activity_type %in% x, ]
})
df_low_act_2 <- lapply(df_low_act, function(x) {
  x[medical_type == 2,]
})
df_low_act_15 <- lapply(df_low_act, function(x) {
  x[medical_type == 15,]
})
save(df_low, df_low_act, df_low_act_2, df_low_act_15,
     file = "./data/final_v3/low_sets_v3.rda")

## Rest urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_rest <- fread("./data/final_v3/df_rest_v3.csv.gz")

df_rest_2 <- df_rest[medical_type == 2,]
df_rest_15 <- df_rest[medical_type == 15,]

df_rest_act <- lapply(activity_types, function(x) {
  df_rest[activity_type %in% x, ]
})
df_rest_act_2 <- lapply(df_rest_act, function(x) {
  x[medical_type == 2,]
})
df_rest_act_15 <- lapply(df_rest_act, function(x) {
  x[medical_type == 15,]
})
save(df_rest, df_rest_act, df_rest_act_2, df_rest_act_15,
     file = "./data/final_v3/rest_sets_v3.rda")

## None urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_none <- fread("./data/final_v3/df_none_v3.csv.gz")

df_none_2 <- df_none[medical_type == 2, ]
df_none_15 <- df_none[medical_type == 15, ]

df_none_act <- lapply(activity_types, function(x) {
  df_none[activity_type %in% x, ]
})
df_none_act_2 <- lapply(df_none_act, function(x) {
  x[medical_type == 2,]
})
df_none_act_15 <- lapply(df_none_act, function(x) {
  x[medical_type == 15,]
})
save(df_none, df_none_act, df_none_act_2, df_none_act_15,
     file = "./data/final_v3/none_sets_v3.rda")

## All urgency

df <- fread("./data/final_v3/df_v3.csv.gz")

df_2 <- df[medical_type == 2, ]
df_15 <- df[medical_type == 15, ]

df_act <- list()
df_act[[1]] <- df[activity_type %in% activity_types[[1]],]
df_act[[2]] <- df[activity_type %in% activity_types[[2]],]
df_act[[3]] <- df[activity_type %in% activity_types[[3]],]
df_act[[4]] <- df[activity_type %in% activity_types[[4]],]
df_act <- lapply(activity_types, function(x) {
  df[activity_type %in% x, ]
})

df_act_2 <- lapply(df_act, function(x) {
  x[medical_type == 2,]
})
df_act_15 <- lapply(df_act, function(x) {
  x[medical_type == 15,]
})

save(df,df_2, df_15, df_act, df_act_2, df_act_15, file = "./data/final_v3/sets_v3.rda")
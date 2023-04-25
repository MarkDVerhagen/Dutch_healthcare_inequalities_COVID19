## Produce low level weekly series to be fetched pre-plotting

## Load libraries
library(tidyverse)
library(data.table)
source("./src/functions.R")

## Load data
load("data/edit/full_analysis_sets.rda")
gc()

## Urgency (5)
df_high <- procedures[urgency_num %in% c(1, 2), ] 
saveRDS(df_high, "./data/final/df_high.rds")

df_low <- procedures[urgency_num %in% c(6, 7), ] 
saveRDS(df_low, "./data/final/df_low.rds")

df_rest <- procedures[urgency_num %in% c(3, 4, 5), ] 
saveRDS(df_rest, "./data/final/df_rest.rds")

df_none <- procedures[urgency_num %in% c(0), ]
saveRDS(df_none, "./data/final/df_none.rds")

saveRDS(procedures, "./data/final/df.rds")

rm(list = ls())
gc()


## High Urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_high <- read_rds("./data/final/df_high.rds")

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
     file = "./data/final/high_sets.rda")

rm(list = ls())
gc()

## Low urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_low <- read_rds("./data/final/df_low.rds")

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
     file = "./data/final/low_sets.rda")

rm(list = ls())
gc()

## Rest urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_rest <- read_rds("./data/final/df_rest.rds")

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
     file = "./data/final/rest_sets.rda")

rm(list = ls())
gc()

## None urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df_none <- read_rds("./data/final/df_none.rds")

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
     file = "./data/final/none_sets.rda")

rm(list = ls())
gc()

## All urgency
activity_types <- list(c(1, 79, 5, 3, 19, 2, 7, 4),
                       c(1, 79, 5, 3, 19, 2),
                       c(19, 3, 5), c(7, 4))
df <- read_rds("./data/final/df.rds")

df_2 <- df[medical_type == 2, ]
df_15 <- df[medical_type == 15, ]

df_act <- lapply(activity_types, function(x) {
  df[activity_type %in% x, ]
})
df_act_2 <- lapply(df_act, function(x) {
  x[medical_type == 2,]
})
df_act_15 <- lapply(df_act, function(x) {
  x[medical_type == 15,]
})

save(df, df_act, df_act_2, df_act_15, file = "./data/final/sets.rda")

rm(list = ls())
gc()

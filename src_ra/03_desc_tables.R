####################################################################################
# Script: Descriptive Tables Generation from Demographic Data
# Description: This script generates create descriptive tables for the demographic
#              data. It involves adding interacted variable groups, making year-wise
#              counts, and grouping data based on various demographic characteristics.
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Full Demographic Data (v3): 'data/edit/dem_full_v3.rda'
#
# Output Files:
#   1. Numeric Descriptive Table: 'data/output/dem_table_num_v3.xlsx'
#   2. Percentage Descriptive Table: 'data/output/dem_table_per_v3.xlsx'
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - xlsx
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

## Make fully interacted demographic groups
dem_full <- dem_full %>% add_interacted_var_group()

## Make year count
tab <- dem_full[. .(n = .N), by = year]
tab[, var_group := "Total"]
tab[, var_name = "Total"]


## Grouping variables
groups <- c("female", "age_group", "background_group", "income_group",
            "poverty", "gih_interact", "ih_interact", "full_interact")

for (i in groups) {
  x <- dem_full %>% 
    group_by(year, get(i)) %>% 
    count() %>% 
    mutate(var_group = i) %>%
    rename(var_name = `get(i)`) %>% 
    mutate(var_name = as.character(var_name))
  
  tab <- rbind(tab, x)
}

## Numeric table  
tab <- tab %>% 
  pivot_wider(names_from = year, values_from = n)

tab <- tab[!is.na(tab$`2017`), ]

## Save numeric table
xlsx::write.xlsx(tab, "data/output/dem_table_num_v3.xlsx")

## Percent_table 
total <- tab %>% filter(var_group == "Total")

for (i in names(tab)) {
  if (is.numeric(tab[[i]])) {
    tab[[i]] <- round(tab[[i]]/total[[i]], 2)
    tab[[i]][1] <- as.integer(total[[i]])
  }
}

## Save percent table
xlsx::write.xlsx(tab, "data/output/dem_table_per_v3.xlsx")


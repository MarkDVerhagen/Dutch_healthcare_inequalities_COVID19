####################################################################################
# Script: COVID Weekly Series Generation
# Description: This script produces a low-level weekly series of COVID-related data
#              for analysis and pre-plotting.
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Procedures Data (v3): 'data/edit/procedures_v3.rda'
#
# Output Files:
#   1. COVID Weekly Series Data: './data/final/covid_by_week_v3.xlsx'
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - assertthat
#   - DescTools
#   - openxlsx
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################


## Load libraries
library(tidyverse)
library(data.table)
source('./src/functions.R')

load("data/edit/procedures_v3.rda")

## Only include covid data
procedures_covid <- procedures[procedures$covid_activity_dbc, ]

## Unit test that all covid activities are in 2020 or 2021
assertthat::assert_that(all(procedures_covid$year >= 2020))

procedures_covid <- procedures_covid %>% as.data.table()

## Generate variables
tab <- procedures_covid[
  , .(n = .N, n_person = length(na.omit(unique(rinpersoon)))),
  by = c("week", "year")]
tab[, var_name := "Total"]
tab[, var_group := "Total"]

## Define grouping variables
groups <- c("female", "age_group", "background_group", "income_group", "poverty")

for (i in groups) {
  x <- procedures_covid[
    , .(n = .N, n_person = length(na.omit(unique(rinpersoon)))),
    by = c("week", "year", i)]
  x[, var_group := i]
  x[, var_name := x[[i]]]
  x[, var_name := as.character(var_name)]
  x[[i]] <- NULL
  tab <- rbind(tab, x)
}

## Remove small counts for CBS output
tab[n_person < 10, n := NA]
tab[n_person < 10, n_person := NA]

## Round to 5s for CBS output
tab[, n_person := DescTools::RoundTo(n_person, 5)]
tab[, n := DescTools::RoundTo(n, 5)]

## Rename
tab[(tab$var_group == "female") & tab$var_name == "TRUE", var_name := "Female"]
tab[(tab$var_group == "female") & tab$var_name == "FALSE", var_name := "Male"]

tab[var_group == "poverty" & tab$var_name == "TRUE", var_name := "Below poverty"]
tab[var_group == "poverty" & tab$var_name == "FALSE", var_name := "Above poverty"]

## Make wide (activities)
tab_n <- tab %>%
  select(-var_group, -n_person) %>%
  pivot_wider(names_from = var_name, values_from = c("n"))

## Make wide (individuals)
tab_n_person <- tab %>%
  select(-var_group, -n) %>%
  pivot_wider(names_from = var_name, values_from = c("n_person"))

## Write
openxlsx::write.xlsx(list("n_activities" = tab_n,
                          "n_persons" = tab_n_person),
                          "./data/final/covid_by_week_v3.xlsx")

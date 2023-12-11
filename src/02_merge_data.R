####################################################################################
# Script: Procedures and Demographics Data Merging for Analysis
# Description: This script generates analysis data by loading and merging activities
#              data from 2016 to 2021 with demographic data.
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Procedures Data (per year, v3):
#      - 'data/edit/procedures_nl_2016_v3.rda'
#      - 'data/edit/procedures_nl_2017_v3.rda'
#      - 'data/edit/procedures_nl_2018_v3.rda'
#      - 'data/edit/procedures_nl_2019_v3.rda'
#      - 'data/edit/procedures_nl_2020_v3.rda'
#      - 'data/edit/procedures_nl_2021_v3.rda'
#   3. Full Demographic Data (v3): 'data/edit/dem_full_v3.rda'
#
# Output Files:
#   1. Merged Procedures Data: 'data/edit/procedures_v3.rda'
#   2. Merged Demographic Data: 'data/edit/dem_full_v3.rda'
#   3. NA Demographics Overview by Year: './data/output/NA_by_year_v3.xlsx'
#
# Libraries Used:
#   - tidyverse
#   - lubridate
#   - data.table
#   - writexl
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################

library(tidyverse)
library(lubridate)
library(data.table)
source("./src/functions.R")


## Load datasets of procedures per year and subset relevant variables
load("data/edit/procedures_nl_2016_v3.rda")

procedures_2016 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

load("data/edit/procedures_nl_2017_v3.rda")
procedures_2017 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

load("data/edit/procedures_nl_2018_v3.rda")
procedures_2018 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

load("data/edit/procedures_nl_2019_v3.rda")
procedures_2019 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

load("data/edit/procedures_nl_2020_v3.rda")
procedures_2020 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

load("data/edit/procedures_nl_2021_v3.rda")
procedures_2021 <- activity_data_merged %>%
  rename(medical_type = hoofdgroep) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type",
         "covid_activity_dbc", "year", "medical_type", "vektmszsoortinstelling4cat") %>%
  as.data.table()
activity_data_merged <- NULL

## Bind into single dataset
p_list <- list(procedures_2016, procedures_2017, procedures_2018,
               procedures_2019, procedures_2020, procedures_2021)

rm(procedures_2016, procedures_2017, procedures_2018, procedures_2019,
   procedures_2020, procedures_2021)


p_dt <- rbindlist(p_list)
rm(p_list)

## Remove activities in 2016
p_dt[, year := as.numeric(year)]
p_dt <- p_dt[year %in% c(2017, 2018, 2019, 2020, 2021), ]

## Merge in demographic variables into procedures
load("data/edit/dem_full_v3.rda")

dem_full[, rinpersoon := as.numeric(rinpersoon)]
dem_full[, year := as.numeric(year)]
dem_full <- dem_full[year %in% c(2017, 2018, 2019, 2020, 2021), ]

## Re-merge background
dem_full$background_group[dem_full$background_group %in%
                            c("cit_west", "cit_diaspora", "cit_non_west")] <- "cit_non_dutch"  
dem_full$background_group[dem_full$background_group %in%
                            c("cit_west", "cit_diaspora", "cit_non_west")] <- "cit_non_dutch"

## Merge demography into activity sets
data.table::setkey(p_dt, rinpersoon, year)
data.table::setkey(dem_full, rinpersoon, year)

procedures_dem <- merge(
  p_dt,
  dem_full,
  all.x = T
)

## Check missingess
print(paste0('Same length? ', as.character(nrow(p_dt) == nrow(procedures_dem))))
print(paste0('Share missing (female): ', sum(is.na(procedures_dem$female))/nrow(procedures_dem)))

## Subset to adult population
dem_full <- dem_full[age >= 18, ]
procedures_dem <- procedures_dem[age >= 18, ]

## Add weekly variable
procedures_dem[, week := lubridate::week(activity_date)]

## Take out all NA demographics (people who do not live in Netherlands)
procedures <- procedures_dem[!is.na(procedures_dem$poverty), ]
rm(procedures_dem)

## Make urgency coding numeric
procedures <- procedures %>%
  assign_numeric_urgency()

## save files
save(procedures, file = "data/edit/procedures_v3.rda")
save(dem_full, file = "data/edit/dem_full_v3.rda")

## Check NA demographics
NA_overview <- procedures[, .(mean_na = mean(is.na(age))), by = year]
writexl::write_xlsx(NA_overview, "./data/output/NA_by_year_v3.xlsx")

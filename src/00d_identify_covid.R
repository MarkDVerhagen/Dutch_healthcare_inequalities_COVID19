####################################################################################
# Script: COVID Hospital Stay Analysis
# Description: This script identifies procedures that included a hospital stay
#              due to COVID-19 for the years 2020 and 2021. It processes health
#              activity data and hospital records to isolate COVID-related cases.
#
# Input Files:
#   1. LBZ Basis Data (Hospital records with ICD10 codes):
#      - './data/raw/LBZBASIS2020TABV1.rds'
#      - './data/raw/LBZBASIS2021TABV1.rds'
#   2. Health Activities Data for COVID years:
#      - 'data/raw/health_activities/MSZZorgactiviteitenVEKT2020_trim.rds'
#      - 'data/raw/health_activities/MSZZorgactiviteitenVEKT2021_trim.rds'
#
# Output Files:
#   1. COVID DBC IDs for each year:
#      - './data/edit/covid_dbc_ids_2020.rda'
#      - './data/edit/covid_dbc_ids_2021.rda'
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - lubridate
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################


## Load libraries
library(tidyverse)
library(data.table)

## Setup paths
raw_dir <- file.path("data", "raw")

for (year in c("2020", "2021")) {
  
  lbz_file <- paste0("LBZBASIS", year, "TABV1.rds")
  activity_file <- paste0("MSZZorgactiviteitenVEKT",
                          year, "_trim.rds")
  
  ## Read in DHD which includes ICD10 codes
  lbz <- read_rds(file.path(raw_dir,
                            lbz_file) %>%
    as.data.table()
  names(lbz) <- tolower(names(lbz))
  
  lbz[, rinpersoon := as.numeric(rinpersoon)]
  
  ## Load data on health activities for COVID years
  activity_data <- read_rds(file.path(raw_dir, "health_activities",
                                      activity_file)) %>%
    as.data.table()
  
  setnames(activity_data, names(activity_data), tolower(names(activity_data)))
  
  activity_data <- activity_data[, rinpersoon := as.numeric(rinpersoon)]
  
  ## Define COVID ICD10 codes
  covid_icd10 <- c("U071", "U072")
  
  ## Only select individuals who have been in hospital with COVID
  lbz[, covid_icd10 := ifelse((lbzicd10hoofddiagnose %in% covid_icd10) |
                                (lbzicd10hoofddiagnoseimp %in% covid_icd10),
                              1, 0)]
  
  lbz$lbzopnamedatum <- lubridate::ymd(lbz$lbzopnamedatum)
  lbz$lbzontslagdatum <- lubridate::ymd(lbz$lbzontslagdatum)
  
  lbz$duration <- lbz$lbzontslagdatum - lbz$lbzopnamedatum
  
  ## Only continue with COVID patients
  lbz_covid <- lbz[covid_icd10 == 1, ]
  
  ## Format dates
  lbz_covid[, start_date := as.Date(lbzopnamedatum,
                                    format = "%Y-%m-%d")]
  lbz_covid[, end_date := as.Date(lbzontslagdatum,
                                  format = "%Y-%m-%d")]
  
  ## Subset to include relevant columns only
  lbz_covid_rel <- lbz_covid[, c("rinpersoon", "start_date", "end_date")]
  
  ## Make counter for people who entered hospital multiple times
  lbz_covid_rel$one <- 1
  lbz_covid_rel[, count := cumsum(one), by = rinpersoon]
  
  ## Make a wide datatable with unique rinpersonen and various start and end dates
  start_dates <- dcast(lbz_covid_rel, rinpersoon ~ count, value.var = "start_date")
  names(start_dates)[2:ncol(start_dates)] <- paste0("start_date_", names(start_dates)[2:ncol(start_dates)])
  
  end_dates <- dcast(lbz_covid_rel, rinpersoon ~ count, value.var = "end_date")
  names(end_dates)[2:ncol(end_dates)] <- paste0("end_date_", names(end_dates)[2:ncol(end_dates)])
  
  wide_dates <- start_dates %>%
    left_join(end_dates)
  
  ## Subset those activities of people who were in hospital with covid
  activity_data_covid <- activity_data[rinpersoon %in% wide_dates$rinpersoon, ]
  
  ## Join start and end dates
  activity_data_covid_inc_lbz <- activity_data_covid %>%
    left_join(wide_dates)
  
  ## Format dates
  activity_data_covid_inc_lbz[, activity_date := as.Date(paste0(
    substr(vektmszzorgactiviteitdatum, 1, 4), "-",
    substr(vektmszzorgactiviteitdatum, 5, 6), "-",
    substr(vektmszzorgactiviteitdatum, 7, 8)),
    format = "%Y-%m-%d")]
  
  ## RERUN: wrong window
  
  activity_data_covid_inc_lbz[, covid_activity := ((activity_date <= end_date_1) &
                                                     (activity_date >= start_date_1)) |
                                ((activity_date <= end_date_2) &
                                   (activity_date >= start_date_2)) |
                                ((activity_date <= end_date_3) &
                                   (activity_date >= start_date_3)) |
                                ((activity_date <= end_date_4) &
                                   (activity_date >= start_date_4)) |
                                ((activity_date <= end_date_5) &
                                   (activity_date >= start_date_5))]
  
  activity_data_covid_inc_lbz$covid_activity[is.na(activity_data_covid_inc_lbz$covid_activity)] <- FALSE
  
  covid_dbc_ids <- activity_data_covid_inc_lbz$vektmszkoppelidprestza[activity_data_covid_inc_lbz$covid_activity]
  
  save(covid_dbc_ids, file = paste0("./data/edit/covid_dbc_ids_", year, ".rda"))
}

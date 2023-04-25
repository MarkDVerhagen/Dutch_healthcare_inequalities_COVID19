## Script to identify procedures that included a hospital stay with covid

# Load libraries
library(tidyverse)
library(data.table)

## Read in DHD which includes ICD10 codes
lbz <- read_rds("./data/raw/LBZBASIS2020TABV1.rds") %>%
  as.data.table()
names(lbz) <- tolower(names(lbz))
lbz[, rinpersoon := as.numeric(rinpersoon)]

## Load data on health activities for 2020 (COVID year)
activity_data <- read_rds(
  "data/raw/health_activities/MSZZorgactiviteitenVEKT2020_trim.rds") %>%
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
start_dates <- dcast(lbz_covid_rel, rinpersoon ~ count,
                     value.var = "start_date")
names(start_dates)[2:ncol(start_dates)] <- paste0(
  "start_date_", names(start_dates)[2:ncol(start_dates)])

end_dates <- dcast(lbz_covid_rel, rinpersoon ~ count, value.var = "end_date")
names(end_dates)[2:ncol(end_dates)] <- paste0(
  "end_date_", names(end_dates)[2:ncol(end_dates)])

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

## Label all activities falling within the covid DBC windows as
## Covid activties

activity_data_covid_inc_lbz[, covid_activity :=
                            ((activity_date <= end_date_1) &
                             (activity_date >= start_date_1)) |
                            ((activity_date <= end_date_2) &
                             (activity_date >= start_date_2)) |
                            ((activity_date <= end_date_3) &
                             (activity_date >= start_date_3)) |
                            ((activity_date <= end_date_4) &
                             (activity_date >= start_date_4)) |
                            ((activity_date <= end_date_5) &
                             (activity_date >= start_date_5))]

activity_data_covid_inc_lbz$covid_activity[
  is.na(activity_data_covid_inc_lbz$covid_activity)] <- FALSE

covid_dbc_ids <- activity_data_covid_inc_lbz$vektmszkoppelidprestza[
  activity_data_covid_inc_lbz$covid_activity]

save(covid_dbc_ids, file = "./data/edit/covid_dbc_ids.rda")

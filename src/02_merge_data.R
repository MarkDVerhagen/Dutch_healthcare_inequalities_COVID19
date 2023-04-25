# ---
# Generate analysis data
# --- 

library(tidyverse)
library(lubridate)
library(data.table)

## Load datasets of procedures in Amsterdam
# Load NL sets
time <- Sys.time()
load("data/edit/procedures_nl_2016_v2.rda")
procedures_2016 <- activity_data_merged %>%
  rename(medical_type = hoofdgroepNZa) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type", "specialisation",
         "covid_activity_dbc", "year", "medical_type")
activity_data_merged <- NULL
Sys.time() - time

time <- Sys.time()
load("data/edit/procedures_nl_2017_v2.rda")
procedures_2017 <- activity_data_merged %>%
  rename(medical_type = hoofdgroepNZa) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type", "specialisation",
         "covid_activity_dbc", "year", "medical_type")
activity_data_merged <- NULL
Sys.time() - time

time <- Sys.time()
load("data/edit/procedures_nl_2018_v2.rda")
procedures_2018 <- activity_data_merged %>%
  rename(medical_type = hoofdgroepNZa) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type", "specialisation",
         "covid_activity_dbc", "year", "medical_type")
activity_data_merged <- NULL
Sys.time() - time

time <- Sys.time()
load("data/edit/procedures_nl_2019_v2.rda")
procedures_2019 <- activity_data_merged %>%
  rename(medical_type = hoofdgroepNZa) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type", "specialisation",
         "covid_activity_dbc", "year", "medical_type")
activity_data_merged <- NULL
Sys.time() - time

time <- Sys.time()
load("data/edit/procedures_nl_2020_v2.rda")
procedures_2020 <- activity_data_merged %>%
  rename(medical_type = hoofdgroepNZa) %>%
  select("rinpersoon", "activity_date", "urgency", "activity_type", "specialisation",
         "covid_activity_dbc", "year", "medical_type")
activity_data_merged <- NULL
Sys.time() - time

procedures_2016 <- procedures_2016 %>% as.data.table()
procedures_2017 <- procedures_2017 %>% as.data.table()
procedures_2018 <- procedures_2018 %>% as.data.table()
procedures_2019 <- procedures_2019 %>% as.data.table()
procedures_2020 <- procedures_2020 %>% as.data.table()

p_list <- list(procedures_2016, procedures_2017, procedures_2018,
               procedures_2019, procedures_2020)

rm(procedures_2016, procedures_2017, procedures_2018, procedures_2019,
   procedures_2020)
gc()

p_dt <- rbindlist(p_list)
rm(p_list)
gc()


p_dt[, year := as.numeric(year)]
p_dt <- p_dt[year %in% c(2017, 2018, 2019, 2020), ]
gc()

## Merge in demographic variables into procedures
load("data/edit/dem_full.rda")

## clear space
dem_full[, rinpersoon := as.numeric(rinpersoon)]
dem_full[, year := as.numeric(year)]
dem_full <- dem_full[year %in% c(2017, 2018, 2019, 2020), ]
gc()

data.table::setkey(p_dt, rinpersoon, year)
data.table::setkey(dem_full, rinpersoon, year)

procedures_dem <- merge(
  p_dt,
  dem_full,
  all.x = T
)

## Check missingess
print(paste0('Same length? ',
             as.character(nrow(p_dt) == nrow(procedures_dem))))
print(paste0('Share missing (female): ',
             sum(is.na(procedures_dem$female))/nrow(procedures_dem)))

rm(p_dt)
gc()

## Make set for 2017-2020
procedures_dem[, week := lubridate::week(activity_date)]
gc()

## save file
save(procedures_dem, file = "data/edit/procedures_dem.rda")

## Check NA demographics
rm(dem_full)
gc()
NA_overview <- procedures_dem[, .(mean_na = mean(is.na(age))), by = year]
writexl::write_xlsx(NA_overview, "./data/output/NA_by_year.xlsx")

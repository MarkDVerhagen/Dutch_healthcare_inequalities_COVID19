####################################################################################
# Script: Activity and Demographic Analysis
# Description: This script generates yearly sets of activities for analysis, which include
#              DBC diagnosis, activity type, and urgency. It also generates yearly demographics
#
# Input Files:
#   1. Functions: './src/functions.R'
#   2. Urgency Classification: 'data/edit/urgency_classification.rds'
#   3. DBC Classification Data: 'data/helper/20200319; versie 4-1F patientengroepen NZa.xlsx'
#   4. Activity Classification: 'data/edit/activity_classification.rds'
#   5. Health Activities Data (2016-2021): 'data/raw/health_activities/MSZZorgactiviteitenVEKT{year}_trim.rds'
#   6. Health Products Data (2016-2021): 'data/raw/health_procedures/health_codes_{year}.rds'
#   7. COVID DBC IDs (2020, 2021): './data/edit/covid_dbc_ids_{year}.rda'
#   8. Demographic Data (2016-2020): 'data/raw/demographics/{year}/rin_demog.rds'
#
# Output Files:
#   1. Procedures Data (per year): 'data/edit/procedures_nl_{year}_v3.rda'
#   2. Demographic Data Summary: 'data/output/desc_demog_by_year_v3.xlsx'
#   3. Full Demographic Data: 'data/edit/dem_full.rda'
#
# Libraries Used:
#   - tidyverse
#   - data.table
#
# Author: Mark Verhagen
# Date: 10-12-2023
####################################################################################

library(tidyverse)
library(data.table)

## Booleans what to generate
procedures <- T
demographic <- F

years <- c(2016:2021)

source("./src/functions.R")

## Setup paths
raw_dir <- file.path("data", "raw")
edit_dir <- file.path("data", "edit")

## Load activity classification crosswalks for Urgency level and Activity type
urgency_classification <- read_rds(file.path(edit_dir, "urgency_classification.rds")) %>%
  as.data.table()

## Update to 2021
urgency_classification <- urgency_code_2021(urgency_classification)

## Add merge code for urgency classification to health products
urgency_classification[, merge_urgency := paste0(str_pad(specialisation, 4, "left", "0"), "-",
                                                 str_pad(diagnosis_dddd, 4, "left", "0"), "-",
                                                 as.character(health_product))]

# There are 6 duplicate codes, all same urgency > omit duplicates
urgency_classification <- urgency_classification[!duplicated(urgency_classification$merge_urgency), ]

## Load DBC classification data
dbc_d_classification <- readxl::read_xlsx(
  file.path(helper_dir, "20200319; versie 4-1F patientengroepen NZa.xlsx"),
            sheet = "DBC-doelgroepNZa_koppeltabel") %>%
  rename(NZA_DIAGNOSE = DIAGNOSE,
         NZA_SPEC_CODE = SPEC_CODE,
         NZA_HOOFDGROEP = `Hoofdgroep code`) %>%
  rowwise() %>%
  mutate(NZA_HOOFDGROEP = case_when(
    nchar(NZA_HOOFDGROEP) == 1 ~ paste0("0", NZA_HOOFDGROEP),
    TRUE ~ NZA_HOOFDGROEP
  ))

## Load activity classification
activity_classification <- read_rds(
  file.path(helper_dir, "./data/edit/activity_classification.rds")) %>%
  as.data.table()

## Define demography data.table to fill
dem_full <- data.table()

for(year in years) {
  
  if(procedures) {
    print(paste("Getting health data for year: ", year))
    
    ## Data on health activities 
    activity_data <- read_rds(
      file.path(raw_dir,
                paste0("health_activities/MSZZorgactiviteitenVEKT", year, "_trim.rds"))) %>%
      select(-VEKTMSZUitvoerendSpecialisme) %>%
      as.data.table()
    
    n_activity <- nrow(activity_data)
    
    # Rename cols
    names(activity_data) <- tolower(names(activity_data))
    setnames(activity_data, c("vektmszkoppelidprestza", "vektmszzorgactiviteitdatum",
                              "vektmszzorgactiviteit"),
             c("dbc_id", "activity_date", "activity"))
    
    # Transform person_id to numeric
    activity_data[, rinpersoon := as.numeric(rinpersoon)]
    
    ## Data on health products
    product_data <- read_rds(file.path(raw_dir,
                                       paste0("health_procedures/health_codes_", year, ".rds"))) %>%
      as.data.table()
    
    # Rename cols
    names(product_data) <- tolower(names(product_data))
    setnames(product_data, c("vektmszdbczorgproduct", "vektmszkoppelidprestza"),
             c("health_product", "dbc_id"))
    product_data[, c("rinpersoon", "health_product") := list(as.numeric(rinpersoon), as.numeric(health_product))]
    product_data <- merge_NZA_classification(product_data, dbc_d_classification)
    
    ## Unit tests 
    # DBC_ids are unique
    assertthat::assert_that(!any(duplicated(product_data$dbc_id)))
    # All activities have a DBC in products
    assertthat::assert_that(mean(activity_data$dbc_id %in% product_data$dbc_id) > 0.9999)
    # All individuals with an activity have a product
    assertthat::assert_that(mean(activity_data$rinpersoon %in% product_data$rinpersoon) > 0.9999)
    
    # Merge: Only look at procedures (both product and activity)
    activity_data_merged <- activity_data[product_data, on = c("dbc_id"), nomatch = 0]
    
    ## Separate out all procedures without dbc codes
    ozp_data <- product_data[!(product_data$dbc_id %in% activity_data_merged$dbc_id), ]
    
    print(paste0("Percentage of 9999 diagnoses in other products: ",
                 mean(ozp_data$vektmszspecialismediagnosecombin == "9999-99-99-9999")))
    print(paste0("Percentage of other products: ", nrow(ozp_data) / nrow(product_data)))
    
    rm(product_data, activity_data)
    gc()
    
    # Merge with information on activity type (zpk)
    activity_data_merged <- merge(activity_data_merged, activity_classification, by = "activity", all.x = T)
    
    print("Activity type NA")
    print(mean(is.na(activity_data_merged$activity_type))) ## Number of activities missing activity types
    
    # Merge with urgency data
    activity_data_merged[, merge_urgency := paste0(str_sub(vektmszspecialismediagnosecombin, 1, 4), "-",
                                            str_sub(vektmszspecialismediagnosecombin, 12, 15), "-",
                                            health_product)]
    
    activity_data_merged <- merge(activity_data_merged, urgency_classification[, c("urgency", "merge_urgency")],
                           by = "merge_urgency", all.x = T)
    print("Urgency NA")
    print(mean(is.na(activity_data_merged$urgency))) ## Number of activities missing urgency codes
    
    activity_data_merged$activity_date <- lubridate::ymd(activity_data_merged$activity_date)
    activity_data_merged$year <- lubridate::year(activity_data_merged$activity_date)
    
    ## add covid dbc indicator
    if (year %in% c(2020, 2021)) {
      ## load Covid DBC ids
      load(paste0("./data/edit/covid_dbc_ids_", year, ".rda"))
      activity_data_merged[, covid_activity_dbc := dbc_id %in%
                      covid_dbc_ids]
    } else {
      activity_data_merged$covid_activity_dbc <- FALSE
    }
    
    #Save
    save(activity_data_merged, file =
         file.path(edit_dir, paste0("procedures_nl_", year, "_v3.rda")))
    gc()
  }
  
  if (demographic) {
    
    if (year != 2021) {
      ## Start with year 2016 and make it the demography file for 2017 data
      ## Stop with year 2020, which is demography for 2021
      
      # DEMOGRAPHIC DATA
      print(paste("Getting demographic data for year: ", year + 1))
      
      # read demographic data
      dem <- readRDS(file.path(
        raw_dir,
        paste0("demographics/", year, "/rin_demog.rds"))) %>%
        as.data.table()
      
      names(dem) <- gsub("_\\d{4}", "", names(dem))
      
      dem <- dem[, c("year", "gem", "wc", "bc") :=
                   list(as.numeric(year), 
                        as.character(gem),
                        as.character(wc),
                        as.character(bc))]
      dem$year <- year + 1
      dem_full <- rbindlist(list(dem_full, dem), fill = TRUE, use.names = TRUE)
      rm(dem)
      gc()
    }
  }
}

# clean demographic data ####
setnames(dem_full, "leeftijd", "age")

dem_full <- dem_full[
  ,
  c("female", "age_group", "background_group") :=
  list(geslacht == "vrouw",
       case_when(
         age < 18 ~ "0-17",
         age < 30 ~ "18-29",
         age < 66 ~ "30-65",
         age < 76 ~ "66-75",
         TRUE ~ "76+"
        ),
       case_when(
         herkomst %in% c("Nederland", "Autochtoon") ~ "cit_dutch",
         herkomst %in% c("Overige westerse migratieachtergrond",
                         "Westers allochtoon",
                         "Europa (exclusief Nederland)",
                         "Poolse migratieachtergrond of MOE-landers") ~ "cit_west",
         herkomst %in% c("Overige niet-westers allochtoon",
                         "Overige niet westerse migratieachtergrond",
                         "Somalische migratieachtergrond",
                         "Syrische migratieachtergrond") ~ "cit_non_west",
         herkomst %in% c("Marokko", "Turkije", "Suriname", "Nederlands Antillen en Aruba") ~ "cit_diaspora"
         ))]

## Remove the original variables
dem_full <- dem_full %>%
  select(-geslacht, -herkomst, -huishoudsamenstelling, -inkomen_klasse, -wc, -bc,
         -herkomst_eerstegen, -inkomen_pers, -income_class)

## Check share of respondents across groups
prop.table(table(dem_full$year , dem_full$female), 1)
prop.table(table(dem_full$year , dem_full$income_group), 1)
prop.table(table(dem_full$year , dem_full$background_group), 1)
prop.table(table(dem_full$year , dem_full$age_group), 1)
prop.table(table(dem_full$year , dem_full$poverty), 1)


year_desc <- rbind(
  dem_full %>%
    group_by(year, income_group) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = c("year"),
                values_from = c("n")) %>%
    rename(group = income_group) %>%
    mutate(grouping_var = "income_group"),
  dem_full %>%
    group_by(year, age_group) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = c("year"),
                values_from = c("n")) %>%
    rename(group = age_group) %>%
    mutate(grouping_var = "age_group"),
  dem_full %>%
    group_by(year, background_group) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = c("year"),
                values_from = c("n")) %>%
    rename(group = background_group) %>%
    mutate(grouping_var = "background_group"),
  dem_full %>%
    group_by(year, female) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = c("year"),
                values_from = c("n")) %>%
    rename(group = female) %>%
    mutate(grouping_var = "female"),
  dem_full %>%
    group_by(year) %>%
    summarise(age = mean(age)) %>%
    pivot_wider(names_from = c("year"),
                values_from = c("age")) %>%
    mutate(grouping_var = "age",
           group = "age")
)

writexl::write_xlsx(year_desc, "data/output/desc_demog_by_year_v3.xlsx")

#Save demography file
save(dem_full, file = "data/edit/dem_full.rda")

## Script to generate set of activities for analysis which includes DBC diagnosis, activity type and
## urgency.

## Load libraries
library(tidyverse)
library(data.table)
source("./src/functions.R")

procedures <- F
demographic <- T
# sample <- T
years <- 2016:2020

## Load activity classification crosswalks for Urgency level and Activity type
urgency_classification <- read_rds("data/edit/urgency_classification.rds") %>%
  as.data.table()
urgency_classification[, merge_urgency := paste0(str_pad(specialisation, 4, "left", "0"), "-",
                                                 str_pad(diagnosis_dddd, 4, "left", "0"), "-",
                                                 as.character(health_product))]

# There are 6 duplicate codes, all same urgency
urgency_classification <- urgency_classification[!duplicated(urgency_classification$merge_urgency), ]

## Load DBC classification data
dbc_d_classification <- readxl::read_xlsx("H:/data/raw/20200319; versie 4-1F patientengroepen NZa.xlsx",
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
activity_classification <- read_rds("data/edit/activity_classification.rds") %>%
  as.data.table()

## Define demography data.table to fill
dem_full <- data.table()

for(year in years) {
  if(procedures) {
    print(paste("Getting health data for year: ", year))
    
    ## Data on health activities 
    activity_data <- read_rds(paste0("data/raw/health_activities/MSZZorgactiviteitenVEKT", year, "_trim.rds")) %>%
      select(-VEKTMSZUitvoerendSpecialisme) %>%
      as.data.table()
    
    n_activity <- nrow(activity_data)
    
    # Rename cols
    setnames(activity_data, c("RINPERSOON", "VEKTMSZKoppelIDPrestZa", "VEKTMSZZorgactiviteitdatum",
                              "VEKTMSZZorgactiviteit"),
             c("rinpersoon", "dbc_id", "activity_date", "activity"))
    
    # Transform person_id to numeric
    activity_data[, rinpersoon := as.numeric(rinpersoon)]
    
    ## Data on health products
    product_data <- read_rds(paste0("data/raw/health_procedures/health_codes_", year, ".rds")) %>%
      as.data.table()
    
    # Rename cols
    setnames(product_data, c("RINPERSOON", "VEKTMSZDBCZorgproduct", "VEKTMSZKoppelIDPrestZa"),
             c("rinpersoon", "health_product", "dbc_id"))
    product_data[, c("rinpersoon", "health_product") := list(as.numeric(rinpersoon), as.numeric(health_product))]
    product_data <- merge_NZA_classification(product_data, dbc_d_classification)
    
    # Unit tests
    assertthat::assert_that(!any(duplicated(product_data$dbc_id))) ## DBC_ids are unique
    assertthat::assert_that(mean(activity_data$dbc_id %in% product_data$dbc_id) > 0.9999) ## All activities have a DBC in products
    assertthat::assert_that(mean(activity_data$rinpersoon %in% product_data$rinpersoon) > 0.9999) ## All individuals with an activity have a product
    
    # Merge: Only look at procedures (both product and activity)
    activity_data_merged <- activity_data[product_data, on = c("dbc_id"), nomatch = 0]
    
    ## Separate out all procedures without dbc codes
    ozp_data <- product_data[!(product_data$dbc_id %in% activity_data_merged$dbc_id), ]
    
    print(paste0("Percentage of 9999 diagnoses in other products: ", mean(ozp_data$VEKTMSZSpecialismeDiagnoseCombin == "9999-99-99-9999")))
    print(paste0("Percentage of other products: ", nrow(ozp_data) / nrow(product_data)))
    
    rm(product_data, activity_data)
    gc()
    
    # Merge with information on activity type (zpk)
    activity_data_merged <- merge(activity_data_merged, activity_classification, by = "activity", all.x = T)
    
    print("Activity type NA")
    print(mean(is.na(activity_data_merged$activity_type))) ## Number of activities missing activity types
    
    # Merge with urgency data
    activity_data_merged[, merge_urgency := paste0(str_sub(VEKTMSZSpecialismeDiagnoseCombin, 1, 4), "-",
                                            str_sub(VEKTMSZSpecialismeDiagnoseCombin, 12, 15), "-",
                                            health_product)]
    
    activity_data_merged <- merge(activity_data_merged, urgency_classification[, c("urgency", "merge_urgency")],
                           by = "merge_urgency", all.x = T)
    print("Urgency NA")
    print(mean(is.na(activity_data_merged$urgency))) ## Number of activities missing urgency codes
    
    activity_data_merged$activity_date <- lubridate::ymd(activity_data_merged$activity_date)
    activity_data_merged$year <- lubridate::year(activity_data_merged$activity_date)
    
    ## add covid dbc indicator
    if (year == 2020) {
      ## load Covid DBC ids
      load("./data/edit/covid_dbc_ids.rda")
      activity_data_merged[, covid_activity_dbc := dbc_id %in%
                      covid_dbc_ids]
    } else {
      activity_data_merged$covid_activity_dbc <- FALSE
    }
    
    #Save
    save(activity_data_merged, file = paste0("data/edit/procedures_nl_", year, "_v2.rda"))
    save(ozp_data, file = paste0("data/edit/ozp_nl_", year, ".rda"))
    rm(activity_data_merged, activity_data_merged_sub, ozp_data)
    gc()
  }
  
  if (demographic) {
    
    if (year != 2020) {
      ## Start with year 2016 and make it the demography file for 2017 data
      ## Stop with year 2019, which is demography for 2020
      
      # DEMOGRAPHIC DATA
      print(paste("Getting demographic data for year: ", year + 1))
      
      # read demographic data
      dem <- readRDS(paste0("data/raw/demographics/", year, "/rin_demog.rds")) %>%
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

## Include custom income
numeric_income <- lapply(c(2016:2019), FUN = function(x) {
  read_rds(paste0("H:/data/numeric_income/", x, "/rin_num_income.rds")) %>%
    mutate(year = x + 1)
})

numeric_income_assigned <- lapply(numeric_income, classify_income)
rm(numeric_income)
gc()

numeric_income_assigned_comb <- bind_rows(numeric_income_assigned)
numeric_income_assigned_comb$year <- as.numeric(numeric_income_assigned_comb$year)

numeric_income_assigned_comb <- numeric_income_assigned_comb %>%
  as.data.table()

rm(numeric_income_assigned)
gc()

## Also use new income classes for demographics
dem_full[, rinpersoon := as.numeric(rinpersoon)]

setkey(numeric_income_assigned_comb, rinpersoon, year)
setkey(dem_full, rinpersoon, year)

dem_full <- merge(dem_full, numeric_income_assigned_comb, all.x = T)
table(dem_full$income_class)
dem_full$income_class[is.na(dem_full$income_class)] <- "Other"
table(dem_full$income_class)

dem_full$income_group <- dem_full$income_class

# clean demographic data ####
setnames(dem_full, "leeftijd", "age")

dem_full <- dem_full[,
                     c("female", "age_group",
                       "background_group"
                     ) :=
                       list(geslacht == "vrouw",
                            case_when(
                              age < 18 ~ "0-17",
                              age < 30 ~ "18-29",
                              age < 66 ~ "30-65",
                              age < 76 ~ "66-75",
                              TRUE ~ "76+"
                            ),
                            case_when(
                              herkomst %in%
                              c("Nederland", "Autochtoon") ~ "cit_dutch",
                              herkomst %in%
                              c("Overige westerse migratieachtergrond",
                                "Westers allochtoon",
                                "Europa (exclusief Nederland)",
                                "Poolse migratieachtergrond of MOE-landers") ~ "cit_west",
                              herkomst %in%
                              c("Overige niet-westers allochtoon",
                                "Overige niet westerse migratieachtergrond",
                                "Somalische migratieachtergrond",
                                "Syrische migratieachtergrond") ~ "cit_non_west",
                              herkomst %in%
                              c("Marokko", "Turkije", "Suriname",
                                "Nederlands Antillen en Aruba") ~ "cit_diaspora"
                              )
                       )]

## Remove the original variables
dem_full <- dem_full %>%
  select(-geslacht, -herkomst, -huishoudsamenstelling, -inkomen_klasse, -wc, -bc,
         -herkomst_eerstegen, -inkomen_pers, -income_class)

# check share of respondents across groups
# prop.table(table(dem_full$year , dem_full$female), 1)
# prop.table(table(dem_full$year , dem_full$income_group), 1)
# prop.table(table(dem_full$year , dem_full$background_group), 1)
# prop.table(table(dem_full$year , dem_full$age_group), 1)
# prop.table(table(dem_full$year , dem_full$poverty), 1)


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

writexl::write_xlsx(year_desc, "data/output/desc_demog_by_year.xlsx")

#Save
save(dem_full, file = paste0("data/edit/dem_full.rda"))

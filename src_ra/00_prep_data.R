### -- 00a_prep_data.R
## Description: This script performs some basic formatting of source data
###

library(tidyverse)

## Load functions
source("./src/functions.R")

# load and format urgency data
excel_file <- "./data/raw/Urgentielijst medisch-specialistische zorg - 29 mei 2020.xlsx"
urgency <- NULL

for (i in 3:28) {
  temp <- readxl::read_excel(excel_file, sheet = i) %>% 
    mutate_all(as.character)
  urgency <- bind_rows(urgency, temp)
}

urgency <- urgency %>%
  rename(urgency = "Urgentie-indeling",
         health_product = "Zorgproductcode",
         diagnosis_dddd = "Diagnosecode",
         specialisation = "tab") %>%
  select(urgency, health_product, specialisation, diagnosis_dddd) %>%
  mutate(health_product = as.numeric(health_product),
         urgency = case_when(
           urgency == "A. <24 uur" ~ "A:<24h",
           urgency %in% c("B. <1 week","B. <1 w") ~ "B:<1w",
           urgency %in% c("C. <2 weken","C. < 2week") ~ "C:<2w",
           urgency == "D. <1 maand" ~ "D:<1m",
           urgency == "E. <2 maanden" ~ "E:<2m",
           urgency == "F. <3 maanden" ~ "F:<3m",
           urgency == "G. >3 maanden" ~ "G:>3m"
         ),
         urgency = factor(urgency, levels = c("A:<24h", "B:<1w", "C:<2w",
                                              "D:<1m", "E:<2m", "F:<3m", "G:>3m"))
  )

saveRDS(urgency, "./data/edit/urgency_classification.rds")

# load and format data on procedure type (diagnostic or operational) and activity
activity_classification <- read_csv2("./data/raw/ReflijstZorgactiviteiten.csv") %>%
  rename(activity = MSZZorgactiviteit,
         activity_type = ZPKcode,
         activity_type_des = ZPKomschrijving) %>%
  select(activity, activity_type, activity_type_des)

saveRDS(activity_classification, "./data/edit/activity_classification.rds")

## Load and format raw income data
years <- c(2017:2020)
lapply(years, function(x) {
  read_csv(paste0(
    "H:/data/numeric_income/", x,
    "/rin_num_income.csv"
  )) %>%
    remove_numerics_from_names() %>%
    gen_numeric_income() %>%
    saveRDS(paste0(
      "H:/data/numeric_income/", x,
      "/rin_num_income.rds"
    ))
})

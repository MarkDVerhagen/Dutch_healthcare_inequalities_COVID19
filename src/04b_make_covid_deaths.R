####################################################################################
# Script: COVID-Related Deaths and Demographics Analysis
# Description: This script processes data related to COVID-related deaths for the
#              years 2020 and 2021. It combines this information with demographic
#              data and provides yearly counts of deaths per subgroup and for
#              the total population.
#
# Input Files:
#   1. COVID-related Deaths Data:
#      - 'G:/GezondheidWelzijn/DOODOORZTAB/2020/DOODOORZ2020TABV2.sav'
#      - 'G:/GezondheidWelzijn/DOODOORZTAB/2021/DOODOORZ2021TABV6.sav'
#   2. Full Demographic Data (v3): 'data/edit/dem_full_v3.rda'
#
# Output Files:
#   1. COVID Deaths Data Analysis: './data/final/covid_deaths_v3.xlsx'
#
# Libraries Used:
#   - tidyverse
#   - data.table
#   - haven
#   - DescTools
#   - writexl
#
# Author: Mark Verhagen
# Date: 11-12-2023
####################################################################################

library(tidyverse)
library(data.table)

## Read cause of death files from registry
do_2020 <- haven::read_spss(
  "G:/GezondheidWelzijn/DOODOORZTAB/2020/DOODOORZ2020TABV2.sav")
do_2021 <- haven::read_spss(
  "G:/GezondheidWelzijn/DOODOORZTAB/2021/DOODOORZ2021TABV6.sav")

## Load demographics
load("data/edit/dem_full_v3.rda")

## Define groups
groups <- c("female", "age_group", "background_group", "income_group",
            "poverty")

## Determine COVID-19 deaths
do_2020_rins_c <- as.numeric(do_2020$RINPERSOON[do_2020$UCCODE %in%
                                                  c("U071", "U072")])
do_2021_rins_c <- as.numeric(do_2021$RINPERSOON[do_2021$UCCODE %in%
                                                  c("U071", "U072")])

## Subset demography by COVID-19 deaths
rel_demog_2020 <- dem_full[dem_full$rinpersoon %in% do_2020_rins_c &
                             dem_full$year == "2020", ]
rel_demog_2021 <- dem_full[dem_full$rinpersoon %in% do_2021_rins_c &
                             dem_full$year == "2021", ]

## Bind 2020 and 2021
rel_demog <- bind_rows(list(rel_demog_2020, rel_demog_2021))

## Make aggregate statistics
tab <- rel_demog[, .(n = .N), by = year]
tab[, var_group := "Total"]
tab[, var_name := "Total"]

## Make group aggregate statistics
for (i in groups) {
  x <- rel_demog %>% 
    group_by(year, get(i)) %>% 
    count() %>% 
    mutate(var_group = i) %>%
    rename(var_name = `get(i)`) %>% 
    mutate(var_name = as.character(var_name))
  
  tab <- rbind(tab, x)
}

## Round to 5s for CBS output
tab$n <- DescTools::RoundTo(tab$n, 5)

## Write output
writexl::write_xlsx(tab, "./data/final/covid_deaths_v3.xlsx")

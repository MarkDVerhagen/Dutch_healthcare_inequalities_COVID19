## Script to make descriptive tables

## Load libraries
library(tidyverse)
library(data.table)

## Load analysis data
load("data/edit/full_analysis_sets.rda")
gc()

rm(procedures)
gc()

tab <- dem_full %>% 
  group_by(year) %>% 
  count() %>% 
  mutate(var_group = "Total",
         var_name = "Total")
  

for (i in c("female", "age_group", "background_group",
            "income_group", "poverty")) {
  x <- dem_full %>% 
    group_by(year, get(i)) %>% 
    count() %>% 
    mutate(var_group = i) %>%
    rename(var_name = `get(i)`) %>% 
    mutate(var_name = as.character(var_name))
  
  tab <- rbind(tab, x)
}

# numeric table  
tab <- tab %>% 
  pivot_wider(names_from = year, values_from = n)

# save numeric table
xlsx::write.xlsx(tab, "data/output/dem_table_num.xlsx")

# percent_table 
total <- tab %>% filter(var_group == "Total")

for (i in names(tab)) {
  if (is.numeric(tab[[i]])) {
    tab[[i]] <- round(tab[[i]]/total[[i]], 2)
    tab[[i]][1] <- as.integer(total[[i]])
  }
}

# save percent table
xlsx::write.xlsx(tab, "data/output/dem_table_per.xlsx")


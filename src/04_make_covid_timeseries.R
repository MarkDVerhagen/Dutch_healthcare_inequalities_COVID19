## Produce weekly series of Covid activities

## Load libraries
library(tidyverse)
library(data.table)

load("data/edit/full_analysis_sets.rda")

## Only include covid data
procedures_covid <- procedures[procedures$covid_activity_dbc, ]

assertthat::assert_that(all(procedures_covid$year == 2020))

procedures_covid <- procedures_covid %>% as.data.table()

tab <- procedures_covid[
  , .(n = .N, n_person = length(na.omit(unique(rinpersoon)))), by = week]
tab[, var_name := "Total"]
tab[, var_group := "Total"]

for (i in c("female", "age_group", "background_group",
            "income_group", "poverty")) {
  x <- procedures_covid[
    , .(n = .N, n_person = length(na.omit(unique(rinpersoon)))),
    by = c("week", i)]
  x[, var_group := i]
  x[, var_name := x[[i]]]
  x[, var_name := as.character(var_name)]
  x[[i]] <- NULL
  tab <- rbind(tab, x)
}

tab[n_person < 10, n := NA]
tab[n_person < 10, n_person := NA]


tab[, n_person := DescTools::RoundTo(n_person, 5)]
tab[, n := DescTools::RoundTo(n, 5)]

tab[(tab$var_group == "female") & tab$var_name == "TRUE", var_name := "Female"]
tab[(tab$var_group == "female") & tab$var_name == "FALSE", var_name := "Male"]

tab[var_group == "poverty" & tab$var_name == "TRUE",
    var_name := "Below poverty"]
tab[var_group == "poverty" & tab$var_name == "FALSE",
    var_name := "Above poverty"]

tab_n <- tab %>%
  select(-var_group, -n_person) %>%
  pivot_wider(names_from = var_name, values_from = c("n"))

tab_n_person <- tab %>%
  select(-var_group, -n) %>%
  pivot_wider(names_from = var_name, values_from = c("n_person"))


openxlsx::write.xlsx(list("n_activities" = tab_n,
                          "n_persons" = tab_n_person),
                          "./data/final/covid_by_week.xlsx")

## Script to generate descriptive tables

library(tidyverse)

# set treatment year
treat_year <- 2020

## Load data


# Specify table parameters
var_groups <- c('total', 'female', 'age_group', 'background_group', 'poverty')
order <- c('total', 'female', 'age_group', 'background_group', 'poverty')
dvs <- c("n_s", "n_person_s")
percent <- TRUE

# table function
count_table <- function(data, y, subtype, order, percent = TRUE) {
  
  if (length(unique(data$var_group)) > length(order)) {
    var_left <- unique(by_urgency$var_group)[!unique(by_urgency$var_group) %in% order]
    warning(paste(var_left, "in var_group but not specified in order. Consider excluding or adding to order."))
  }
  tab <- data %>% 
    filter(urgency_type == subtype) %>%
    group_by(year, urgency_type, var_group, var_name) %>% 
    summarise(y := sum(get(y))) %>% 
    group_by(var_group, var_name) %>%
    pivot_wider(names_from = "year", values_from = y) %>%
    arrange(factor(var_group, levels = order))
  
  if (percent) {
    total <- tab %>% filter(var_group == "total")
    
    for (i in names(tab)) {
      if (is.numeric(tab[[i]])) {
        tab[[i]] <- round(tab[[i]] / total[[i]], 2)
        tab[[i]][1] <- as.integer(total[[i]])
      }
    }
  }
  return(tab)
}

dvs <- c("n", "n_person")

all_files <- c("Rel_all", "Rel_Trauma", "Rel_Onc",
               "RelDiag_all", "RelDiag_Trauma", "RelDiag_Onc",
               "Intense_all", "Intense_Trauma", "Intense_Onc",
               "Diag_all", "Diag_Trauma", "Diag_Onc",
               "all_all", "all_Trauma", "all_Onc")

for(dataname in all_files) {
  
  data <- readxl::read_xlsx(paste0("data/", dataname, ".xlsx"))
  
  # remove all but relevant var_groups
  data <- data %>% 
    filter(var_group %in% c(var_groups, "total"))
  
  for (y in dvs) {
    
    if (y %in% c("n", "n_s")) {
      y_name <- "activities"
    } else {
      y_name <- "individuals"
    }
    
    for (t in unique(data$urgency_type)) {
      tab <- count_table(data, y, subtype = t, order, percent = percent)
      write_csv(tab, 
                file = paste0("tables", "/", 
                              "main", "/",
                              y_name, "/", 
                              treat_year, "/", 
                              dataname, "/",
                              "table_",
                              t, "_",
                              if (percent) {
                                paste0("percent")
                              } else {
                                paste0("count")
                              },
                              ".csv"))
    }
  }
}

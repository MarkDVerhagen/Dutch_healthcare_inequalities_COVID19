library(tidyverse)
library(lubridate)
library(broom)
library(ggrepel)
library(data.table)

#' Function to add urgency codes for new products in 2021
#'
#' @param urgency_classification Current urgency classification up until 2020
#' @return Current urgency classifications supplemented with 2021 products
urgency_code_2021 <- function(urgency_classification) {
  urgency_2021 <- readxl::read_xlsx("./producten_2021_nieuw_inc_DBC_code.xlsx")
  urgency_2021 <- urgency_2021[!is.na(urgency_2021$code), ]
  urgency_2021$specialisation <- gsub(
    "-.*", "", urgency_2021$vektmszspecialismediagnosecombin)
  urgency_2021$diagnosis_dddd <- gsub(
    ".*-", "", urgency_2021$vektmszspecialismediagnosecombin)
  urgency_2021$urgency <- case_when(
    grepl("A", urgency_2021$code) ~ "A:<24h",
    grepl("B", urgency_2021$code) ~ "B:<1w",
    grepl("C", urgency_2021$code) ~ "C:<2w",
    grepl("D", urgency_2021$code) ~ "D:<1m",
    grepl("E", urgency_2021$code) ~ "E:<2m",
    grepl("F", urgency_2021$code) ~ "F:<3m",
    grepl("G", urgency_2021$code) ~ "G:>3m"
  )
  
  urgency_classification <- rbind(
    urgency_classification,
    urgency_2021[, c("urgency", "health_product", "specialisation",
                     "diagnosis_dddd")]  
  )
  
  return(urgency_classification)
}


#' Function to generate an interacted demographic variable
#'
#'@param data Datatable with relevant demographic variables
#'@return Datatable with added `full_interact` column

add_interacted_var_group <- function(data) {

  
  data <- as.data.table(data)
  
  data[, poverty_label := "Poor"]
  data[!data$poverty, poverty_label := "Not-poor"]
  
  data[, female_label := "Female"]
  data[!data$female, female_label := "Male"]
  
  data[, background_label := "Dutch"]
  data[data$background_group == "cit_non_dutch",
       background_label := "Non-Dutch"]
  
  data[, full_interact := paste0(age_group, "_",
                                 female_label, "_",
                                 background_label, "_",
                                 poverty_label)]
  data[, gih_interact := paste0(female_label, "_",
                                background_label, "_",
                                poverty_label)]
  data[, ih_interact := paste0(background_label, "_",
                               poverty_label)]
  data$poverty_label <- NULL
  data$female_label <- NULL
  data$background_label <- NULL
  return(data)
}


merge_NZA_classification <- function(df, dbc_d_classification) {
  ## Function to merge in dbc groups
  #' @param df Dataframe with stacked activity data
  #' @param dbc_d_classification Crosswalk table from DBC to NZa group
  #' @param dbc_h_classification Crosswalk table from NZa to DBC H group
  #' @return Dataframe inlcuding DBC classification
  
  # remove NAs in procedures (6)
  old_n <- nrow(df)
  
  df <- df[!is.na(vektmszspecialismediagnosecombin), ]
  
  print(paste("Dropped", (old_n - nrow(df)), "observations due to NA DBC"))
  
  
  df$vektmszspecialismediagnosecombin
  
  # generate variables
  df[, NZA_SPEC_CODE := gsub("-.*", "", vektmszspecialismediagnosecombin)]
  df[, NZA_SPEC_CODE := as.numeric(NZA_SPEC_CODE)]
  df[, NZA_DIAGNOSE := gsub(".*-", "", vektmszspecialismediagnosecombin)]
  df[, NZA_HOOFDGROEP := gsub("\\d{4}-", "", vektmszspecialismediagnosecombin)]
  df[, NZA_HOOFDGROEP := gsub("-.*", "", NZA_HOOFDGROEP)]
  
  # Add a NZa hoofdgroep voor OZP
  
  dbc_d_classification <- bind_rows(dbc_d_classification,
                                    data.frame(
                                      "NZA_DIAGNOSE" = "9999",
                                      "NZA_HOOFDGROEP" = "99",
                                      "NZA_SPEC_CODE" = 9999,
                                      "doelgroepNZa" = 99,
                                      "hoofdgroep" = 99)
  )
  
  # check 
  assertthat::assert_that(mean(df$NZA_SPEC_CODE %in%
                                 dbc_d_classification$NZA_SPEC_CODE) > 0.99)
  assertthat::assert_that(all(mean(nchar(df$NZA_DIAGNOSE)) == 4))
  assertthat::assert_that(mean(df$NZA_DIAGNOSE %in% dbc_d_classification$NZA_DIAGNOSE) > 0.99)
  assertthat::assert_that(all(mean(nchar(df$NZA_HOOFDGROEP)) == 2))
  assertthat::assert_that(mean(df$NZA_HOOFDGROEP %in% dbc_d_classification$NZA_HOOFDGROEP) > 0.99)
  
  # select only relevant variables
  merge_NZA <- dbc_d_classification %>%
    dplyr::select(starts_with("NZA_"), doelgroepNZa, hoofdgroep)
  
  # merge 
  df <- df %>%
    left_join(merge_NZA)
  df <- df %>%
    select(-NZA_DIAGNOSE, -NZA_HOOFDGROEP, -doelgroepNZa)
  
  assertthat::assert_that(nrow(df) == old_n)
  
  return(df)
}


assign_numeric_urgency <- function(data) {
  ## Function to change string version of urgency code to numerics
  #' @param data Dataframe with activities and string urgency codes
  #' @return Dataframe including a `urgency_num` variable for numeric urgency
  #'
  
  data$urgency_num <- 0
  data[urgency == "A:<24h", urgency_num := 1]
  data[urgency == "B:<1w", urgency_num := 2]
  data[urgency == "C:<2w", urgency_num := 3]
  data[urgency == "D:<1m", urgency_num := 4]
  data[urgency == "E:<2m", urgency_num := 5]
  data[urgency == "F:<3m", urgency_num := 6]
  data[urgency == "G:>3m", urgency_num := 7]
  return(data)
}


#' Generates count data by age and sex
#'
#' This function takes a dataset as an input, and returns a dataset with the number of individuals for each age and sex.
#' @param data Raw data
#' @param year Optional. If specified, only computes counts for year.
#' @return dataset with columns year, age, female, and n
gen_age_sex_counts <- function(data, year = NULL) {
  
  dt <- data %>%
    as.data.table()
  
  # filter to year 
  if (!is.null(year)) {
    dt <- dt[year == year, ]
  }
  
  # generate counts for each age and sex 
  setkey(dt, year, age, female)
  count_dt <- dt[CJ(year, age, female, unique = TRUE), .(n = .N), by = .EACHI]
  count_dt <- na.omit(count_dt)
  return(count_dt)
} 

#' Generates count data by age and sex
#'
#' This function takes a dataset as an input, and returns a dataset with the number of individuals for each age and sex.
#' @param data Raw data
#' @param year Optional. If specified, only computes counts for year.
#' @return dataset with columns year, age, female, and n
gen_age_sex_var_counts <- function(data, var_group = NULL, year = NULL) {
  
  dt <- data %>%
    as.data.table()
  
  # filter to year 
  if (!is.null(year)) {
    dt <- dt[year == year, ]
  }
  
  # generate counts for each age and sex 
  
  if (is.null(var_group)) {
    setkey(dt, year, age, female)
    count_dt <- dt[CJ(year, age, female, unique = TRUE), .(n = .N), by = .EACHI]
    count_dt <- na.omit(count_dt)  
    } else if (var_group == "female") {
    setkey(dt, year, age, female)
    count_dt <- dt[CJ(year, age, female, unique = TRUE), .(n = .N), by = .EACHI]
    count_dt <- na.omit(count_dt)  
    } else {
    setnames(dt, var_group, "var_group")
    setkey(dt, year, age, female, var_group)
    count_dt <- dt[CJ(year, age, female, var_group, unique = TRUE), .(n = .N), by = .EACHI]
    count_dt <- na.omit(count_dt)
    setnames(count_dt, "var_group", var_group)
  }
  
  return(count_dt)
} 


#' Function to call the weekly count functionality and save counts
#'
#' @param data Activity data
#' @param dem Demographic data
#' @param var_group Demographic group
#' @param activity_sel Activity type
#' @param medical_sel Medicaly type
#' @param urgency_sel  Urgency type
#' @param output_folder Output folder
#' @param scalar_year Year to scale to
#' @param exclude_covid Boolean whether to exclude covid
#' @param sub_folder Sub folder in output folder
write_weekly_count <- function(data,
                               dem,
                               var_group = "all",
                               activity_sel = "all",
                               medical_sel = "all",
                               urgency_sel = "all",
                               output_folder = "H:/health_access/data/final_v3/",
                               scalar_year=2017, 
                               exclude_covid = TRUE,
                               sub_folder = "sample") {
  
  output_folder <- paste0(output_folder, sub_folder, "/series/")
  
  filename = paste0(paste0(
    c(var_group, activity_sel, medical_sel, urgency_sel), collapse = "_"),
    "_v3.rds")
  
  if (var_group == "all") {
    # TOTAL COUNT
    # generate total count by week
    data_final <- weekly_procedures_scaled(data,
                                           dem,
                                           scalar_year = scalar_year,
                                           var_group = NULL,
                                           exclude_covid = exclude_covid)
    
    # add variable names
    data_final <- data_final %>%
      mutate(var_name = 'Total',
             var_group = 'Total')
    
    data_final$week_date <- as.Date(paste(data_final$year,
                                          data_final$week,
                                          01,
                                          sep = "-"), format = "%Y-%U-%u")
    data_final_list <- data_final
    
  } else {
    data_final <- weekly_procedures_scaled(data, 
                                           dem, 
                                           scalar_year = scalar_year,
                                           var_group = var_group, 
                                           exclude_covid = exclude_covid)
    
    # change var_name for sex
    if (var_group == "female") {
      data_final <- data_final %>%
        mutate(var_name = ifelse(var_name == TRUE, "Female", "Male"))
    }
    if (var_group == "poverty") {
      data_final <- data_final %>%
        mutate(var_name = ifelse(var_name == TRUE, "Below poverty line", "Above poverty line"))
    }
    
    data_final$week_date <- as.Date(paste(data_final$year, 
                                          data_final$week, 
                                          01,
                                          sep = "-"), format = "%Y-%U-%u")
    
    var_names <- unique(data_final$var_name)
    
    data_final_list <- lapply(var_names, function(x) {
      data_final[data_final$var_name == x, ]
    })
    names(data_final_list) <- var_names
  }
  # write series
  print(paste0("Writing: ", filename))
  saveRDS(data_final_list, paste0(output_folder, filename))
}


#' Function to bulk apply `write_weekly_count`
#'
#' @param data Activity data
#' @param dem Demographic data
#' @param var_group Demographic group
#' @param activity_sel Activity type
#' @param medical_sel Medicaly type
#' @param urgency_sel  Urgency type
#' @param sub_folder Sub folder in output folder

apply_count <- function(data, dem, var_group = "all", urgency_sel = "all",
                        medical_sel = "all", activity_sel = "all",
                        sub_folder = "full") {
  print(var_group)
  print(Sys.time())
  t1 <- Sys.time()
  write_weekly_count(data, dem,
                     sub_folder = sub_folder,
                     var_group = var_group,
                     activity_sel = activity_sel,
                     medical_sel = medical_sel,
                     urgency_sel = urgency_sel)
  print(paste0("Generating the weekly count took: ", round((Sys.time() - t1) / 60, 2), " minutes"))
}

#' Generates counts per week
#'
#' This function takes raw procedure data as input and returns the weekly count
#' by age and sex. If var_group = T, returns weekly count by age, sex, and unique(var_group).
#' @param data Raw procedures, including var_group of the person receiving the procedure and procedure_type
#' @param var_group An optional variable by which to also generate weekly procedure counts. 
#' @param urgency_num An optional variable by which to also subset the procedures by urgency type.
#' @param exclude_covid A boolean that can be used to exclude activities part of a procedure that included a covid-19 hospital visit
#' @return dataset with columns year, week, age, female, n, procedure_group (optional), var_name (optional) and group (optional)
weekly_procedures <- function(data, 
                              var_group = NULL, 
                              urgency_num = NULL,
                              exclude_covid = FALSE){
  
  dt <- data %>%
    as.data.table()
  
  # subset by non-covid only
  if (exclude_covid) {
    dt <- dt[!dt$covid_activity_dbc, ]
  }
  
  # subset by urgency_num only
  if(!is.null(urgency_num)) {
    dt <- dt[urgency_num %in% urgency_num, ]
  }
  
  # also group by var_group
  if(is.null(var_group)) {
    # generate counts by year, week, age, and sex
    setkey(dt, year, week, age, female)
    count_data <- dt[CJ(year, week, age, female, unique = TRUE),
                     .(n = .N,
                       n_person = length(na.omit(unique(rinpersoon)))), 
                     by = .EACHI]
  } else if (var_group == "all") {
    # generate counts by year, week, age, and sex
    setkey(dt, year, week, age, female)
    count_data <- dt[CJ(year, week, age, female, unique = TRUE),
                     .(n = .N,
                       n_person = length(na.omit(unique(rinpersoon)))), 
                     by = .EACHI]
  } else {
    dt[, var_name := dt[[var_group]]]
    
    # generate counts by year, week, age, sex, and var group
    setkey(dt, year, week, age, female, var_name)
    count_data <- dt[CJ(year, week, age, female, var_name, unique = TRUE),
                     .(n = .N,
                       n_person = length(na.omit(unique(rinpersoon)))), 
                     by = .EACHI]
    count_data[, var_group := var_group]    
  }
  
  return(count_data)
}


#' Generates weekly rate and scaled n
#' 
#' This function takes procedure data and population data, computes the rate of procedures per week
#' As well as the number of procedures scaled to a specific constant year. 
#' @param data_procedures a dataset listing all unique procedures
#' @param dem population data
#' @param scalar_year the year to which to scale the data to
#' @param var_group an optional grouping varable, for which to also compute the weekly number and rate.
#' @param urgency_num An optional variable by which to also subset the procedures by urgency type.
#' @returns Dataset including year, week, age, female, n, n_s (scaled n), rate, n_year and n_benchmark. Optionally also includes var_name and var_group
weekly_procedures_scaled <- function(data, 
                                     dem,
                                     scalar_year = 2017,
                                     var_group = NULL, 
                                     urgency_num = NULL, 
                                     exclude_covid = FALSE) {
  
  # generate number of weekly procedures
  data <- weekly_procedures(data, 
                            var_group, 
                            urgency_num, 
                            exclude_covid)
  
  data_year <- gen_age_sex_var_counts(dem, var_group)

  # specify scalar year to benchmark counts to
  data_scale <- data_year %>% 
    filter(year == scalar_year) %>%
    group_by(age, female) %>%
    summarise(n_benchmark = sum(n)) %>%
    ungroup() %>%
    as.data.table()
  
  data_year <- data_year %>%
    rename(n_year = n)
  
  if(is.null(var_group)) {
    # generate constant age population counts 
    # Merge in the year-specific count
    setkey(data, age, female, year)
    setkey(data_year, age, female, year)
    
    data <- data_year[data]  
  } else if(var_group == "all") {
    # generate constant age population counts 
    # Merge in the year-specific count
    setkey(data, age, female, year)
    setkey(data_year, age, female, year)
    
    data <- data_year[data]  
  } else if(var_group == "female") {
    # generate constant age population counts 
    # Merge in the year-specific count
    setnames(data_year, "female", "var_name")
    
    setkey(data, age, var_name, year)
    setkey(data_year, age, var_name, year)
    
    data <- data_year[data]  
  }else {
    setnames(data_year, var_group, "var_name")
    
    # generate constant age population counts 
    # Merge in the year-specific count
    setkey(data, age, female, year, var_name)
    setkey(data_year, age, female, year, var_name)
    
    data <- data_year[data]  
  }
  
  
  # Merge in the benchmark year count
  setkey(data, age, female)
  setkey(data_scale, age, female)
  
  data <- data_scale[data]
  data[is.na(n_benchmark), n_benchmark := 0]

  # generate scaled counts
  data[, n_s := (n / n_year) * n_benchmark]
  data[is.na(n_s), n_s := 0]
  data[, n_person_s := (n_person / n_year) * n_benchmark]
  data[is.na(n_person_s), n_person_s := 0]
  
  # data[data$n_s==Inf, ]
    
  # aggregate data to weekly counts
  if (!is.null(var_group)) {
    setkey(data, year, week, var_name)  
  } else {
    setkey(data, week, year)  
  }
  
  agg_data <- data[, list(n = sum(n), 
                          n_s = sum(n_s),
                          n_person = sum(n_person), 
                          n_person_s = sum(n_person_s)),
                          by = key(data)]
  
  return(agg_data)
}


## Function to make a weekly dataframe with the number of holiday days
#' @param start_year start year for holiday data
#' @param end_year end year for holiday data
#' @return datatable with weeks for rows and number of holiday days as columns
gen_holidays_weekly <- function(start_year=NULL, end_year=NULL) {
  
  ## Get public holiday data
  holidays <- as.Date(c(
    '2017-01-01',  ## New Year's Day
    '2017-04-16',  ## Easter
    '2017-04-17',  ## Easter
    '2017-04-27',  ## King's day
    '2017-06-04',  ## Whitsun
    '2017-06-05',  ## Whitsun
    '2017-12-25',  ## Christmas
    '2017-12-26',  ## Christmas
    '2018-01-01',  ## New Year's Day
    '2018-04-01',  ## Easter
    '2018-04-02',  ## Easter
    '2018-04-27',  ## King's day
    '2018-05-20',  ## Whitsun
    '2018-05-21',  ## Whitsun
    '2018-12-25',  ## Christmas
    '2018-12-26',  ## Christmas
    '2019-01-01',  ## New Year's Day
    '2019-04-21',  ## Easter
    '2019-04-22',  ## Easter
    '2019-04-27',  ## King's day
    '2019-06-09',  ## Whitsun
    '2019-06-10',  ## Whitsun
    '2019-12-25',  ## Christmas
    '2019-12-26',  ## Christmas
    '2020-01-01',  ## New Year's Day
    '2020-04-12',  ## Easter
    '2020-04-13',  ## Easter
    '2020-04-27',  ## King's day
    '2020-05-31',  ## Whitsun
    '2020-06-01',  ## Whitsun
    '2020-12-25',  ## Christmas
    '2020-12-26',  ## Christmas
    '2021-01-01',  ## New Year's Day
    '2021-04-04',  ## Easter
    '2021-04-05',  ## Easter
    '2021-04-27',  ## King's day
    '2021-05-23',  ## Whitsun
    '2021-05-24',  ## Whitsun
    '2021-12-25',  ## Christmas
    '2021-12-26'  ## Christmas
    ))
  
  if(is.null(start_year)) {
    start_year <- min(lubridate::year(holidays))
  }
  if(is.null(end_year)) {
    end_year <- max(lubridate::year(holidays))
  }
  
  dates <- data.table(date = seq(as.Date(paste0(start_year, '-01-01')),
                                 as.Date(paste0(end_year, '-12-31')), 
                                 by = '1 day'))
  
  dates[, week := lubridate::week(date)]
  dates[, year := lubridate::year(date)]
  dates[, holiday := 0]
  dates[date %in% holidays, holiday := 1]
  
  holidays_weekly <- dates[, 
                           .(holidays = sum(holiday),
                             ndays = length(unique(date))),
                           by = list(week, year)]
  return(holidays_weekly)
}


#' Function to fetch data and make predictions
#'
#' @param name Name of file
#' @param linear Boolean whether to assume linear year tred
#' @param treat_year Treatment start
#' @param controls Control variables
#' @param interacted Whether to do interacted analysis
#' @param nb Whether to use a negative binomial regression
#' @param version Version
#' @param dir Data directory
fetch_data_plots <- function(name, 
                             linear = F, 
                             treat_year = 2020,
                             var_groups = c(
                               'total',
                               'female',
                               'age_group',
                               'background_group',
                               'poverty'),
                             controls = c(
                               "as.factor(week)",
                               "holidays",
                               "ndays",
                               "year_lin"),
                             interacted = F,
                             nb = F,
                             version = "",
                             dir = file.path("data", "timeseries")) {
  
  data_file <- paste0(name, version, ".xlsx")
  
  data <- readxl::read_xlsx(file.path(dir, data_file)) %>%
    as.data.table()
  
  ## Order urgency and omit "none" urgency
  data$urgency_type <- factor(data$urgency_type, levels = c("high", "rest", "low", "none", "all"))
  
  # data <- data[data$year <= treat_year, ]
  
  data_inc_pred <- bind_rows(lapply(unique(data$urgency_type), function(u) {
    temp <- data[data$urgency_type == u, ]
    temp %>%
      include_predictions(
        linear = linear, treat_year = treat_year,
        dvs = c("n_s", "n_person_s", "n_person"),
        controls = controls,
        nb = nb
      ) %>%
      return()
  }))

  # remove all but relevant var_groups
  data_inc_pred <- data_inc_pred %>%
    filter(var_group %in% var_groups)
  
  # change week date to date class
  data_inc_pred <- data_inc_pred %>%
    mutate(week_date = as.Date(week_date))
  
  return(data_inc_pred)
}


#' Function to include predictions
#'
#' @param data Weekly timeseries data
#' @param linear Boolean whether to assume linear year tred
#' @param treat_year Treatment start
#' @param controls Control variables
#' @param nb Whether to use a negative binomial regression
include_predictions <- function(data,
                                linear = TRUE,
                                treat_year = treat_year,
                                dvs = dvs,
                                controls = c("as.factor(week)", 
                                             "holidays", 
                                             "ndays", 
                                             "year_lin"),
                                nb = F) {
  ## Add predictions
  # add separate predictions by unique(var_name)
  data <- as.data.table(data)
  
  if (is.null(data$var_name)) {
    
    temp <- add_predicted_rate2(
      data,
      treat_year,
      dvs,
      controls,
      linear,
      nb)
    
  } else {
    dt_list <- lapply(unique(data$var_name),
                      FUN = function(x) {
                        sub <- data[var_name == x, ]
                        sub[, var_name := x]
                        # sub[, var_group := var]
                        return(sub)
                      })  
    
    temp <- bind_rows(lapply(dt_list, 
                             add_predicted_rate2, 
                             treat_year = treat_year,
                             dvs = dvs,
                             controls = controls,
                             linear = linear,
                             nb = nb))
  }
  
  return(temp)
}


#' Function to include predictions
#'
#' @param data Weekly timeseries data
#' @param linear Boolean whether to assume linear year tred
#' @param treat_year Treatment start
#' @param controls Control variables
#' @param nb Whether to use a negative binomial regression
add_predicted_rate2 <- function(data, 
                               treat_year,
                               dvs,
                               controls = c("as.factor(week)", 
                                            "holidays", 
                                            "ndays",
                                            "year_lin"),
                               linear = T,
                               nb = F) {
  
  # add public holidays
  holidays <- gen_holidays_weekly()
  setkey(data, week, year)  
  setkey(holidays, week, year)
  data <- holidays[data]
  
  # add linear time trend
  data[, year_lin := year - 2016]
  
  
  for (y in dvs) {
    data[, year_treat := year]
    data[year > treat_year, year_treat := treat_year]

    if (linear) {
      
      model <- paste(y, "~", paste(controls, collapse = " + "))
      if (nb) {
        m_weekly <- MASS::glm.nb(formula(model), data = data[year < treat_year, ])
      } else {
        m_weekly <- lm(formula(model),
                     data = data[year , treat_year, ])    
      }
      
    } else {
      controls <- gsub("year_lin", "as.factor(year_treat)", controls)
      model <- paste(y, "~", paste(controls, collapse = " + "))
      data[, treat := "Control"]
      data[(data$year == treat_year & data$week > 8 |
                         data$year > treat_year), treat := "Treat"]
      if (nb) {
        m_weekly <- MASS::glm.nb(formula(model),
                           data = data[data$treat == "Control", ])  
      } else {
        m_weekly <- lm(formula(model),
                     data = data[data$treat ==  "Control", ])  
      }
      
    }
    
    
    var_pred <- paste0(y, "_pred")
    var_lwr <- paste0(y, "_lwr")
    var_upr <- paste0(y, "_upr")
    
    
    if (nb) {
      data[, var_pred] <- predict(m_weekly, newdata = data, type = "response")
      ## Omit prediction interval for negative binomial
      data[, var_lwr] <- predict(m_weekly, newdata = data)
      data[, var_upr] <- predict(m_weekly, newdata = data)
    } else {
      data[, var_pred] <- predict(m_weekly, newdata = data)
      data[, var_lwr] <- predict(m_weekly, newdata = data,
                               interval = 'predict')[, 'lwr']
      data[, var_upr] <- predict(m_weekly, newdata = data,
                                interval = 'predict')[, 'upr']
    }
    
  }
  
  return(data)
}


## Function to generate covariates for regression
#' @param data Data table containing weekly observed counts
#' @return Data table including code
fetch_data <- function(years=2017:2020,
                       var_group = NULL,
                       var_sel = NULL,
                       urgency_type = NULL,
                       activity_type = NULL,
                       medical_type = NULL,
                       sub_dir = "sample",
                       version = "") {
  ## Function to collect weekly counts
  #' @param years Year to collect
  #' @param var_group The var_group to include in the count fetch
  #' @param var_sel List of var_names to subset the var_group by
  #' @param urgency_type Urgency types to subset
  #' @param activity_type Activity types to subset
  #' @param medical_type Medical types to subset
  #' @return Weekly counts of 'n', 'n_s', 'n_person', 'n_person_s'
  
  if (is.null(var_group)) {
    var_group_fetch <- "all"
  } else {
    var_group_fetch <- var_group
  }
  
  if (is.null(urgency_type)) {
    urgency_fetch <- "all"
  } else {
    urgency_fetch <- urgency_type
  }
  
  if (is.null(activity_type)) {
    activity_fetch <- "all"
  } else {
    activity_fetch <- activity_type
  }
  
  if (is.null(medical_type)) {
    medical_fetch <- "all"
  } else {
    medical_fetch <- medical_type
  }
  
  act_med_fetch <- do.call(
    paste0, expand.grid(activity_fetch, "_", medical_fetch)
  )
  act_med_urg_fetch <- do.call(
    paste0, expand.grid(act_med_fetch, "_", urgency_fetch)
  )
  var_urg_act_med_fetch <- do.call(
    paste0, expand.grid(var_group_fetch, "_", act_med_urg_fetch)
  )
  
  rds_fetch <- paste0("./data/final", version, "/", sub_dir, "/series/",
                      var_urg_act_med_fetch, version,
                      ".rds")
  
  if (is.null(var_sel)) {
    if (length(rds_fetch) == 1) {
      comb_series <- bind_rows(read_rds(rds_fetch))
    } else {
      comb_series <- bind_rows(lapply(rds_fetch, function(y) {
        temp <- read_rds(y)
        temp <- bind_rows(temp)
        return(temp)
      }))  
    }
  } else {
    comb_series <- bind_rows(lapply(rds_fetch, function(y) {
      temp <- read_rds(y)
      return(temp[temp$var_name %in% var_sel])
    }))
  }
  
  if (is.null(var_group)) {
    final <- comb_series %>%
      select(-any_of(c("var_name", "var_group"))) %>%
      group_by(year, week, week_date) %>%
      summarise_all(.funs = sum)  
  } else {
    final <- comb_series %>%
      group_by(var_name, year, week, week_date) %>%
      summarise_all(.funs = sum)  
  }
  
  return(final)
}d covariates
make_covs <- function(data) {
  data$age <- as.factor(gsub("_.*", "" , data$var_name))
  data$poverty <- as.factor(gsub(".*_", "" , data$var_name))
  data$female <- as.factor(ifelse(grepl("Female", data$var_name), "Female",
                        "Male"))
  data$native <- as.factor(ifelse(grepl("Non-Dutch", data$var_name),
                        "Non-Dutch", "Dutch"))
  
  data$age <- relevel(data$age, ref = "76+")
  data$female <- relevel(data$female, ref = "Male")
  
  ## Code covid wave periods
  # Categorical
  data$time_periods <- case_when(
    data$year == "2020" & between(data$week, 12, 22) ~ "wave_1",
    data$year == "2020" & between(data$week, 23, 39) ~ "interwave_1",
    (data$year == "2020" & data$week > 39) |
      (data$year == "2021" & data$week < 26) ~ "wave_2",
    data$year == "2021" & between(data$week, 26, 41) ~ "interwave_2",
    data$year == "2021" & data$week > 41 ~ "wave_3",
    TRUE ~ "pre-covid"
  )
  
  # Dummy-encoded
  data$wave_1 <- data$time_periods == "wave_1"
  data$wave_2 <- data$time_periods == "wave_2"
  data$wave_3 <- data$time_periods == "wave_3"
  data$interwave_1 <- data$time_periods == "interwave_1"
  data$interwave_2 <- data$time_periods == "interwave_2"
  data$pandemic <- data$time_periods != "pre-covid"

  data$year_fe <- as.character(data$year)
  data$year_fe[data$year >= 2020] <- "2020-2021"
  return(data)
}

# --
# Functions
# ---

library(tidyverse)
library(lubridate)
library(broom)
library(ggrepel)
library(data.table)

## 01_gen_data

#' Function to merge in dbc groups
#'
#' @param df Dataframe with stacked activity data
#' @param dbc_d_classification Crosswalk table from DBC to NZa group
#' @param dbc_h_classification Crosswalk table from NZa to DBC H group
#' @return Dataframe inlcuding DBC classification
merge_NZA_classification <- function(df, dbc_d_classification) {
  # remove NAs in procedures (6)
  old_n <- nrow(df)

  df <- df[!is.na(VEKTMSZSpecialismeDiagnoseCombin), ]

  print(paste("Dropped", (old_n - nrow(df)), "observations due to NA DBC"))


  df$VEKTMSZSpecialismeDiagnoseCombin

  # generate variables
  df[, NZA_SPEC_CODE := gsub("-.*", "",
                             VEKTMSZSpecialismeDiagnoseCombin)]
  df[, NZA_SPEC_CODE := as.numeric(NZA_SPEC_CODE)]
  df[, NZA_DIAGNOSE := gsub(".*-", "",
                            VEKTMSZSpecialismeDiagnoseCombin)]
  df[, NZA_HOOFDGROEP := gsub("\\d{4}-", "",
                              VEKTMSZSpecialismeDiagnoseCombin)]
  df[, NZA_HOOFDGROEP := gsub("-.*", "", NZA_HOOFDGROEP)]

  # Add a NZa hoofdgroep voor OZP

  dbc_d_classification <- bind_rows(
    dbc_d_classification,
    data.frame(
      "NZA_DIAGNOSE" = "9999",
      "NZA_HOOFDGROEP" = "99",
      "NZA_SPEC_CODE" = 9999,
      "doelgroepNZa" = 99,
      "hoofdgroep" = 99
    )
  )

  # unit tests
  assertthat::assert_that(mean(df$NZA_SPEC_CODE %in%
    dbc_d_classification$NZA_SPEC_CODE) > 0.99)
  assertthat::assert_that(all(mean(nchar(df$NZA_DIAGNOSE)) == 4))
  assertthat::assert_that(mean(df$NZA_DIAGNOSE %in%
                               dbc_d_classification$NZA_DIAGNOSE) > 0.99)
  assertthat::assert_that(
    all(mean(nchar(df$NZA_HOOFDGROEP)) == 2))
  assertthat::assert_that(
    mean(df$NZA_HOOFDGROEP %in%
         dbc_d_classification$NZA_HOOFDGROEP) > 0.99)

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

#' Function to assign numeric income to classes
#' and mutate rinpersoon to numeric
#'
#' @param data data including rinpersoon and numeric income, and year variable
#' @reutnr Dataset including numeric rinpersoon and income class
classify_income <- function(data) {
  
  yearly_quintiles <- quantile(data$lower_bound_num[data$lower_bound_num > 0],
                               c(0.2, 0.4, 0.6, 0.8))
  
  data$income_class <- case_when(
    data$lower_bound_num < 0 ~ "Other",
    data$lower_bound_num < yearly_quintiles[[1]] ~ "Q1",
    data$lower_bound_num < yearly_quintiles[[2]] ~ "Q2",
    data$lower_bound_num < yearly_quintiles[[3]] ~ "Q3",
    data$lower_bound_num < yearly_quintiles[[4]] ~ "Q4",
    data$lower_bound_num >= yearly_quintiles[[4]] ~ "Q5"
  )
  
  data$poverty <- (data$lower_bound_num > 0) &
    (data$lower_bound_num < 120)
  
  names(data) <- tolower(names(data))
  
  data <- data %>%
    mutate(rinpersoon = as.numeric(rinpersoon))
  
  return(data %>% select(rinpersoon, income_class, poverty, year))
}




#' Function to change string version of urgency code to numerics
#'
#' @param data Dataframe with activities and string urgency codes
#' @return Dataframe including a `urgency_num` variable for numeric urgency
assign_numeric_urgency <- function(data) {

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

write_weekly_count <- function(data,
                               dem,
                               var_group = "all",
                               activity_sel = "all",
                               medical_sel = "all",
                               urgency_sel = "all",
                               output_folder = "H:/health_access/data/final/",
                               scalar_year = 2017,
                               exclude_covid = TRUE,
                               sub_folder = "sample") {
  output_folder <- paste0(output_folder, sub_folder, "/series/")

  filename <- paste0(
    paste0(
      c(var_group, activity_sel, medical_sel, urgency_sel),
      collapse = "_"
    ),
    ".rds"
  )

  if (var_group == "all") {
    # TOTAL COUNT
    # generate total count by week
    data_final <- weekly_procedures_scaled(data,
      dem,
      scalar_year = scalar_year,
      var_group = NULL,
      exclude_covid = exclude_covid
    )

    # add variable names
    data_final <- data_final %>%
      mutate(
        var_name = "Total",
        var_group = "Total"
      )

    data_final$week_date <- as.Date(paste(data_final$year,
      data_final$week,
      01,
      sep = "-"
    ), format = "%Y-%U-%u")
    data_final_list <- data_final
  } else {
    data_final <- weekly_procedures_scaled(data,
      dem,
      scalar_year = scalar_year,
      var_group = var_group,
      exclude_covid = exclude_covid
    )

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
      sep = "-"
    ), format = "%Y-%U-%u")

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
    urgency_sel = urgency_sel
  )
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
                              exclude_covid = FALSE) {
  dt <- data %>%
    as.data.table()

  # subset by non-covid only
  if (exclude_covid) {
    dt <- dt[!dt$covid_activity_dbc, ]
  }

  # subset by urgency_num only
  if (!is.null(urgency_num)) {
    dt <- dt[urgency_num %in% urgency_num, ]
  }

  # also group by var_group
  if (is.null(var_group)) {
    # generate counts by year, week, age, and sex
    setkey(dt, year, week, age, female)
    count_data <- dt[CJ(year, week, age, female, unique = TRUE),
      .(
        n = .N,
        n_person = length(na.omit(unique(rinpersoon)))
      ),
      by = .EACHI
    ]
  } else if (var_group == "all") {
    # generate counts by year, week, age, and sex
    setkey(dt, year, week, age, female)
    count_data <- dt[CJ(year, week, age, female, unique = TRUE),
      .(
        n = .N,
        n_person = length(na.omit(unique(rinpersoon)))
      ),
      by = .EACHI
    ]
  } else {
    dt[, var_name := dt[[var_group]]]

    # generate counts by year, week, age, sex, and var group
    setkey(dt, year, week, age, female, var_name)
    count_data <- dt[CJ(year, week, age, female, var_name, unique = TRUE),
      .(
        n = .N,
        n_person = length(na.omit(unique(rinpersoon)))
      ),
      by = .EACHI
    ]
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
  data <- weekly_procedures(
    data,
    var_group,
    urgency_num,
    exclude_covid
  )

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

  if (is.null(var_group)) {
    # generate constant age population counts
    # Merge in the year-specific count
    setkey(data, age, female, year)
    setkey(data_year, age, female, year)

    data <- data_year[data]
  } else if (var_group == "all") {
    # generate constant age population counts
    # Merge in the year-specific count
    setkey(data, age, female, year)
    setkey(data_year, age, female, year)

    data <- data_year[data]
  } else if (var_group == "female") {
    # generate constant age population counts
    # Merge in the year-specific count
    setnames(data_year, "female", "var_name")

    setkey(data, age, var_name, year)
    setkey(data_year, age, var_name, year)

    data <- data_year[data]
  } else {
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

  agg_data <- data[, list(
    n = sum(n),
    n_s = sum(n_s),
    n_person = sum(n_person),
    n_person_s = sum(n_person_s)
  ),
  by = key(data)
  ]

  return(agg_data)
}


## Function to make a weekly dataframe with the number of holiday days
#' @param start_year start year for holiday data
#' @param end_year end year for holiday data
#' @return datatable with weeks for rows and number of holiday days as columns
gen_holidays_weekly <- function(start_year = NULL, end_year = NULL) {

  ## Get public holiday data
  holidays <- as.Date(c(
    "2017-01-01",
    "2017-04-14",
    "2017-04-16",
    "2017-04-17",
    "2017-04-27",
    "2017-05-25",
    "2017-06-04",
    "2017-06-05",
    "2017-12-25",
    "2017-12-26",
    "2018-01-01",
    "2018-04-01",
    "2018-04-02",
    "2018-04-27",
    "2018-05-10",
    "2018-05-20",
    "2018-05-21",
    "2018-12-25",
    "2018-12-26",
    "2019-01-01",
    "2019-04-19",
    "2019-04-21",
    "2019-04-22",
    "2019-04-27",
    "2019-05-30",
    "2019-06-09",
    "2019-06-10",
    "2019-12-25",
    "2019-12-26",
    "2020-01-01",
    "2020-04-10",
    "2020-04-12",
    "2020-04-13",
    "2020-04-27",
    "2020-05-21",
    "2020-05-31",
    "2020-06-01",
    "2020-12-25",
    "2020-12-26"
  ))

  if (is.null(start_year)) {
    start_year <- min(lubridate::year(holidays))
  }
  if (is.null(end_year)) {
    end_year <- max(lubridate::year(holidays))
  }

  dates <- data.table(date = seq(as.Date(paste0(start_year, "-01-01")),
    as.Date(paste0(end_year, "-12-31")),
    by = "1 day"
  ))

  dates[, week := lubridate::week(date)]
  dates[, year := lubridate::year(date)]
  dates[, holiday := 0]
  dates[date %in% holidays, holiday := 1]

  holidays_weekly <- dates[,
    .(
      holidays = sum(holiday),
      ndays = length(unique(date))
    ),
    by = list(week, year)
  ]
  return(holidays_weekly)
}




add_predicted_rate <- function(data,
                               treat_year,
                               dvs,
                               controls,
                               linear = T) {

  # add public holidays
  holidays <- gen_holidays_weekly()
  setkey(data, week, year)
  setkey(holidays, week, year)
  data <- holidays[data]

  # add linear time trend
  data[, year_lin := year - 2016]

  for (y in dvs) {
    if (linear) {
      model <- paste(y, "~", paste(controls, collapse = " + "))
      m_weekly <- lm(formula(model),
        data = data[year != treat_year, ]
      )
    } else {
      controls <- gsub("year_lin", "as.factor(year)", controls)
      model <- paste(y, "~", paste(controls, collapse = " + "))
      data$treat <- (data$year == treat_year) & (data$week > 8)
      m_weekly <- lm(formula(model),
        data = data[!treat, ]
      )
    }


    var_pred <- paste0(y, "_pred")
    var_lwr <- paste0(y, "_lwr")
    var_upr <- paste0(y, "_upr")

    data <- data %>%
      mutate(
        !!var_pred := predict(m_weekly, newdata = data),
        !!var_lwr := predict(m_weekly,
          newdata = data,
          interval = "predict"
        )[, "lwr"],
        !!var_upr := predict(m_weekly,
          newdata = data,
          interval = "predict"
        )[, "upr"]
      )
  }
  data[, treat := "Control"]
  data[year == treat_year, treat := "Treat"]

  return(data)
}


gen_plot_data <- function(data,
                          dem,
                          scalar_year,
                          treat_year,
                          urgency_num = NULL,
                          exclude_covid = FALSE,
                          dvs = c("n_s", "n_person_s"),
                          controls = c(
                            "as.factor(week)",
                            "holidays",
                            "ndays",
                            "year_lin"
                          ),
                          vars = "all",
                          linear = T) {

  # TOTAL COUNT
  # generate total count by week
  data_total <- weekly_procedures_scaled(data,
    dem,
    scalar_year = scalar_year,
    var_group = NULL,
    urgency_num = urgency_num,
    exclude_covid = exclude_covid
  )

  # add predictions
  data_total <- add_predicted_rate(data_total,
    treat_year = treat_year,
    dvs = dvs,
    controls = controls
  )

  # add variable names
  data_total <- data_total %>%
    mutate(
      var_name = "Total",
      var_group = "Total"
    )

  # GROUP COUNT
  if (vars == "all") {
    vars <- c("income_group", "background_group", "age_group", "female", "poverty")
  }

  for (var in vars) {
    # generate weekly count by var
    temp <- weekly_procedures_scaled(data,
      dem,
      scalar_year = scalar_year,
      var_group = var,
      urgency_num = urgency_num,
      exclude_covid = exclude_covid
    )

    # add separate predictions by unique(var_name)
    dt_list <- lapply(unique(temp$var_name),
      FUN = function(x) {
        sub <- temp[var_name == x, ]
        sub[, var_name := x]
        sub[, var_group := var]
        return(sub)
      }
    )

    temp <- bind_rows(lapply(dt_list,
      add_predicted_rate,
      treat_year = treat_year,
      dvs = dvs,
      controls = controls,
      linear = linear
    ))

    # change var_name for sex
    if (var == "female") {
      temp <- temp %>%
        mutate(var_name = ifelse(var_name == TRUE, "Female", "Male"))
    }
    if (var == "poverty") {
      temp <- temp %>%
        mutate(var_name = ifelse(var_name == TRUE, "Below poverty line", "Above poverty line"))
    }
    # combine with data
    data_total <- bind_rows(data_total, temp)
  }

  # generate week date variable
  data_total$week_date <- as.Date(paste(data_total$year,
    data_total$week,
    01,
    sep = "-"
  ), format = "%Y-%U-%u")
  return(data_total)
}


fetch_data <- function(years = 2017:2020,
                       var_group = NULL,
                       var_sel = NULL,
                       urgency_type = NULL,
                       activity_type = NULL,
                       medical_type = NULL,
                       sub_dir = "sample") {
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

  rds_fetch <- paste0(
    "./data/final/", sub_dir, "/series/", var_urg_act_med_fetch,
    ".rds"
  )

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
}


include_predictions <- function(data,
                                linear = TRUE,
                                treat_year = treat_year,
                                dvs = dvs,
                                controls = controls) {
  ## Add predictions
  # add separate predictions by unique(var_name)
  data <- as.data.table(data)

  if (is.null(data$var_name)) {
    temp <- add_predicted_rate2(
      data,
      treat_year,
      dvs,
      controls,
      linear
    )
  } else {
    dt_list <- lapply(unique(data$var_name),
      FUN = function(x) {
        sub <- data[var_name == x, ]
        sub[, var_name := x]
        # sub[, var_group := var]
        return(sub)
      }
    )

    temp <- bind_rows(lapply(dt_list,
      add_predicted_rate2,
      treat_year = treat_year,
      dvs = dvs,
      controls = controls,
      linear = linear
    ))
  }

  return(temp)
}


add_predicted_rate2 <- function(data,
                                treat_year,
                                dvs,
                                controls,
                                linear = T) {

  # add public holidays
  holidays <- gen_holidays_weekly()
  setkey(data, week, year)
  setkey(holidays, week, year)
  data <- holidays[data]

  # add linear time trend
  data[, year_lin := year - 2016]


  for (y in dvs) {
    if (linear) {
      model <- paste(y, "~", paste(controls, collapse = " + "))
      m_weekly <- lm(formula(model),
        data = data[year != treat_year, ]
      )
    } else {
      controls <- gsub("year_lin", "as.factor(year)", controls)
      model <- paste(y, "~", paste(controls, collapse = " + "))
      m_weekly <- lm(formula(model),
        data = data[!((data$year == treat_year) & (data$week > 8)), ]
      )
    }


    var_pred <- paste0(y, "_pred")
    var_lwr <- paste0(y, "_lwr")
    var_upr <- paste0(y, "_upr")
    var_se <- paste0(y, "_se")

    data[, var_se] <- predict(m_weekly, newdata = data, se.fit = T)$se.fit
    data[, var_pred] <- predict(m_weekly, newdata = data)
    data[, var_lwr] <- predict(m_weekly,
      newdata = data,
      interval = "predict"
    )[, "lwr"]
    data[, var_upr] <- predict(m_weekly,
      newdata = data,
      interval = "predict"
    )[, "upr"]
  }

  data[, treat := "Control"]
  data[year == treat_year, treat := "Treat"]

  return(data)
}

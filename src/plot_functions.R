# Script for plotting functions
# This theme extends the 'theme_minimal' that comes with ggplot2.
# The "Lato" font is used as the base font. This is similar
# to the original font in Cedric's work, Avenir Next Condensed.
theme_set(theme_minimal(base_family = "Lato"))

# color palette

color_palette <- c(
  "#88CCEE", "#CC6677", "#DDCC77",
  "#117733", "#332288", "#AA4499",
  "#44AA99", "#999933", "#882255",
  "#661100", "#6699CC", "#888888"
)

# update base theme
theme_update(
  # Remove title for x axes
  axis.title.x = element_blank(),
  # edit y axis title
  axis.title.y = element_text(
    size = 100,
    margin = margin(r = 5),
    color = "grey10",
    face = "bold"
  ),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 70, margin = margin(t = 5)),
  axis.text.y = element_text(size = 70, margin = margin(r = 5)),
  # Facet labels
  strip.text = element_text(color = "grey10", face = "bold"),
  strip.text.x = element_text(size = 70, margin = margin(b = 5)),
  strip.text.y = element_text(size = 70, margin = margin(l = 5)),
  panel.spacing = unit(2, "lines"),
  # Legend text
  legend.text = element_text(size = 70),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10",
    size = 28,
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = ggtext::element_markdown(
    color = "grey30",
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30",
    size = 13,
    lineheight = 1.2,
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)


# CUMULATIVE PLOT ---------------------------------------------------------

# special theme for cumulative plot
# Remove the grid lines that come with ggplot2 plots by default
theme_cum <- theme(panel.grid = element_blank(),
                   axis.text.x = element_text(size = 42.5, margin = margin(b = 5)))

# specify new geometry
plt_geometry <- function(data, x, y, x_min, x_max, y_min, y_max) {
  delta_y <- y_max - y_min
  step_y <- delta_y / 5
  step_y <- round(step_y, 2)
  
  plt <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    # add grid lines
    geom_vline(
      xintercept = seq(x_min, x_max, length.out = 5),
      color = "grey91",
      size = .5
    ) +
    # theme_void() +
    geom_segment(
      data = tibble(
        y = c(0:-5) * step_y,
        x1 = x_min,
        x2 = x_max
      ),
      aes(
        x = x1,
        xend = x2,
        y = y,
        yend = y
      ),
      inherit.aes = FALSE,
      color = "grey91",
      size = .5
    ) +
    geom_segment(
      data = tibble(y = 0, x1 = x_min, x2 = x_max),
      aes(
        x = x1,
        xend = x2,
        y = y,
        yend = y
      ),
      inherit.aes = FALSE,
      color = "grey60",
      size = .8
    ) +
    scale_y_continuous(
      breaks = c(0:-5) * step_y,
      labels = scales::label_percent(accuracy = 1)
    )
  return(plt)
}

# function to generate the cumulative plot
#' @param return_geom Boolean in case the geometry of the plot has to be re-used
#' @param custom_geom Custom geometry
gen_cum_plot <- function(data, y, treat_year = 2020,
                         remove_type = F, overwrite_geom = FALSE,
                         y_min = NULL, y_max = NULL
                         ) {
  if (y %in% c("n", "n_s")) {
    y_name <- "activities"
  } else {
    y_name <- "treated individuals"
  }
  
  # remove last two weeks, specify treatment year, arrange
  data <- data %>%
    filter(week < 52) %>%
    filter(year >= treat_year) %>%
    arrange(week, year, var_name, var_group)
  
  # calculate total predicted activites
  var_pred <- paste0(y, '_pred')
  data$y <- data[[y]]
  data$var_pred <- data[[var_pred]]
  data$total <- sum(data[[var_pred]])
  
  # generate variables and labels
  data <- data %>%
    arrange(year, week, var_name, var_group) %>%
    group_by(var_name, urgency_type) %>%
    mutate(
      week_date = as.Date(week_date),
      total = sum(var_pred),
      n_miss = cumsum(y - var_pred) / total,
      n_neg = ifelse(n_miss < 0, TRUE, FALSE),
      label = ifelse(week_date == max(week_date),
                     var_name_labels[var_name], NA),
      label_total = ifelse(week_date == min(week_date),
                           "Overall loss", NA)
    )
  
  # generate total loss
  n_miss_total <- data %>%
    filter(var_group %in% c("total", "Total")) %>%
    arrange(urgency_type, -week, -year) %>%
    group_by(urgency_type) %>%
    filter(row_number() == 1) %>%
    dplyr::select(urgency_type, n_miss) %>%
    rename(n_miss_total = n_miss)
  
  # remove "total" from var_group, add as vertical line instead
  data <- left_join(data, n_miss_total, by = "urgency_type") %>%
    filter(!(var_group %in% c("Total", "total")))
  
  # specify parameters for axes grids
  date_min <- first(as.Date(paste0(unique(data$year), "-01-01")))
  date_max <- last(as.Date(paste0(unique(data$year), "-12-31")))

  # generate plot geometry
  if (overwrite_geom) {
    plt <- plt_geometry(data, "week_date", "n_miss", date_min, date_max, y_min, y_max) 
  } else {
    y_min <- min(data$n_miss)
    y_max <- round(max(data$n_miss), 1)
    plt <-
      plt_geometry(data, "week_date", "n_miss", date_min, date_max, y_min, y_max)  
  }
  
  # add geometry
  plt <- plt +
    # vertical total line and text
    geom_line(
      aes(y = n_miss_total),
      linetype = "dotted",
      col = "grey40",
      size = 1.2
    ) +
    geom_text(
      aes(label = label_total,
          x = week_date,
          y = n_miss_total),
      family = "Lato",
      color = "grey40",
      vjust = -0.25,
      hjust = -0.05,
      size = 20
    ) +
    # cumlative change
    geom_line(aes(col = var_name),  size = 1.2) +
    geom_text_repel(
      aes(color = var_name, label = label),
      family = "Lato",
      fontface = "bold",
      direction = "y",
      xlim = c(as.Date(max(data$week_date) + 15), NA),
      hjust = 0,
      segment.size = .7,
      segment.alpha = .5,
      segment.linetype = "dotted",
      box.padding = .4,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      size = 20,
    ) +
    facet_grid(fct_rev(urgency_type) ~ var_group, labeller = as_labeller(facet_labels)) +
    # expand x axis
    scale_x_date(
      limits = c(date_min, date_max + 80),
      breaks = seq(date_min, date_max, length.out = 5),
      date_labels = "%b-%Y"
    ) +
    ylab(paste("Cum. change in", y_name)) +
    theme_cum +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    geom_rect(  ## Wave 1
      aes(xmin = start_w1, xmax = end_w1, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) +
    geom_rect(  ## Wave 2
      aes(xmin = start_w2, xmax = end_w2, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) +
    geom_rect(  ## Wave 3
      aes(xmin = start_w3, xmax = end_w3, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) 
    return(plt)
}


# Relative difference plot ------------------------------------------------
gen_rel_plot <- function(data, y) {
  
  if (y %in% c("n", "n_s")) {
    y_name <- "activities"
  } else {
    y_name <- "individuals"
  }
  
  # remove last two weeks, specify treatment year, arrange
  data <- data %>%
    filter(week < 52) %>%
    filter(year >= treat_year) %>%
    arrange(week, year, var_name, var_group)
  

  # calculate total predicted activites
  var_pred <- paste0(y, '_pred')
  data$y <- data[[y]]
  data$var_pred <- data[[var_pred]]
  data$total <- sum(data[[var_pred]])
  
  # generate variables and labels
  data_2020 <- data %>%
    filter(year == 2020) %>%
    group_by(var_name, urgency_type) %>%
    mutate(
      week_date = as.Date(week_date),
      total = sum(var_pred),
      n_miss = cumsum(y - var_pred) / total,
      n_neg = ifelse(n_miss < 0, TRUE, FALSE),
      label = ifelse(week_date == max(week_date),
                     var_name_labels[var_name], NA),
      label_total = ifelse(week_date == min(week_date),
                           "Overall loss", NA)
    )
  
  data_2021 <- data %>%
    # filter(year == 2021) %>%
    group_by(var_name, urgency_type) %>%
    mutate(
      week_date = as.Date(week_date),
      total = sum(var_pred),
      n_miss = cumsum(y - var_pred) / total,
      n_neg = ifelse(n_miss < 0, TRUE, FALSE),
      label = ifelse(week_date == max(week_date),
                     var_name_labels[var_name], NA),
      label_total = ifelse(week_date == min(week_date),
                           "Overall loss", NA)
    )
  
  # remove "total" from var_group, add as vertical line instead
  end_data_2020 <- data_2020[data_2020$week == 51 &
                               !(data_2020$var_group %in%
                               c("Total", "total")), ]
  end_data_2021 <- data_2021[data_2021$week == 51 &
                               !(data_2021$var_group %in%
                                   c("Total", "total")) &
                               data_2021$year == 2021, ]
  
  end_data <- rbind(end_data_2020, end_data_2021)
  
  reference_cats <- c("18-29", "cit_dutch", "Male", "Above poverty line")
  
  references <- end_data %>%
    filter(var_name %in% reference_cats) %>%
    dplyr::select(var_group, var_name, n_miss, urgency_type, year)
  
  assertthat::assert_that(
    !any(duplicated(references[, c("var_group", "urgency_type", "year")])))
  
  comparisons <- end_data %>%
    filter(!(var_name %in% reference_cats)) %>%
    dplyr::select(var_group, var_name, n_miss, urgency_type, label, year)
  
  comparisons <- comparisons %>%
    left_join(references, by = c("var_group", "urgency_type", "year"))
  comparisons$prop <- comparisons$n_miss.x / comparisons$n_miss.y
  comparisons <- comparisons %>%
    dplyr::select(var_group, var_name.x, urgency_type, prop, label, year) %>%
    rename(var_name = var_name.x)
  
  comparisons <- comparisons[comparisons$urgency_type != "none", ]
  comparisons$var_group_label <- case_when(
    comparisons$var_group == "background_group" ~ "Migrant background (Natives)",
    comparisons$var_group == "female" ~ "Sex (Male)",
    comparisons$var_group == "age_group" ~ "Age (18 to 29)",
    comparisons$var_group == "poverty" ~ "Poverty (Not poor)")
  
  custom_palette <- color_palette[c(2:4, 6, 8:9 )]
  
  comparisons$prop_label <- str_pad(round(comparisons$prop, 2 ),
                                    4, "right", "0")
  comparisons$prop_label[comparisons$prop_label == "1000"] <- "1.00"
  
  ggplot(comparisons, aes(x = prop, y = var_group_label, color = var_name,
                          shape = as.factor(year))) +
    geom_point(size = 5, position = position_dodge(width=0.7)) +
    geom_linerange(aes(xmin = 1, y = var_group_label, xmax = prop), size = 1.8, position = position_dodge(width=0.7)) +
    facet_grid(fct_rev(urgency_type) ~ year, labeller = as_labeller(facet_labels)) +
    ylab("") +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette) +
    geom_vline(xintercept = 1, linetype = "dashed", size = 0.8) +
    geom_text_repel(
      aes(color = var_name, label = prop_label, x = 1.35),
      position = position_dodge(width=0.7),
      direction = "y",
      # xlim = 1.4,
      # size = 5,
    ) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.title.x = element_text(size = 27, face = "bold")
    ) +
    # geom_text_repel(aes(color = var_name, label = round(prop, 2)),
    #                 position = position_dodge(width = 0.7),
    #           vjust=-1.3) +
    scale_x_continuous(
      # limits = c(0.7, 1.625),
                       name = "Ratio of cumulative loss at the end of period relative to benchmark group")
}


# Prediction plot ---------------------------------------------------------

theme_pred <- theme(
  
)

# function to generate predicted plots
gen_pred_plot <- function(data, y, types = "all", total_only = F) {
  if (y %in% c("n", "n_s")) {
    y_name <- "activities"
  } else {
    y_name <- "individuals"
  }
  
  data <- data %>%
    filter(week < 52) %>%
    filter(urgency_type %in% types)
  
  var_pred <- paste0(y, '_pred')
  var_lwr <- paste0(y, '_lwr')
  var_upr <- paste0(y, '_upr')
  
  data[var_group %in% c("total", "Total"), var_group := "Total"]
  
  data <- data %>%
    group_by(var_name) %>%
    mutate(ymin = mean(eval(parse(text = var_pred))) / 2)
  
  p_total <- data %>%
    filter(var_group %in% c("total", "Total")) %>%
    # mutate(treat = ifelse((data$year == treat_year & data$week > 9) |
    #                         data$year >= treat_year, TRUE, FALSE)) %>%
    ggplot(
      aes(
        x = week_date,
        y = eval(parse(text = var_pred)),
        ymin = eval(parse(text = var_lwr)),
        ymax = eval(parse(text = var_upr)),
        fill = treat,
        color = treat,
        group = treat
      )
    ) +
    geom_vline(
      xintercept = as.Date("2020-03-15"),
      color = "grey40",
      linetype = "dotted",
      size = 0.9
    ) +
    geom_ribbon(alpha = 1 / 3,
                color = NA,
                size = 0.9) +
    geom_line(size = 1.2) +
    # facet_wrap(var_group ~ .) +
    geom_line(aes(y = eval(parse(text = y))), color = 'grey10', size = 1.2) +
    ylab(str_to_title(paste(y_name, "treated per week"))) +
    scale_x_date(date_breaks = "6 month", date_labels =  "%b-%y",
                 limits = c(as.Date("2017-01-01"), as.Date("2021-12-31")),
                 expand = c(0,0))  +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    geom_blank(aes(y=ymin))
  
  p <- data %>%
    filter(!(var_group %in% c("Total", "total"))) %>%
    filter(!var_name %in% c("30-65", "66-75")) %>%
    mutate(treat = ifelse((year == treat_year & week > 9) |
                           (year >= treat_year), TRUE, FALSE)) %>%
    filter(year >= treat_year) %>%
    ggplot(aes(
      x = week_date,
      y = eval(parse(text = var_pred)),
      ymin = eval(parse(text = var_lwr)),
      ymax = eval(parse(text = var_upr)),
      fill = treat,
      color = treat
    )) +
    geom_vline(
      xintercept = as.Date("2020-03-15"),
      color = "grey40",
      linetype = "dotted",
      size = 0.9
    ) +
    geom_ribbon(alpha = 1 / 3,
                color = NA,
                size = 0.9) +
    geom_line(aes(y = eval(parse(text = y))), color = 'grey10', size = 1.2) +
    geom_line(size = 1.2) +
    ylab(str_to_title(paste(y_name, "treated per week"))) +
    # scale_y_log10() +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b-%Y",
                 expand = c(0,0))  +
    facet_wrap(
       ~ var_name,
      scales = 'free_y',
      ncol = 2,
      labeller = labeller(
        var_name = var_name_labels,
        # var_group = NULL,
        .multi_line = FALSE
      )
    ) +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_y_continuous(labels = scales::comma) +
    geom_blank(aes(y=ymin))
  
  p <- p_total /
    p + plot_layout(heights = c(0.5, 1))
  
  if (total_only) {
    return(p_total)
  } else {
    return(p)  
  }
  
}



# Deviation plot ----------------------------------------------------------
theme_dev <- theme(
  axis.text.x = element_text(size = 70, margin = margin(t = 5)),
  axis.text.y = element_text(size = 70, margin = margin(r = 5)),
  legend.text = element_text(size = 70),
  axis.title.y = element_text(
    size = 100,
    margin = margin(r = 5),
    color = "grey10",
    face = "bold"
  ),
  legend.position = "top"
)
gen_dev_plot <- function(data, y, cdata, include_covid = T,
                         treat_year = 2020, moving_avg = T) {
  if (y %in% c("n", "n_s")) {
    y_name <- "activities"
  } else {
    y_name <- "treated individuals"
  }
  
  data[data$var_group %in% c("total", "Total"), var_group := "Total"]
  data <- data[urgency_type != "all", ]
  data <- data %>%
    filter(week < 52) %>%
    filter(var_group %in% c("total", "Total")) %>%
    filter(year >= treat_year)
  
  var_pred <- paste0(y, '_pred')
  
  data$y <- data[[y]]
  data$var_pred <- data[[var_pred]]
  
  data$urgency_type <- case_when(
    data$urgency_type == "high" ~ "High urgency",
    data$urgency_type == "rest" ~ "Middle urgency",
    data$urgency_type == "low" ~ "Low urgency",
    data$urgency_type == "none" ~ "No urgency"
  )
  
  ## Label once
  act_data <- data %>%
    mutate(n_miss = y - var_pred) %>% 
    group_by(urgency_type) %>% 
    mutate(label = ifelse(n_miss == min(n_miss),
                          as.character(urgency_type), NA))
  
  cdata <- cdata %>%
    mutate(n_miss = value,
           urgency_type = "Covid-19") %>%
    left_join(act_data[act_data$urgency_type == "High urgency", c("week", "week_date", "year")]) %>%
    filter(week < 52)
  
  if (include_covid) {
    comb_data <- bind_rows(act_data,
                           cdata)  
  } else {
    comb_data <- act_data
  }
  
  comb_data <- comb_data %>% 
    group_by(week_date) %>%
    mutate(total = sum(n_miss))
    
  comb_data$urgency_type <- factor(comb_data$urgency_type,
    levels = c(
      "High urgency", "Middle urgency",
      "Low urgency", "No urgency", "Covid-19"
    )
  )
  
  tot_data <- comb_data[comb_data$var_name == "Total", ]
  
  if (moving_avg) {
    comb_data <- comb_data %>% arrange(urgency_type, week_date)  
    comb_data <- comb_data %>%
      group_by(urgency_type) %>%
      mutate(
        prev_week = lag(n_miss, 1),
        next_week = lead(n_miss, 1),
        running_avg = (coalesce(prev_week, 0) + n_miss + coalesce(next_week, 0)) / 
                      (1 + (!is.na(prev_week)) + (!is.na(next_week)))
                      )
    comb_data$n_miss <- comb_data$running_avg

    tot_data <- tot_data %>% arrange(urgency_type, week_date)
    tot_data <- tot_data %>%
      group_by(urgency_type) %>%
      mutate(
        prev_week = lag(total, 1),
        next_week = lead(total, 1),
        running_avg = (coalesce(prev_week, 0) + total + coalesce(next_week, 0)) / 
                      (1 + (!is.na(prev_week)) + (!is.na(next_week)))
                      )
    tot_data$total <- tot_data$running_avg
  }
  

  p <- comb_data %>%
    ggplot(aes(x = week_date, y = n_miss, fill = urgency_type)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_rect(  ## Wave 1
      aes(xmin = start_w1, xmax = end_w1, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) +
    geom_rect(  ## Wave 2
      aes(xmin = start_w2, xmax = end_w2, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) +
    geom_rect(  ## Wave 3
      aes(xmin = start_w3, xmax = end_w3, ymin = -Inf, ymax = Inf),
      fill = "lightgray",
      alpha = 0.01,
      position = "identity"
    ) +
    geom_area(alpha = 0.7) +
    geom_line(data = tot_data, aes(y = total, linetype = var_name), color = "grey10") + 
    ylab(paste("Change in ", y_name)) +
    scale_x_date(date_breaks = "3 month", date_labels =  "%b-%Y")  +
    scale_color_manual(values = color_palette, breaks = c("High urgency", "Middle urgency", "Low urgency", "No urgency", "Covid-19"),
                       name = "") +
    scale_fill_manual(values = color_palette, breaks = c("High urgency", "Middle urgency", "Low urgency", "No urgency", "Covid-19"), name = "") + 
    scale_linetype_manual(values = c("dashed"), labels = c("Total difference", ""), name = "") +
    scale_y_continuous(labels = scales::comma) +
    theme_dev
  
  return(p)
}


#' Function to make a regression plot
#'
#' @param data Analysis dataset
#' @param scale Boolean whether to scale the effect by pre-pandemic levels
plot_reg <- function(data, model_1_int, model_2_int, scale = T) {
  
  theme_reg <- theme(
    axis.title.x = element_text(
      size = 25,
      margin = margin(r = 5),
      color = "grey10",
      face = "bold"
    ),
    # panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill=NA, size = 1),
    text = element_text(size = 15),
    axis.text.x = element_text(size = 15, margin = margin(t = 5)),
    axis.text.y = element_text(size = 15, margin = margin(r = 5)),
    axis.title.y = element_text(size = 25, margin = margin(r = 5)),
    # Facet labels
    strip.text = element_text(color = "grey10", face = "bold"),
    strip.text.x = element_text(size = 20, margin = margin(b = 5)),
    strip.text.y = element_text(size = 20, margin = margin(l = 5)),
    panel.spacing = unit(2, "lines"),
    # Legend text
    legend.text = element_text(size = 20),
    legend.title = element_blank())


  ## Interacted
  model1_int <- glmmTMB(model_1_int, data = data)
  model2_int <- glmmTMB(model_2_int, data = data)        
  
  # model1_int_nb <- glmmTMB(model_1_int, data = data, family = nbinom2)
  # model2_int_nb <- glmmTMB(model_2_int, data = data, family = nbinom2)        

  coefficients1 <- as.data.frame(summary(model1_int)$coefficients$cond) %>%
    rownames_to_column(var = "variable") %>%
    filter(grepl(":", variable))
  coefficients2 <- as.data.frame(summary(model2_int)$coefficients$cond) %>%
    rownames_to_column(var = "variable") %>%
    filter(grepl(":", variable))

  total_coefs <- bind_rows(coefficients1, coefficients2)

  coeff_df <- total_coefs %>%
    mutate(Treatment = gsub(".*:", "", variable),  # Extract treatment variable name from 'variable'
          Demographic = gsub(":.*", "", variable),
          Group = gsub("\\).*|.*\\(", "", variable)) %>% # Extract demographic variable name from 'variable'
    rename(SE = `Std. Error`)

  coeff_df$Group <- case_when(
    grepl("18", coeff_df$Demographic) ~ "Age: 18-29",
    grepl("30", coeff_df$Demographic) ~ "Age: 30-65",
    grepl("66", coeff_df$Demographic) ~ "Age: 66-75",
    grepl("76", coeff_df$Demographic) ~ "Age: 76+",
    TRUE ~ coeff_df$Group
  )
  
  ## Standardize by pre-treatment mean
  control_means <- bind_rows(lapply(c("age", "poverty", "native",
                          "female"), function(x) {
    var_name <- "merge"
    temp <- df %>%
      filter(treat == "Control") %>%
      group_by_at(x) %>%
      summarise(control_mean = mean(n_person_s)) %>%
      rename_with(~var_name, all_of(x))
    return(temp)}))
  
  coeff_df$Treatment_num <- case_when(
    coeff_df$Treatment == "wave_1TRUE" ~ 1,
    coeff_df$Treatment == "interwave_1TRUE" ~ 2,
    coeff_df$Treatment == "wave_2TRUE" ~ 3,
    coeff_df$Treatment == "interwave_2TRUE" ~ 4,
    coeff_df$Treatment == "wave_3TRUE" ~ 5,
    coeff_df$Treatment == "pandemicTRUE" ~ 0,
  )
  
  if (scale) {
    coeff_df <- coeff_df %>%
    mutate(merge = gsub(".*\\)", "", Demographic)) %>%
    left_join(control_means, by = "merge")
  
    coeff_df$Estimate <- coeff_df$Estimate / coeff_df$control_mean
    coeff_df$SE <- coeff_df$SE / coeff_df$control_mean  
  }
  
  coeff_df$Group <- case_when(
    coeff_df$Group == "poverty" ~ "Poverty",
    coeff_df$Group == "native" ~ "Migrant Background",
    coeff_df$Group == "female" ~ "Sex",
    TRUE ~ coeff_df$Group
  )
  
  coeff_df$group_label <- gsub(".*\\)", "", coeff_df$Demographic)
  coeff_df$group_label <- case_when(
    coeff_df$group_label == "Poor" ~ "Poverty (No poverty)",
    coeff_df$group_label == "Non-Dutch" ~ "Non-Dutch (Dutch)",
    coeff_df$group_label == "Female" ~ "Female (Male)",
    TRUE ~ coeff_df$group_label
  )
  p_non_age <- ggplot(coeff_df[!grepl("Age", coeff_df$Group), ],
                aes(x = Treatment_num, y = Estimate, ymin = Estimate - SE,
                    ymax = Estimate + SE, color = group_label)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2)

  p_age <- ggplot(coeff_df[grepl("Age", coeff_df$Group), ],
                  aes(x = Treatment_num, y = Estimate, ymin = Estimate - SE,
                      ymax = Estimate + SE, color = merge)) +
    geom_point() +
    geom_errorbar(width = 0.2)

  plots <- lapply(list(p_age, p_non_age), function(p) {
    p + geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0.5) +
      facet_grid(. ~ group_label) +
      labs(x = "Treatment", y = "Additional decline relative to reference\n(% of pre-pandemic levels)") +
      scale_x_continuous(
        breaks = 0:5,
        labels = c("Overall", "Wave 1", "Inter-\nwave 1", "Wave 2", "Inter\n-wave 2", "Wave 3")
      ) + 
      theme(text = element_text(size = 12)) +
      scale_y_continuous(labels = scales::percent) +
      xlab("Pandemic wave") +
      theme_reg})
    return(plots)
}

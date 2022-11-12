
get_main_graph_HC <- function(UI_ID,
                              the_data,
                              x_axis_field,
                              input) {
  log_action(action_category = "UI", action = "Getting main graph", reference = UI_ID)
  
  data_to_use <- get_filtered_and_sampled_data(the_data, x_axis_field, input)
  
  if (nrow(data_to_use$DATA) == 0) {
    # Too many filters applied - there is no
    return(NULL)
  } else {
    
    predicted_data_per_data_point <- get_predicted_data_per_data_point(data_to_use, x_axis_field)
    
    predicted_data <- get_predicted_data(data_to_use, x_axis_field)
    
    result <- highcharter::highchart() %>%
      highcharter::hc_add_series(
        data_to_use$DATA,
        type = "scatter",
        marker = list(enabled = TRUE),
        highcharter::hcaes(x = !!x_axis_field, y = DriversKilled),
        name = "Actual"
      ) %>%
      highcharter::hc_add_series(
        predicted_data_per_data_point,
        type = "scatter",
        marker = list(enabled = TRUE),
        highcharter::hcaes(x = x_axis_value, y = predicted_value),
        name = "Predicted"
      ) %>%
      highcharter::hc_add_series(
        predicted_data,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = predicted_mean),
        name = "Mean"
      ) %>%
      highcharter::hc_add_series(
        predicted_data,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = predicted_median),
        name = "Median"
      ) %>%
      highcharter::hc_xAxis(title = list(text = x_axis_field))
    
    for (area_range_index in 1:(length(PREDICTION_PERCENTILES) / 2)) {
      percentile_index_low <- area_range_index
      percentile_index_high <- length(PREDICTION_PERCENTILES) - area_range_index + 1
      
      percentile_name_low <- names(PREDICTION_PERCENTILES)[percentile_index_low]
      percentile_name_high <- names(PREDICTION_PERCENTILES)[percentile_index_high]
      
      result <- result %>%
        highcharter::hc_add_series(
          predicted_data,
          type = "arearange",
          marker = list(enabled = FALSE),
          highcharter::hcaes(x = x_axis_value,
                             low = !!rlang::sym(percentile_name_low),
                             high = !!rlang::sym(percentile_name_high)),
          name = paste0(percentile_name_low, "-", percentile_name_high),
          color = "blue",
          opacity = 0.3
        )
    }
      
    
    log_action(action_category = "UI", action = "Got main graph", reference = UI_ID)
    
    return(result)
  }
}



get_filtered_and_sampled_data <- function(the_data, x_axis_field, input) {
  field_names_to_filter <- names(the_data$DATA)
  field_names_to_filter <- field_names_to_filter[!(field_names_to_filter %in% c(x_axis_field, the_data$RESPONSE))]
  
  filtered_and_sampled_data <- the_data
  
  for (field_name_to_filter in field_names_to_filter) {
    the_field_class <- class(the_data$DATA[[field_name_to_filter]])
    field_UI_ID <- get_sub_ID(FIELD_FILTERS_UI_ID, field_name_to_filter)
    
    if (the_field_class %in% c("integer", "numeric")) {
      filtered_and_sampled_data$DATA <- filtered_and_sampled_data$DATA %>%
        dplyr::filter(!!rlang::sym(field_name_to_filter) >= input[[field_UI_ID]][1],
                      !!rlang::sym(field_name_to_filter) <= input[[field_UI_ID]][2])
    } else {
      filtered_and_sampled_data$DATA <- filtered_and_sampled_data$DATA %>%
        dplyr::filter(!!rlang::sym(field_name_to_filter) %in% input[[field_UI_ID]])
    }
  }
  
  filtered_and_sampled_data$DATA <- filtered_and_sampled_data$DATA %>%
    dplyr::sample_n(size = min(nrow(.), DATA_SAMPLE_SIZE))
  
  return(filtered_and_sampled_data)
}



get_predicted_data_per_data_point <- function(the_data,
                                              x_axis_field) {
  predicted_values <- the_data$PREDICT_FUNCTION(the_data$MODEL, the_data$DATA)
  
  predicted_values_DF <- data.frame(x_axis_value = the_data$DATA[[x_axis_field]],
                                    predicted_value = predicted_values)
  
  return(predicted_values_DF)
}



get_predicted_data <- function(the_data,
                               x_axis_field) {
  x_axis_prediction_values <- get_x_axis_data_prediction_values(the_data$DATA[[x_axis_field]])
  
  the_data_to_predict <- the_data$DATA
  predicted_data <- NULL
  for (x_axis_prediction_value in x_axis_prediction_values) {
    the_data_to_predict[[x_axis_field]] <- x_axis_prediction_value
    predicated_values <- the_data$PREDICT_FUNCTION(the_data$MODEL, the_data_to_predict)
    predicted_mean <- mean(predicated_values)
    predicted_median <- quantile(predicated_values, 0.5, names = FALSE)
    predicted_percentiles <- quantile(predicated_values, PREDICTION_PERCENTILES, names = FALSE)
    
    predicted_data_to_insert <- data.frame(x_axis_value = x_axis_prediction_value,
                                           predicted_mean = predicted_mean,
                                           predicted_median = predicted_median,
                                           t(predicted_percentiles))
    
    names(predicted_data_to_insert) <- c(names(predicted_data_to_insert)[1:3],
                                         names(PREDICTION_PERCENTILES))
    
    predicted_data <- rbind(predicted_data, predicted_data_to_insert)
  }
  
  return(predicted_data)
}



stack_the_percentiles <- function(percentiles) {
  percentiles_prior <- percentiles %>%
    dplyr::mutate(dummy = 0) %>%
    dplyr::select(dummy, dplyr::everything())
  
  percentiles_prior <- percentiles_prior[, -ncol(percentiles_prior)]
  
  percentiles_stacked <- percentiles - percentiles_prior
  
  return(percentiles_stacked)
}



get_x_axis_data_prediction_values <- function(x_axis_data) {
  the_min <- min(x_axis_data)
  the_max <- max(x_axis_data)
  the_range <- the_max - the_min
  the_range_units <- 10^(floor(log(the_range, base = 10)) - 1)
  
  the_min_floored <- floor(the_min / the_range_units) * the_range_units
  the_min_ceilinged <- ceiling(the_min / the_range_units) * the_range_units
  the_max_floored <- floor(the_max / the_range_units) * the_range_units
  the_max_ceilinged <- ceiling(the_max / the_range_units) * the_range_units
  
  prediction_values_count <- (the_max_floored - the_min_ceilinged) / the_range_units
  
  the_middle <- the_min_ceilinged + (0:prediction_values_count) * the_range_units
  
  if (the_min_floored == the_min_ceilinged) { the_start <- NULL } else { the_start <- the_min_floored }
  if (the_max_ceilinged == the_max_floored) { the_end <- NULL } else { the_end <- the_max_ceilinged }
  
  prediction_values <- c(the_start, the_middle, the_end)
  
  return(prediction_values)
}



get_data_min_and_max <- function(the_data) {
  the_min <- min(the_data)
  the_max <- max(the_data)
  the_range <- the_max - the_min
  the_range_units <- 10^(floor(log(the_range, base = 10)) - 1)
  
  the_min_floored <- floor(the_min / the_range_units) * the_range_units
  the_max_ceilinged <- ceiling(the_max / the_range_units) * the_range_units
  
  return(c(min = the_min_floored, max = the_max_ceilinged))
}



get_x_axis_field_choices <- function(the_data) {
  if (is.null(the_data)) {
    return(NULL)
  }
  
  data_names <- names(the_data$DATA)
  data_names <- data_names[which(data_names != the_data$RESPONSE)]
  return(data_names)
}


get_field_filter <- function(field_to_be_filtered,
                             UI_ID,
                             the_data) {
  
  the_field_data <- the_data[[field_to_be_filtered]]
  the_field_class <- class(the_field_data)
  sub_ID <- get_sub_ID(UI_ID, field_to_be_filtered)
  
  if (the_field_class %in% c("integer", "numeric")) {
    data_min_and_max <- get_data_min_and_max(the_field_data)
    
    shiny::sliderInput(inputId = sub_ID,
                       label = field_to_be_filtered,
                       min = data_min_and_max["min"],
                       max = data_min_and_max["max"],
                       value = data_min_and_max)
  } else {
    shiny::selectInput(inputId = sub_ID,
                       label = field_to_be_filtered,
                       choices = levels(the_field_data),
                       selected = levels(the_field_data),
                       multiple = TRUE)
  }
}



get_sub_ID <- function(UI_ID,
                       sub_ID) {
  paste0(UI_ID, "_", sub_ID)
}



get_field_filters <- function(UI_ID,
                              the_data,
                              x_axis_field_choices,
                              x_axis_field) {
  
  if (is.null(the_data) | is.null(x_axis_field_choices) | is.null(x_axis_field)) {
    return(NULL)
  }
  
  fields_to_be_filtered <- x_axis_field_choices[which(x_axis_field_choices != x_axis_field)]
  
  field_filters <- lapply(
    fields_to_be_filtered,
    get_field_filter,
    UI_ID,
    the_data$DATA
  )
  
  return(shiny::tagList(field_filters))
}
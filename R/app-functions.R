
get_main_graph_HC <- function(UI_ID,
                              the_data,
                              selected_x_axis_field,
                              input) {
  log_action(action_category = "UI", action = "Getting main graph", reference = UI_ID)
  
  selected_model_set <- input[[SELECTED_MODEL_SET_SI_ID]]
  selected_model <- input[[SELECTED_MODEL_SI_ID]]
  
  filtered_and_sampled_data <- filtered_data <- get_filtered_data(the_data, selected_x_axis_field, input,
                                                                  selected_model_set)
  
  filtered_and_sampled_data$DATA <- filtered_and_sampled_data$DATA |>
    dplyr::sample_n(size = min(nrow(filtered_and_sampled_data$DATA), DATA_SAMPLE_SIZE))
  
  if (nrow(filtered_and_sampled_data$DATA) == 0) {
    # Too many filters applied - there is no data left!
    return(NULL)
  } else {
    
    predicted_data_per_data_point <- get_predicted_data_per_data_point(filtered_and_sampled_data,
                                                                       selected_model_set,
                                                                       selected_model,
                                                                       selected_x_axis_field)
    
    predicted_data_statistics <- get_predicted_data_statistics(filtered_data,
                                                               selected_model_set,
                                                               selected_model,
                                                               selected_x_axis_field)
    
    actual_data_statistics <- get_actual_data_statistics(filtered_data,
                                                         selected_model_set,
                                                         selected_x_axis_field)
    
    result <- highcharter::highchart() |>
      highcharter::hc_add_series(
        filtered_and_sampled_data$DATA,
        type = "scatter",
        marker = list(enabled = TRUE),
        highcharter::hcaes(x = !!selected_x_axis_field, y = !!filtered_and_sampled_data$MODEL_SETS[[selected_model_set]]$RESPONSE),
        name = "Actual"
      ) |>
      highcharter::hc_add_series(
        actual_data_statistics,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = actual_mean),
        name = "Actual Mean",
        visible = FALSE
      ) |>
      highcharter::hc_add_series(
        actual_data_statistics,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = actual_median),
        name = "Actual Median",
        visible = FALSE
      ) |>
      highcharter::hc_add_series(
        predicted_data_per_data_point,
        type = "scatter",
        marker = list(enabled = TRUE),
        highcharter::hcaes(x = x_axis_value, y = predicted_value),
        name = "Predicted",
        visible = FALSE
      ) |>
      highcharter::hc_add_series(
        predicted_data_statistics,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = predicted_mean),
        name = "Predicted Mean",
        visible = FALSE
      ) |>
      highcharter::hc_add_series(
        predicted_data_statistics,
        type = "line",
        marker = list(enabled = FALSE),
        highcharter::hcaes(x = x_axis_value, y = predicted_median),
        name = "Predicted Median",
        visible = FALSE
      ) |>
      highcharter::hc_xAxis(title = list(text = selected_x_axis_field))
    
    for (actual_or_predicted in c("Actual", "Predicted")) {
      if (actual_or_predicted == "Actual") {
        percentile_data <- actual_data_statistics
        percentile_colour <- "green"
      } else {
        percentile_data <- predicted_data_statistics
        percentile_colour <- "blue"
      }
      
      for (area_range_index in 1:(length(PREDICTION_PERCENTILES) / 2)) {
        percentile_index_low <- area_range_index
        percentile_index_high <- length(PREDICTION_PERCENTILES) - area_range_index + 1
        
        percentile_name_low <- names(PREDICTION_PERCENTILES)[percentile_index_low]
        percentile_name_high <- names(PREDICTION_PERCENTILES)[percentile_index_high]
        
        result <- result |>
          highcharter::hc_add_series(
            percentile_data,
            type = "arearange",
            marker = list(enabled = FALSE),
            highcharter::hcaes(x = x_axis_value,
                               low = !!rlang::sym(percentile_name_low),
                               high = !!rlang::sym(percentile_name_high)),
            name = paste0(actual_or_predicted, " ", percentile_name_low, "-", percentile_name_high),
            color = percentile_colour,
            opacity = 0.3,
            visible = (actual_or_predicted == "Actual")
          )
      }
    }
    
      
    
    log_action(action_category = "UI", action = "Got main graph", reference = UI_ID)
    
    return(result)
  }
}



get_filtered_data <- function(the_data, selected_x_axis_field, input, selected_model_set) {
  
  field_names_to_filter <- names(the_data$DATA)
  
  fields_to_exclude_from_filter <- get_fields_to_exclude_from_filter(the_data, selected_x_axis_fieldselected_model_set)
  
  field_names_to_filter <- field_names_to_filter[!(field_names_to_filter %in% fields_to_exclude_from_filter)]
  
  filtered_data <- the_data
  
  for (field_name_to_filter in field_names_to_filter) {
    
    the_field_class <- class(the_data$DATA[[field_name_to_filter]])
    field_UI_ID <- get_sub_ID(FIELD_FILTERS_UI_ID, field_name_to_filter)
    
    if (the_field_class %in% c("integer", "numeric")) {
      filtered_data$DATA <- filtered_data$DATA |>
        dplyr::filter(!!rlang::sym(field_name_to_filter) >= input[[field_UI_ID]][1],
                      !!rlang::sym(field_name_to_filter) <= input[[field_UI_ID]][2])
    } else {
      filtered_data$DATA <- filtered_data$DATA |>
        dplyr::filter(!!rlang::sym(field_name_to_filter) %in% input[[field_UI_ID]])
    }
  }
  
  return(filtered_data)
}



get_predicted_data_per_data_point <- function(the_data,
                                              selected_model_set,
                                              selected_model,
                                              x_axis_field) {
  selected_model <- the_data$MODEL_SETS[[selected_model_set]]$MODELS[[selected_model]]
  
  predicted_values <- selected_model$PREDICT_FUNCTION(selected_model$MODEL, the_data$DATA)
  
  predicted_values_DF <- data.frame(x_axis_value = the_data$DATA[[x_axis_field]],
                                    predicted_value = predicted_values)
  
  return(predicted_values_DF)
}



get_predicted_data_statistics <- function(the_data,
                                          selected_model_set,
                                          selected_model,
                                          x_axis_field) {
  x_axis_prediction_values <- get_x_axis_data_values(the_data$DATA[[x_axis_field]])
  
  selected_model <- the_data$MODEL_SETS[[selected_model_set]]$MODELS[[selected_model]]
  
  the_data_to_predict <- the_data$DATA
  predicted_data <- NULL
  for (x_axis_prediction_value in x_axis_prediction_values) {
    the_data_to_predict[[x_axis_field]] <- x_axis_prediction_value
    predicated_values <- selected_model$PREDICT_FUNCTION(selected_model$MODEL, the_data_to_predict)
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

get_actual_data_statistics <- function(the_data,
                                       selected_model_set,
                                       selected_x_axis_field) {
  x_axis_values <- get_x_axis_data_values(the_data$DATA[[selected_x_axis_field]])
  
  # Order data by x axis field and add indexes to determine KNN
  the_data_to_examine <- the_data$DATA
  the_data_to_examine <- the_data_to_examine[order(the_data_to_examine[[selected_x_axis_field]]), ]
  the_data_to_examine$x_axis <- the_data_to_examine[[selected_x_axis_field]]
  
  unique_x_axis <- unique(the_data_to_examine$x_axis)
  
  the_data_to_examine <- the_data_to_examine |>
    dplyr::left_join(data.frame(x_axis = unique_x_axis, order_index = 1:length(unique_x_axis)), by = "x_axis") |>
    dplyr::select(-x_axis)
  
  actual_data_statistics <- NULL
  for (x_axis_value in x_axis_values) {
    # Get k-nearest neighbours
    
    order_index_lower <- max(1, length(which(unique_x_axis <= x_axis_value)))
    
    if (any(unique_x_axis == x_axis_value)) {
      order_index_upper <- order_index_lower
    } else if (all(x_axis_value > unique_x_axis)) {
      order_index_upper <- order_index_lower
    } else {
      order_index_upper <- order_index_lower + 1
    }
    
    order_index_KNN_lower <- max(1, order_index_lower - (ACTUAL_DATA_STATISTICS_KNN - 1))
    order_index_KNN_upper <- min(max(the_data_to_examine$order_index), order_index_upper + (ACTUAL_DATA_STATISTICS_KNN - 1))
    
    actual_values <- the_data_to_examine |>
      dplyr::filter(order_index >= order_index_KNN_lower,
                    order_index <= order_index_KNN_upper) |>
      dplyr::pull(the_data$MODEL_SETS[[selected_model_set]]$RESPONSE)
    
    actual_mean <- mean(actual_values)
    actual_median <- quantile(actual_values, 0.5, names = FALSE)
    actual_percentiles <- quantile(actual_values, PREDICTION_PERCENTILES, names = FALSE)
    
    actual_data_to_insert <- data.frame(x_axis_value = x_axis_value,
                                        actual_mean = actual_mean,
                                        actual_median = actual_median,
                                        t(actual_percentiles))
    
    names(actual_data_to_insert) <- c(names(actual_data_to_insert)[1:3],
                                      names(PREDICTION_PERCENTILES))
    
    actual_data_statistics <- rbind(actual_data_statistics, actual_data_to_insert)
  }
  
  return(actual_data_statistics)
}



stack_the_percentiles <- function(percentiles) {
  percentiles_prior <- percentiles |>
    dplyr::mutate(dummy = 0) |>
    dplyr::select(dummy, dplyr::everything())
  
  percentiles_prior <- percentiles_prior[, -ncol(percentiles_prior)]
  
  percentiles_stacked <- percentiles - percentiles_prior
  
  return(percentiles_stacked)
}



get_x_axis_data_values <- function(x_axis_data) {
  the_min <- min(x_axis_data)
  the_max <- max(x_axis_data)
  the_range <- the_max - the_min
  
  if (the_range > 0) {
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
  } else {
    prediction_values <- c(the_min)
  }
  
  return(prediction_values)
}



get_data_min_and_max <- function(the_data) {
  the_min <- min(the_data)
  the_max <- max(the_data)
  the_range <- the_max - the_min
  
  if (the_range > 0) {
    the_range_units <- 10^(floor(log(the_range, base = 10)) - 1)
    
    the_min_floored <- floor(the_min / the_range_units) * the_range_units
    the_max_ceilinged <- ceiling(the_max / the_range_units) * the_range_units
    
    data_min_and_max <- c(min = the_min_floored, max = the_max_ceilinged)
  } else {
    data_min_and_max <- c(min = the_min, max = the_max)
  }
  
  return(data_min_and_max)
}



get_x_axis_field_choices <- function(the_data,
                                     selected_model_set) {
  if (is.null(the_data)) {
    return(NULL)
  }
  
  data_names <- names(the_data$DATA)
  data_names <- data_names[which(data_names != the_data$MODEL_SETS[[selected_model_set]]$RESPONSE)]
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



get_fields_to_exclude_from_filter <- function(the_data,
                                              selected_x_axis_field,
                                              selected_model_set,
                                              do_exclude_selected_x_axis_field_from_filters = DO_EXCLUDE_SELECTED_X_AXIS_FIELD_FROM_FILTERS_DEFAULT,
                                              do_exclude_response_field_from_filters = DO_EXCLUDE_RESPONSE_FIELD_FROM_FILTERS_DEFAULT) {
  fields_to_exclude <- c()
  
  if (do_exclude_selected_x_axis_field_from_filters) {
    fields_to_exclude <- c(fields_to_exclude, selected_x_axis_field)
  }
  
  if (do_exclude_response_field_from_filters) {
    fields_to_exclude <- c(fields_to_exclude, the_data$MODEL_SETS[[selected_model_set]]$RESPONSE)
  }
  
  return(fields_to_exclude)
}



get_field_filters <- function(UI_ID,
                              the_data,
                              selected_x_axis_field) {
  
  if (is.null(the_data) | is.null(selected_x_axis_field)) {
    return(NULL)
  }
  
  fields_to_exclude_from_filter <- get_fields_to_exclude_from_filter(the_data, selected_x_axis_field, selected_model_set)
  
  fields <- names(the_data$DATA)
  
  fields_to_be_filtered <- fields[!(fields %in% fields_to_exclude_from_filter)]
  
  
  field_filters <- lapply(
    fields_to_be_filtered,
    get_field_filter,
    UI_ID,
    the_data$DATA
  )
  
  return(shiny::tagList(field_filters))
}
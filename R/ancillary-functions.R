get_actual_graph_HC <- function(UI_ID) {
  log_action(action_category = "UI", action = "Getting actual_graph_HC", reference = UI_ID)
  
  result <- highcharter::highchart() %>%
    highcharter::hc_add_series(
      DUMMY_DATA$DATA,
      type = "scatter",
      marker = list(enabled = TRUE),
      highcharter::hcaes(x = drivers, y = DriversKilled)
    )
  
  log_action(action_category = "UI", action = "Got actual_graph_HC", reference = UI_ID)
  
  return(result)
}

get_x_axis_choices <- function(the_data) {
  data_names <- names(the_data$DATA)
  data_names <- data_names[which(data_names != the_data$RESPONSE)]
  return(data_names)
}

get_variable_element <- function(the_data) {
  
  variable_types <- sapply(the_data$DATA, class)
  if (!(all(variable_types %in% c("integer", "numeric", "factor")))) stop()
  
  for (x_axis_choice in x_axis_choices) {
    variable_data <- the_data$DATA[, x_axis_choice]
    variable_data_class <- class(variable_data)
    if (variable_data_class %in% c("integer", "numeric")) {
      shiny::sliderInput(asdf, asdf, asdf, asdf)
    } else {
      shiny::selectInput(asdf, asdf, multiple = TRUE)
    }
  }
} 




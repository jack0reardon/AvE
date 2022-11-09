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
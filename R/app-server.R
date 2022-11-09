app_server <- function(input, output, session) {
  
  output[[ACTUAL_GRAPH_ID]] <- highcharter::renderHighchart({
    get_actual_graph_HC(UI_ID = ACTUAL_GRAPH_ID)
  })
  
}
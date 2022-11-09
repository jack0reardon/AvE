app_ui <- function() {
  shiny::tagList(
    shiny::div(
      id = ENTIRE_PAGE_ID,
      
      shiny::h1("jack"),
      
      shiny::fluidRow(
        shiny::column(6, highcharter::highchartOutput(ACTUAL_GRAPH_ID))
      )
    )
  )
}
app_ui <- function() {
  shiny::tagList(
    shiny::div(
      id = ENTIRE_PAGE_ID,
      
      shiny::h1("jack"),
      
      shiny::fluidRow(
        shinydashboard::box(width = 9,
                            solidHeader = FALSE,
                            status = "primary",
                            collapsible = FALSE,
                            highcharter::highchartOutput(ACTUAL_GRAPH_ID)),
        
        shinydashboard::box(width = 3,
                            solidHeader = FALSE,
                            status = "primary",
                            collapsible = FALSE,
                            shiny::div(
                              shiny::actionButton(APPLY_AB_ID, label = "Apply"),
                              shiny::selectInput(X_AXIS_SI_ID, label = "X-Axis Variable", choices = c()),
                              shiny::uiOutput(VARIABLE_FILTERS_UI_ID)
                            ))
      )
    )
  )
}
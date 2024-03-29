app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("AvE"),
    shiny::helpText("Graph mean, median, and percentiles of prediced response variable alongside actual data"),
    shiny::hr(),
    shiny::sidebarPanel(
      width = 3,
      shiny::div(
        shiny::actionButton(APPLY_AB_ID, label = "Apply"),
        shiny::hr(),
        shiny::selectInput(SELECTED_MODEL_SET_SI_ID, label = "Model Set", choices = c()),
        shiny::selectInput(SELECTED_MODEL_SI_ID, label = "Model", choices = c()),
        shiny::selectInput(X_AXIS_SI_ID, label = "X-Axis Variable", choices = c()),
        shiny::uiOutput(FIELD_FILTERS_UI_ID)
      )
    ),
    shiny::mainPanel(
      width = 9,
      shiny::h2("Graph of response variable by selected x-axis variable"),
      highcharter::highchartOutput(MAIN_GRAPH_ID)
    )
  )
}
app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_data <- shiny::reactiveVal(NULL)
  has_updated_variables_and_filters <- shiny::reactiveVal(FALSE)
  
  
  
  #### 2 Reactive relationships ####
  
  has_updated_variables_and_filters <- shiny::reactive(!is.null(selected_data()))
  
  
  
  #### 3.1 First observe: Load data ####
  
  shiny::observeEvent(on_load(), {
    waiter::waiter_show(html = waiter::spin_wave())
    selected_data(DUMMY_DATA)
  })
  
  
  
  #### 3.2 Second observe: Update filters and set graph specifications ####
  
  shiny::observeEvent(has_updated_variables_and_filters(), {
    if (has_updated_variables_and_filters()) {
      x_axis_choices <- get_x_axis_choices(selected_data())
      
      shiny::updateSelectInput(session, X_AXIS_SI_ID,
                               choices = x_axis_choices)
      
      output[[VARIABLE_FILTERS_UI_ID]] <- shiny::renderUI({ shiny::helpText(shiny::HTML("Help"))})
      
      output[[ACTUAL_GRAPH_ID]] <- highcharter::renderHighchart({
        get_actual_graph_HC(UI_ID = ACTUAL_GRAPH_ID)
      })
    }
  })
  
}
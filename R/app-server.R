app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_data <- shiny::reactiveVal(NULL)
  has_updated_variables_and_filters <- shiny::reactiveVal(FALSE)
  
  
  
  #### 2 Reactive relationships ####
  
  has_updated_variables_and_filters <- shiny::reactive(!is.null(selected_data()))
  x_axis_field_choices <- shiny::reactive(get_x_axis_field_choices(selected_data()))
  
  
  
  #### 3.1 First observe: Load data ####
  
  shiny::observeEvent(on_load(), {
    waiter::waiter_show(html = waiter::spin_wave())
    selected_data(DUMMY_DATA)
  })
  
  
  
  #### 3.2 Second observe: Update filters and set graph specifications ####
  
  shiny::observeEvent(x_axis_field_choices(), {
    if (!is.null(x_axis_field_choices())) {
      shiny::updateSelectInput(session, X_AXIS_SI_ID,
                               choices = shiny::isolate(x_axis_field_choices()))
      
      output[[FIELD_FILTERS_UI_ID]] <- shiny::renderUI({ shiny::helpText(shiny::HTML("Help")) })
    }
  })
  
  shiny::observeEvent(input[[X_AXIS_SI_ID]], {
    # Proceed to update graph only if the X_AXIS_SI_ID has been populated with field names
    if (nchar(input[[X_AXIS_SI_ID]]) > 0) {
      output[[MAIN_GRAPH_ID]] <- highcharter::renderHighchart({
        get_main_graph_HC(UI_ID = MAIN_GRAPH_ID,
                          the_data = selected_data(),
                          x_axis_field = input[[X_AXIS_SI_ID]])
      })
    }
  })
  
}
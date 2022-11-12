app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_data <- shiny::reactiveVal(NULL)
  main_graph <- shiny::reactiveVal(NULL)
  
  
  
  #### 2 Reactive relationships ####
  
  x_axis_field_choices <- shiny::reactive(get_x_axis_field_choices(selected_data()))
  field_filters <- shiny::reactive(get_field_filters(UI_ID = FIELD_FILTERS_UI_ID,
                                                     the_data = selected_data(),
                                                     x_axis_field_choices = x_axis_field_choices(),
                                                     x_axis_field = input[[X_AXIS_SI_ID]]))
  
  
  
  #### 3.1 First observe: Load data ####
  
  shiny::observeEvent(on_load(), {
    waiter::waiter_show(html = waiter::spin_wave())
    selected_data(DUMMY_DATA)
  })
  
  shiny::observeEvent(selected_data(), {
    waiter::waiter_hide()
  })
  
  
  
  #### 3.2 Other observes ####
  
  shiny::observeEvent(x_axis_field_choices(), {
    if (!is.null(x_axis_field_choices())) {
      shiny::updateSelectInput(session, X_AXIS_SI_ID,
                               choices = x_axis_field_choices())
    }
  })
  
  shiny::observeEvent(field_filters(), {
    # Proceed to update filters only if the field_filters() has been updated
    if (!is.null(field_filters())) {
      output[[FIELD_FILTERS_UI_ID]] <- shiny::renderUI({
        field_filters()
      })
    }
  })
  
  shiny::observeEvent(input[[APPLY_AB_ID]], {
    # Proceed to update graph only if the field_filters() has been updated
    shiny::req(input[[get_sub_ID(FIELD_FILTERS_UI_ID, x_axis_field_choices()[which(x_axis_field_choices() != input[[X_AXIS_SI_ID]])][1])]])
  
    main_graph(get_main_graph_HC(UI_ID = MAIN_GRAPH_ID,
                                 the_data = selected_data(),
                                 x_axis_field = input[[X_AXIS_SI_ID]],
                                 input))
  })
  
  output[[MAIN_GRAPH_ID]] <- highcharter::renderHighchart({ main_graph() })
  
}
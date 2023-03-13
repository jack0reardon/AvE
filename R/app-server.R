app_server <- function(input, output, session) {
  
  #### 1 Reactive values ####
  
  on_load <- shiny::reactiveVal("dummy")
  selected_data <- shiny::reactiveVal(NULL)
  main_graph <- shiny::reactiveVal(NULL)
  x_axis_field_choices <- shiny::reactiveVal(NULL)
  
  
  
  #### 2.1 First observe: Load data ####
  
  shiny::observeEvent(on_load(), {
    waiter::waiter_show(html = waiter::spin_wave())
    selected_data(DUMMY_DATA)
  })
  
  shiny::observeEvent(selected_data(), {
    if (!is.null(selected_data())) {
      model_sets <- names(selected_data()$MODEL_SETS)
      
      shiny::updateSelectInput(session, SELECTED_MODEL_SET_SI_ID,
                               choices = model_sets,
                               selected = model_sets[1])
      
      waiter::waiter_hide()
    }
  })
  
  shiny::observeEvent(input[[SELECTED_MODEL_SET_SI_ID]], {
    selected_model_set <- input[[SELECTED_MODEL_SET_SI_ID]]
    
    if (!is.null(selected_model_set)) {
      selected_model_choices <- names(selected_data()$MODEL_SETS[[selected_model_set]]$MODELS)
      shiny::updateSelectInput(session, SELECTED_MODEL_SI_ID,
                               choices = selected_model_choices,
                               selected = selected_model_choices[1])
      
      x_axis_field_choices(get_x_axis_field_choices(selected_data(),
                                                    selected_model_set))
      shiny::updateSelectInput(session, X_AXIS_SI_ID,
                               choices = x_axis_field_choices(),
                               selected = x_axis_field_choices()[1])
    }
  })
  
  shiny::observeEvent(input[[X_AXIS_SI_ID]], {
    selected_x_axis <- input[[X_AXIS_SI_ID]]
    
    if (!is.null(selected_x_axis)) {
      output[[FIELD_FILTERS_UI_ID]] <- shiny::renderUI({
        get_field_filters(UI_ID = FIELD_FILTERS_UI_ID,
                          the_data = selected_data(),
                          selected_x_axis_field = input[[X_AXIS_SI_ID]])
      })
    }
  })
  
  
  
  #### 2.2 Other observes ####
  
  shiny::observeEvent(input[[APPLY_AB_ID]], {
    # Proceed to update graph only if the field_filters() has been updated
    shiny::req(input[[get_sub_ID(FIELD_FILTERS_UI_ID, x_axis_field_choices()[which(x_axis_field_choices() != input[[X_AXIS_SI_ID]])][1])]])
  
    main_graph(get_main_graph_HC(UI_ID = MAIN_GRAPH_ID,
                                 the_data = selected_data(),
                                 selected_x_axis_field = input[[X_AXIS_SI_ID]],
                                 input))
  })
  
  output[[MAIN_GRAPH_ID]] <- highcharter::renderHighchart({ main_graph() })
  
}
#' Run the app
#'
#' @export
run_app <- function() {
  shiny::shinyApp(ui = app_ui(),
                  server = app_server)
}




add_visual_resources <- function() {
  log_action(action_category = "Setup", action = "Adding visual resources", reference = NULL)
  
  lang = getOption("highcharter.lang")
  lang$numericSymbols <- c("k", "M", "B")
  options("highcharter.lang" = lang)
  
  shiny::addResourcePath("www", get_system_file("www", package_name = PACKAGE_NAME))
}
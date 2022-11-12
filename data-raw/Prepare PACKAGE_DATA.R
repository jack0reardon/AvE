# A function for the special shiny ID variables
# Their values should be excatly their variable names
create_shiny_ID <- function(ID) {
  do.call("<<-", list(ID, ID))
}


#### Meta information ####

PACKAGE_NAME <- "ave"
VERSION_NUMBER <- "v1.0"
LOG_FILE_NAME <- "log.csv"



#### UI Elements ####

create_shiny_ID("ENTIRE_PAGE_ID")
create_shiny_ID("MAIN_GRAPH_ID")
create_shiny_ID("APPLY_AB_ID")
create_shiny_ID("X_AXIS_SI_ID")
create_shiny_ID("FIELD_FILTERS_UI_ID")



#### Constants ####

DATA_SAMPLE_SIZE <- 1000
FACTOR_ELEMENTS_MAX_COUNT <- 20
N_PREDICTIONS_ALONG_X_AXIS <- 20
prediction_percentiles_values <- c(0, 0.05, 0.25, 0.75, 0.95, 1)
PREDICTION_PERCENTILES <- setNames(prediction_percentiles_values, nm = sprintf("%1.f%%", prediction_percentiles_values * 100))

create_shiny_ID("INTEGER_VARIABLE_TYPE")
create_shiny_ID("NUMERIC_VARIABLE_TYPE")
create_shiny_ID("FACTOR_VARIABLE_TYPE")


usethis::use_data(PACKAGE_NAME,
                  VERSION_NUMBER,
                  LOG_FILE_NAME,
                  
                  ENTIRE_PAGE_ID,
                  MAIN_GRAPH_ID,
                  APPLY_AB_ID,
                  X_AXIS_SI_ID,
                  FIELD_FILTERS_UI_ID,
                  
                  DATA_SAMPLE_SIZE,
                  FACTOR_ELEMENTS_MAX_COUNT,
                  N_PREDICTIONS_ALONG_X_AXIS,
                  PREDICTION_PERCENTILES,
                  
                  INTEGER_VARIABLE_TYPE,
                  NUMERIC_VARIABLE_TYPE,
                  FACTOR_VARIABLE_TYPE,
                  
                  internal = TRUE,
                  overwrite = TRUE)

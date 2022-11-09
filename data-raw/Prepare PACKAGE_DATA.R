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


usethis::use_data(PACKAGE_NAME,
                  VERSION_NUMBER,
                  LOG_FILE_NAME,
                  
                  ENTIRE_PAGE_ID,
                  
                  internal = TRUE,
                  overwrite = TRUE)

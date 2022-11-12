DUMMY_DATA <- list()

ggplot2::diamonds

DUMMY_DATA$DATA <- as.data.frame(datasets::Seatbelts) %>%
  dplyr::mutate(law = factor(ifelse(law == 1, "yes", "no")))

DUMMY_DATA$MODEL <- glm(DriversKilled ~ . + drivers:front + rear:front + I(front^2),
                        data = DUMMY_DATA$DATA)

DUMMY_DATA$RESPONSE <- "DriversKilled"

DUMMY_DATA$PREDICT_FUNCTION <- function(the_model,
                                        the_data) {
  predict(the_model, newdata = the_data, type = "response")
}



usethis::use_data(DUMMY_DATA,
                  internal = FALSE,
                  overwrite = TRUE)


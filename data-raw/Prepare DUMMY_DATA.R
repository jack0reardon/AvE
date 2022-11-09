DUMMY_DATA <- list()

DUMMY_DATA$DATA <- as.data.frame(datasets::Seatbelts)

DUMMY_DATA$MODEL <- glm(DriversKilled ~ .,
                        data = DUMMY_DATA$DATA)

DUMMY_DATA$RESPONSE <- "DriversKilled"


usethis::use_data(DUMMY_DATA,
                  internal = FALSE,
                  overwrite = TRUE)


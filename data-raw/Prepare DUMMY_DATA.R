DUMMY_DATA <- list()

DUMMY_DATA$DATA <- as.data.frame(datasets::Seatbelts) |>
  dplyr::mutate(law = factor(ifelse(law == 1, "yes", "no")))

DUMMY_DATA$MODEL_SETS <- list(
  drivers_killed_model_set = list(
    RESPONSE = "DriversKilled",
    MODELS = list(
      plain_glm = list(
        MODEL = glm(DriversKilled ~ . + drivers:front + rear:front + I(front^2),
                    family = Gamma(link = "inverse"),
                    data = DUMMY_DATA$DATA),
        PREDICT_FUNCTION = function(the_model,
                                    the_data,
                                    quantiles = NULL) {
          means <- predict(the_model, newdata = the_data, type = "response")
          
          if (is.null(quantiles)) {
            return(means)
          }
          
          dispersion <- sigma(the_model)^2
          alpha <- 1 / dispersion
          betas <- alpha / means
          
          result <- sapply(betas, function(x) {
            qgamma(quantiles, shape = alpha, rate = x)
          }) |> t() |> as.data.frame()
          
          return(result)
        }
      )
    )
  )
)

usethis::use_data(DUMMY_DATA,
                  internal = FALSE,
                  overwrite = TRUE)

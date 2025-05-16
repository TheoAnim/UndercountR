#' @title poisson model
#' @export
#' @param data A named list of data to pass to JAGS
#'
#' @return A list with model outputs and DICs

urc_mcmc <- function(data) {
  future::plan(future::multisession)
  parameters_poisson <-  c("mu", "lambda", "p")
  parameters_zip <- c("lambda", "p", "pi")
  parameters_nb <- c("lambda", "c", "p")
  fit_model <- function(file_name, parameters) {
    data$n_obs <- length(data$yobs)
    data$n_valdata <- length(data$ystar)
    file_path <- system.file(file.path("jags", file_name),
                             package = "Rbayesucsel",
                             mustWork = TRUE)
    lines <- readLines(file_path)
    temp <- tempfile()
    writeLines(lines, temp)
    on.exit(unlink(temp, force = TRUE))
    model <- R2jags::jags(
      model.file = temp,
      data = data,
      parameters.to.save = parameters,
      n.chains = 3,
      n.iter = 8e4,
      quiet = TRUE,
      DIC = TRUE,
      RNGname = "Wichmann-Hill",
      inits = NULL
    )
  }
  # poisson_model <- fit_model("underreported_poisson.jags", parameters_poisson)
  # zip_model <- fit_model("underreported_zip.jags", parameters_zip)
  # nb_model <- fit_model("underreported_nb.jags", parameters_nb)
  # list(poisson = poisson_model,
  #      zip = zip_model,
  #      negbinom = nb_model)

model_files <- c("underreported_poisson.jags", "underreported_zip.jags", "underreported_nb.jags")
model_params <- list(parameters_poisson, parameters_zip, parameters_nb)
model_names <- c("poisson", "zip", "negbinom")
# Parallel model fitting
model_outputs <- purrr::map2(model_files, model_params, fit_model)
models <- rlang::set_names(model_outputs, model_names)
DICs <- tibble(
  model_names,
  DIC = c(
    models$poisson$BUGSoutput$DIC,
    models$zip$BUGSoutput$DIC,
    models$negbinom$BUGSoutput$DIC
  )
)

list(models, DICs)

}

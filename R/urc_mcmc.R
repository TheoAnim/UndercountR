#' @title poisson model
#' @export
#' @param data A named list of data to pass to JAGS
#'
#' @return A list with model outputs and DICs

urc_mcmc <- function(data) {
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
  poisson_model <- fit_model("underreported_poisson.jags", parameters_poisson)
  zip_model <- fit_model("underreported_zip.jags", parameters_zip)
  nb_model <- fit_model("underreported_nb.jags", parameters_nb)
  list(poisson = poisson_model,
       zip = zip_model,
       negbinom = nb_model)
}

#' @title model fitting for underreporting count data
#' @export
#' @param data A named list of data to pass to JAGS
#' @param thresh decision rule for choosing parsimonious model when DICs are very close
#' @param lambda_a shape for prior on expected number of true counts, lambda
#' @param lambda_b rate for prior on expected number of true counts, lambda
#' @return A list with model outputs, DICs and model with smallest DIC


urc_mcmc <- function(data, thresh = 2, lambda_a = 0.1, lambda_b = 0.1) {
  #setup to parallize
  future::plan(future::multisession)
  
  #define parameters
  parameters_poisson <-  c("mu", "lambda", "p")
  parameters_zip <- c("lambda", "p", "pi")
  parameters_nb <- c("lambda", "c", "p")


  #function to fit each model
  fit_model <- function(file_name, parameters) {
    data$n_obs <- length(data$yobs)
    data$n_valdata <- length(data$ystar)
    #jags require prior dist inputs as part of the datalist
    data$lambda_a <- lambda_a
    data$lambda_b <- lambda_b
    density_lambda <- "dgamma(lambda_a, lambda_b)"

    file_path <- system.file(file.path("jags", file_name),
                             package = "Rbayesucsel",
                             mustWork = TRUE)
    lines <- readLines(file_path)
    lines <- gsub(
      pattern = "prior_lambda",
      replacement = density_lambda,
      x = lines,
      fixed = TRUE
    )
    temp <- tempfile()
    on.exit(unlink(temp, force = TRUE))
    writeLines(lines, temp)
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

  model_files <- c("underreported_poisson.jags",
                   "underreported_zip.jags",
                   "underreported_nb.jags")
  model_params <- list(parameters_poisson, parameters_zip, parameters_nb)
  # Parallel model fitting
  model_outputs <- purrr::map2(model_files, model_params, fit_model)
  model_names <- c("poisson", "zip", "negbinom")
  models <- rlang::set_names(model_outputs, model_names)
  DICs <- tibble(
    model_names,
    DIC = c(
      models$poisson$BUGSoutput$DIC,
      models$zip$BUGSoutput$DIC,
      models$negbinom$BUGSoutput$DIC
    )
  )
  list(
    models = models,
    DICs = DICs,
    best_model = model_choice(DICs, thresh = thresh)
  )
}

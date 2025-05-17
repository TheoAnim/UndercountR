#' @title model fitting for underreporting count data
#' @export
#' @param data A named list of data to pass to JAGS
#' @param thresh decision rule for choosing parsimonious model when DICs are very close
#' @param lambda_a shape for prior on expected number of true counts, lambda
#' @param lambda_b rate for prior on expected number of true counts, lambda
#' @return A list with model outputs, DICs and model with smallest DIC


model_choice <- function(df, thresh) {
  # Ensure the tibble has the expected structure
  if (!all(c("model_names", "DIC") %in% colnames(df))) {
    stop("The tibble must have columns 'model_names' and 'DIC'")
  }
  min_dic <- min(df$DIC)

  #models within 2 units of the min DIC
  candidate_models <- df$model_names[df$DIC - min_dic <= thresh]
  # If there are competing models, choose based on parsimony
  if (length(candidate_models) > 1 &
      "poisson" %in% candidate_models) {
    # If poisson is among candidates, choose it (most parsimonious)
    "poisson"
  }
  # If both zip and negbinom are candidates (equally complex), return the one with lower DIC
  else {
    candidate_dics <- df$DIC[df$model_names %in% candidate_models]
    candidate_names <- df$model_names[df$model_names %in% candidate_models]
    candidate_names[which.min(candidate_dics)]
  }
}



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
  list(
    models = models,
    DICs = DICs,
    best_model = model_choice(DICs, thresh = thresh)
  )
}

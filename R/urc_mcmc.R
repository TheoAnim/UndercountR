#' @title Fit Models for Underreported Count Data Using JAGS
#'
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB) models
#' for underreported count data using JAGS. The function selects the most
#' parsimonious model based on the Deviance Information Criterion (DIC).
#'
#' @param data A named list containing the data to be passed to the JAGS models.
#' @param thresh Numeric. Threshold for deciding between models when DICs are close.
#' @param den_lambda A prior distribution for the Poisson or ZIP rate parameter \code{lambda}.
#' @param den_p A prior distribution for the reporting probability \code{p}.
#' @param den_pi A prior distribution for the zero-inflation probability \code{pi} (ZIP only).
#' @param den_c A prior distribution for the dispersion parameter \code{c} (NB only).
#' @param n_iter Integer. Total number of MCMC iterations per chain.
#' @param seed Integer. Random seed for reproducibility.
#' @param n_chains Integer. Number of MCMC chains.
#' @param n_burnin Integer. Number of burn-in iterations to discard.
#' @param inits Optional. A function or list specifying initial values for the MCMC.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{models}{A list of fitted model objects (class \code{rjags}).}
#'   \item{DICs}{A named numeric vector of DIC values for each model.}
#'   \item{best_model}{The model with the lowest DIC.}
#' }
#'
#' @export


urc_mcmc <- function(data,
                     thresh = 2,
                     den_lambda = "dgamma(0.1, 0.1)",
                     den_c = "dunif(0, 20)",
                     den_p = "dunif(0, 1)",
                     den_pi = "dunif(0, 1)",
                     n_iter = 8e4,
                     n_chains = 2,
                     n_burnin = 8e4/2,
                     seed = 123,
                     inits = NULL) {
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

    file_path <- system.file(file.path("jags", file_name),
                             package = "UndercountR",
                             mustWork = TRUE)
    lines <- readLines(file_path)
    #user-specified priors
    if (grepl("nb", file_name)) {
      lines <- gsub(
        pattern = "prior_c",
        replacement = den_c,
        x = lines,
        fixed = TRUE
      )
    } else if (grepl("zip", file_name)) {
      lines <- gsub("prior_pi", den_pi, lines, fixed = TRUE)
    }
    lines <- stringr::str_replace_all(lines,
                                      c("prior_lambda" = den_lambda,
                                        "prior_p" = den_p))

    temp <- tempfile()
    on.exit(unlink(temp, force = TRUE))
    writeLines(lines, temp)
    model <- R2jags::jags(
      model.file = temp,
      data = data,
      parameters.to.save = parameters,
      n.chains = n_chains,
      n.iter = n_iter,
      quiet = TRUE,
      DIC = TRUE,
      RNGname = "Wichmann-Hill",
      inits = NULL,
      n.burnin = n_burnin,
      jags.seed = seed
    )
  }

  model_files <- c(
    "underreported_poisson.jags",
    "underreported_zip.jags",
    "underreported_nb.jags"
  )

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

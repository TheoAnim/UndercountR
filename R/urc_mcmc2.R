#' @title Fit Underreported Count Models with Two Independent Samples via JAGS.
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB) models
#' for underreported count data using JAGS. The function selects the most
#' parsimonious model based on the Deviance Information Criterion (DIC).
#'
#' @param x A named list containing the data for JAGS models. Must include:
#'   'yobs' (observed count df), 'ystar' (true "unobserved" count df),
#'   and 'yv' (validation set df). All dataframes are 2-dimensional.
#'
#' @param thresh Numeric. Threshold for deciding between models when DICs are close. Default is \code{2}.
#' @param prior_lambda A prior distribution for the Poisson or ZIP rate parameter \code{lambda}.
#' @param prior_p A prior distribution for the reporting probability \code{p}.
#' @param prior_pi A prior distribution for the zero-inflation probability \code{pi} (ZIP only).
#' @param prior_c A prior distribution for the dispersion parameter \code{c} (NB only).
#' @param n_iter Integer. Total number of MCMC iterations per chain.
#' @param n_chains Integer. Number of MCMC chains.
#' @param n_burnin Integer. Number of burn-in iterations to discard.
#' @param inits Optional. A function or list specifying initial values for the MCMC.
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{models}{A list of fitted model objects (class \code{rjags}).}
#'   \item{DICs}{A named numeric vector of DIC values for each model.}
#'   \item{best_model}{The model with the lowest DIC.}
#' }
#'
#' @export


urc_mcmc2 <- function(x,
                      thresh = 2,
                      prior_lambda = "dgamma(0.1, 0.1)",
                      prior_p = "dunif(0, 1)",
                      prior_pi = "dunif(0, 1)",
                      prior_c = "dgamma(0.1, 0.1)",
                      n_iter = 8e3,
                      n_chains = 2,
                      n_burnin = 4e3,
                      inits = NULL,
                      seed = 123) {

  #define parameters
  parameters_poisson <-  c("mu", "lambda", "p")
  parameters_zip <- c("lambda", "p", "pi")
  parameters_nb <- c("lambda", "c", "p")


  #function to fit a model at a time
  fit_model <- function(file_name, parameters) {
    x$n <- nrow(x$yobs) #no. observed counts
    x$nv <- nrow(x$ystar) #no. validation set
    x$J <- ncol(x$yobs) #number of different samples
    file_path <- system.file(file.path("jags", file_name),
                             package = "UndercountR",
                             mustWork = TRUE)
    lines <- readLines(file_path)
    if (grepl("two", file_name) && grepl("nb", file_name)) {
      lines <- gsub(
        pattern = "prior_c",
        replacement = prior_c,
        x = lines,
        fixed = TRUE
      )
    } else if (grepl("two", file_name) && grepl("zip", file_name)) {
      lines <- gsub("prior_pi", prior_pi, lines, fixed = TRUE)
    }
    lines <- gsub("prior_lambda", prior_lambda, lines, fixed = TRUE)
    lines <- gsub("prior_p", prior_p, lines, fixed = TRUE)

    temp <- tempfile()
    on.exit(unlink(temp, force = TRUE))
    writeLines(lines, temp)

    model <- R2jags::jags(
      model.file = temp,
      data = x,
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
  #
  model_files <- c(
    "two_underreported_poisson.jags",
    "two_underreported_zip.jags",
    "two_underreported_nb.jags"
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
    best_model = model_choice(DICs, thresh = thresh),
    DICs = DICs,
    models = models
  )
}

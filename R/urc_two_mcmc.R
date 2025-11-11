#' @title Fit Two-Sample Underreported Count Models via JAGS
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB) models
#' for two independent underreported count samples using JAGS.
#' The function selects the most parsimonious model based on the Deviance Information Criterion (DIC).
#'
#' @param x A named list containing the data for JAGS models. Must include:
#'   \code{yobs1}, \code{yobs2} (observed counts),
#'   \code{ystar1}, \code{ystar2} (true counts for validation),
#'   \code{yval1}, \code{yval2} (validation observed counts)
#' @param thresh Numeric. Threshold for deciding between models when DICs are close. Default is 2.
#' @param prior_lambda1 Prior for lambda (sample 1, Poisson/NB)
#' @param prior_lambda2 Prior for lambda (sample 2, Poisson/NB)
#' @param prior_p1 Prior for reporting probability (sample 1)
#' @param prior_p2 Prior for reporting probability (sample 2)
#' @param prior_pi1 Prior for zero-inflation probability (sample 1, ZIP only)
#' @param prior_pi2 Prior for zero-inflation probability (sample 2, ZIP only)
#' @param prior_c1 Prior for NB dispersion (sample 1, NB only)
#' @param prior_c2 Prior for NB dispersion (sample 2, NB only)
#' @param n_iter Total number of MCMC iterations per chain.
#' @param n_chains Number of MCMC chains.
#' @param n_burnin Number of burn-in iterations.
#' @param inits Optional initial values (function or list).
#' @param seed Random seed for reproducibility.
#' @param parallel Logical.
#'   If `TRUE`, model fitting is performed in parallel using the
#'   \pkg{future} and \pkg{furrr} frameworks.
#'   This enables simultaneous fitting of the Poisson, zero-inflated Poisson,
#'   and negative binomial models across multiple workers.
#'   If `FALSE`, the models are fitted sequentially.
#' @return A named list containing:
#' \describe{
#'   \item{models}{A list of fitted \code{rjags} model objects.}
#'   \item{DICs}{A tibble with DIC values for each model.}
#'   \item{best_model}{The model with the lowest DIC.}
#' }
#'
#' @export
urc_two_mcmc <- function(x,
                         thresh = 2,
                         prior_lambda1 = "dgamma(0.1,0.1)",
                         prior_lambda2 = "dgamma(0.1,0.1)",
                         prior_p1      = "dbeta(1,1)",
                         prior_p2      = "dbeta(0,1)",
                         prior_pi1     = "dbeta(0,1)",
                         prior_pi2     = "dbeta(0,1)",
                         prior_c1      = "dgamma(0.1,0.1)",
                         prior_c2      = "dgamma(0.1,0.1)",
                         n_iter = 8e3,
                         n_chains = 2,
                         n_burnin = 4e3,
                         inits = NULL,
                         seed = 123,
                         parallel = FALSE) {
  # --- sample sizes ---
  x$n1  <- length(x$yobs1)
  x$n2  <- length(x$yobs2)
  x$nv1 <- length(x$ystar1)
  x$nv2 <- length(x$ystar2)

  # --- parameters for each model ---
  parameters_poisson <- c("mu1", "mu2", "lambda1", "lambda2", "p1", "p2")
  parameters_zip     <- c("lambda1", "lambda2", "p1", "p2", "pi1", "pi2")
  parameters_nb      <- c("lambda1", "lambda2", "p1", "p2", "c1", "c2")

  # --- function to fit a single model ---
  fit_model <- function(file_name, parameters) {
    file_path <- system.file(file.path("jags", file_name),
                             package = "UndercountR",
                             mustWork = TRUE)
    lines <- readLines(file_path)

    # replace priors
    if (grepl("nb", file_name)) {
      lines <- gsub("prior_c1", prior_c1, lines, fixed = TRUE)
      lines <- gsub("prior_c2", prior_c2, lines, fixed = TRUE)
    }
    if (grepl("zip", file_name)) {
      lines <- gsub("prior_pi1", prior_pi1, lines, fixed = TRUE)
      lines <- gsub("prior_pi2", prior_pi2, lines, fixed = TRUE)
    }
    lines <- gsub("prior_lambda1",
                  prior_lambda1,
                  fixed = TRUE,
                  x = lines)
    lines <- gsub("prior_lambda2",
                  prior_lambda2,
                  fixed = TRUE,
                  x = lines)
    lines <- gsub("prior_p1", prior_p1, fixed = TRUE, x = lines)
    lines <- gsub("prior_p2", prior_p2, fixed = TRUE, x = lines)

    temp <- tempfile(fileext = ".jags")
    on.exit(unlink(temp, force = TRUE))
    writeLines(lines, temp)

    R2jags::jags(
      model.file = temp,
      data = x,
      parameters.to.save = parameters,
      n.chains = n_chains,
      n.iter = n_iter,
      n.burnin = n_burnin,
      inits = inits,
      DIC = TRUE,
      quiet = TRUE,
      RNGname = "Wichmann-Hill",
      jags.seed = seed
    )
  }

  # --- JAGS files ---
  model_files <- c("two_sample_poisson.jags",
                   "two_sample_zip.jags",
                   "two_sample_nb.jags")
  model_params <- list(parameters_poisson, parameters_zip, parameters_nb)

  # --- fit models in parallel --
  if (parallel) {
    model_outputs <- furrr::future_map2(model_files,
                                        model_params,
                                        fit_model,
                                        .options = furrr::furrr_options(seed = seed))
  } else{
    model_outputs <- purrr::map2(model_files, model_params, fit_model)
  }
  model_names <- c("poisson", "zip", "negbinom")
  models <- rlang::set_names(model_outputs, model_names)

  # --- DIC table ---
  dics <- tibble::tibble(
    model_names = model_names,
    DIC = c(
      models$poisson$BUGSoutput$DIC,
      models$zip$BUGSoutput$DIC,
      models$negbinom$BUGSoutput$DIC
    )
  )

  list(
    dic_best = dic_choice(dics, thresh = thresh),
    dics = dics,
    models = models
  )
}


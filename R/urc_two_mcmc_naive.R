#' @title Fit Two-Sample Naive Count Models via JAGS
#'
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB)
#' models for two independent count samples assuming no underreporting.
#' The function compares models using the Deviance Information Criterion
#' (DIC), Watanabe-Akaike Information Criterion (WAIC), and
#' Pareto-smoothed importance sampling leave-one-out cross-validation
#' (PSIS-LOO).
#'
#' @param x A named list containing the observed count data for the two
#'   samples. Must include \code{yobs1} and \code{yobs2}.
#'
#' @param thresh Numeric. The threshold used for the parsimony rule. If the
#' underreported Poisson model is within \code{thresh} units of the minimum
#' value of a model-selection criterion, the Poisson model is selected as the
#' preferred model; otherwise, the model with the minimum value of the
#' criterion is selected. Applied separately to DIC, WAIC, and PSIS-LOO. Default is 2.
#' 
#' @param prior_lambda1 Prior for lambda in sample 1
#'   (Poisson/NB/ZIP).
#'
#' @param prior_lambda2 Prior for lambda in sample 2
#'   (Poisson/NB/ZIP).
#'
#' @param prior_pi1 Prior for the zero-inflation probability in sample 1
#'   (ZIP only).
#'
#' @param prior_pi2 Prior for the zero-inflation probability in sample 2
#'   (ZIP only).
#'
#' @param prior_c1 Prior for the NB dispersion parameter in sample 1
#'   (NB only).
#'
#' @param prior_c2 Prior for the NB dispersion parameter in sample 2
#'   (NB only).
#'
#' @param n_iter Total number of MCMC iterations per chain.
#'
#' @param n_chains Number of MCMC chains.
#'
#' @param n_burnin Number of burn-in iterations.
#'
#' @param inits Optional initial values (function or list).
#'
#' @param seed Random seed for reproducibility.
#'
#' @param parallel Logical.
#'   If \code{TRUE}, model fitting is performed in parallel using the
#'   \pkg{future} and \pkg{furrr} frameworks. If \code{FALSE}, models
#'   are fitted sequentially.
#'
#' @return A named list containing:
#' \describe{
#'   \item{models}{A list of fitted \code{rjags} model objects.}
#'   \item{dics}{A data frame containing DIC values for each model.}
#'   \item{waics}{A data frame containing WAIC values for each model.}
#'   \item{loos}{A data frame containing PSIS-LOO values for each model.}
#'   \item{dic_best}{The model selected using DIC.}
#'   \item{waic_best}{The model selected using WAIC.}
#'   \item{loo_best}{The model selected using PSIS-LOO.}
#' }
#'
#' @export
urc_two_mcmc_naive <- function(
    x,
    thresh = 2,
    prior_lambda1 = "dgamma(0.1,0.1)",
    prior_lambda2 = "dgamma(0.1,0.1)",
    prior_pi1 = "dbeta(1,1)",
    prior_pi2 = "dbeta(1,1)",
    prior_c1 = "dgamma(0.1,0.1)",
    prior_c2 = "dgamma(0.1,0.1)",
    n_iter = 8e3,
    n_chains = 2,
    n_burnin = 4e3,
    inits = NULL,
    seed = 123,
    parallel = FALSE) {

  # --- sample sizes ---
  x$n1 <- length(x$yobs1)
  x$n2 <- length(x$yobs2)

  # --- parameters for each model ---
  parameters_poisson <- c(
    "lambda1",
    "lambda2",
    "rate_ratio",
    "loglik"
  )

  parameters_zip <- c(
    "lambda1",
    "lambda2",
    "rates_ratio",
    "pi1",
    "pi2",
    "loglik"
  )

  parameters_nb <- c(
    "lambda1",
    "lambda2",
    "rates_ratio",
    "c1",
    "c2",
    "loglik"
  )

  # --- function to fit a single model ---
  fit_model <- function(file_name, parameters) {

    file_path <- system.file(
      file.path("jags", file_name),
      package = "BUCM",
      mustWork = TRUE
    )

    lines <- readLines(file_path)

    # --- replace lambda priors ---
    lines <- gsub(
      "prior_lambda1",
      prior_lambda1,
      lines,
      fixed = TRUE
    )

    lines <- gsub(
      "prior_lambda2",
      prior_lambda2,
      lines,
      fixed = TRUE
    )

    # --- replace ZIP priors ---
    if (grepl("zip", file_name)) {

      lines <- gsub(
        "prior_pi1",
        prior_pi1,
        lines,
        fixed = TRUE
      )

      lines <- gsub(
        "prior_pi2",
        prior_pi2,
        lines,
        fixed = TRUE
      )
    }

    # --- replace NB priors ---
    if (grepl("nb", file_name)) {

      lines <- gsub(
        "prior_c1",
        prior_c1,
        lines,
        fixed = TRUE
      )

      lines <- gsub(
        "prior_c2",
        prior_c2,
        lines,
        fixed = TRUE
      )
    }

    # --- temporary JAGS file ---
    temp <- tempfile(fileext = ".jags")
    on.exit(unlink(temp, force = TRUE))

    writeLines(lines, temp)

    # --- fit model ---
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
  model_files <- c(
    "two_sample_poisson_naive.jags",
    "two_sample_zip_naive.jags",
    "two_sample_nb_naive.jags"
  )

  model_params <- list(
    parameters_poisson,
    parameters_zip,
    parameters_nb
  )

  # --- fit models ---
  if (parallel) {

    model_outputs <- furrr::future_map2(
      model_files,
      model_params,
      fit_model,
      .options = furrr::furrr_options(seed = seed)
    )

  } else {

    model_outputs <- purrr::map2(
      model_files,
      model_params,
      fit_model
    )
  }

  # --- model names ---
  model_names <- c(
    "poisson",
    "zip",
    "negbinom"
  )

  models <- rlang::set_names(
    model_outputs,
    model_names
  )

  # --- DIC table ---
  dics <- data.frame(
    model_names = model_names,
    DIC = c(
      models$poisson$BUGSoutput$DIC,
      models$zip$BUGSoutput$DIC,
      models$negbinom$BUGSoutput$DIC
    )
  )

  # --- WAIC and PSIS-LOO ---
  waics <- waic_comparison(models)
  loos <- loo_comparison(models)

  # --- return results ---
  list(
    models = models,
    dics = dics,
    waics = waics,
    loos = loos,
    dic_best = dic_choice(
      dics,
      thresh = thresh
    ),
    waic_best = waic_choice(
      waics,
      thresh = thresh
    ),
    loo_best = loo_choice(
      loos,
      thresh = thresh
    )
  )
}
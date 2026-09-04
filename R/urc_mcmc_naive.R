#' @title Fit Count Models Assuming no Underreporting
#'
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB) models. Model selection
#' is performed using the Deviance Information Criterion (DIC), Watanabe-Akaike
#' Information Criterion (WAIC), and Pareto-smoothed importance sampling
#' leave-one-out cross-validation (PSIS-LOO). For each criterion, the function
#' applies a parsimony rule that favors the underreported Poisson model when its
#' criterion value is within a specified threshold of the minimum value across
#' the candidate models; otherwise, the model with the minimum criterion value
#' is selected.
#'
#' @param x A named list containing the data(observed counts) to be passed to the JAGS models.
#'
#' @param thresh Numeric. The threshold used for the parsimony rule. If the
#' underreported Poisson model is within \code{thresh} units of the minimum
#' value of a model-selection criterion, the Poisson model is selected as the
#' preferred model; otherwise, the model with the minimum value of the
#' criterion is selected. Applied separately to DIC, WAIC, and PSIS-LOO. Default is 2.
#' 
#' @param prior_lambda A prior distribution for the Poisson or ZIP rate parameter \code{lambda}.
#'
#' @param prior_pi A prior distribution for the zero-inflation probability \code{pi} (ZIP only).
#'
#' @param prior_c A prior distribution for the dispersion parameter \code{c} (NB only).
#'
#' @param n_iter Integer. Total number of MCMC iterations per chain.
#'
#' @param n_chains Integer. Number of MCMC chains.
#'
#' @param n_burnin Integer. Number of burn-in iterations to discard.
#'
#' @param inits Optional. A function or list specifying initial values for the MCMC.
#'
#' @param seed Integer. Random seed for reproducibility.
#'
#' @param parallel Logical.
#'   If `TRUE`, model fitting is performed in parallel using the
#'   \pkg{future} and \pkg{furrr} frameworks.
#'   This enables simultaneous fitting of the Poisson, zero-inflated Poisson,
#'   and negative binomial models across multiple workers.
#'   If `FALSE`, the models are fitted sequentially.
#' @return A named list containing:
#' \describe{
#'   \item{models}{A named list of fitted \code{rjags} model objects.}
#'   \item{dics}{A data frame containing the DIC value for each fitted model.}
#'   \item{waics}{A data frame containing the WAIC value for each fitted model.}
#'   \item{loos}{A data frame containing the PSIS-LOO value for each fitted model.}
#'   \item{dic_best}{The preferred model selected using the DIC-based parsimony rule.}
#'   \item{waic_best}{The preferred model selected using the WAIC-based parsimony rule.}
#'   \item{loo_best}{The preferred model selected using the PSIS-LOO-based parsimony rule.}
#' }
#'
#' @export
urc_mcmc_naive <- function(
    x,
    thresh = 2,
    prior_lambda = "dgamma(0.1, 0.1)",
    prior_c = "dgamma(0.1, 0.1)",
    prior_pi = "dbeta(1, 1)",
    n_iter = 8000,
    n_chains = 2,
    n_burnin = n_iter / 2,
    seed = 123,
    inits = NULL,
    parallel = FALSE,
    workers = NULL
) {

  # -------------------------------------------------------------------
  # Input validation
  # -------------------------------------------------------------------

  if (!is.list(x) || !"yobs" %in% names(x)) {
    stop(
      "`x` must be a named list containing `yobs`.",
      call. = FALSE
    )
  }

  if (!is.numeric(x$yobs) || length(x$yobs) == 0) {
    stop(
      "`x$yobs` must be a non-empty numeric vector.",
      call. = FALSE
    )
  }

  if (n_iter <= 0 || n_chains <= 0 || n_burnin < 0 ||
      n_burnin >= n_iter) {
    stop(
      "`n_iter`, `n_chains`, and `n_burnin` must have valid values.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Model specifications
  # -------------------------------------------------------------------

  model_specs <- list(
    poisson = list(
      file = "naive_poisson.jags",
      parameters = c("lambda", "loglik")
    ),
    zip = list(
      file = "naive_zip.jags",
      parameters = c("lambda", "pi", "loglik")
    ),
    negbinom = list(
      file = "naive_nb.jags",
      parameters = c("lambda", "c", "loglik")
    )
  )

  # -------------------------------------------------------------------
  # Set up parallel processing only when requested
  # -------------------------------------------------------------------

  if (parallel) {

    old_plan <- future::plan()

    on.exit(
      future::plan(old_plan),
      add = TRUE
    )

    if (is.null(workers)) {
      workers <- min(
        length(model_specs),
        max(1, future::availableCores() - 1)
      )
    }

    if (.Platform$OS.type == "windows") {
      future::plan(
        future::multisession,
        workers = workers
      )
    } else {
      future::plan(
        future::multicore,
        workers = workers
      )
    }
  }

  # -------------------------------------------------------------------
  # Fit a single model
  # -------------------------------------------------------------------

  fit_model <- function(model_name, spec, model_seed) {

    data <- x
    data$n_obs <- length(data$yobs)

    model_path <- system.file(
      "jags",
      spec$file,
      package = "BUCM",
      mustWork = TRUE
    )

    model_code <- readLines(model_path)

    # Replace user-specified priors
    model_code <- gsub(
      "prior_lambda",
      prior_lambda,
      model_code,
      fixed = TRUE
    )

    if (model_name == "zip") {
      model_code <- gsub(
        "prior_pi",
        prior_pi,
        model_code,
        fixed = TRUE
      )
    }

    if (model_name == "negbinom") {
      model_code <- gsub(
        "prior_c",
        prior_c,
        model_code,
        fixed = TRUE
      )
    }

    # Write modified JAGS model to a temporary file
    temp_model <- tempfile(fileext = ".jags")

    on.exit(
      unlink(temp_model, force = TRUE),
      add = TRUE
    )

    writeLines(model_code, temp_model)

    R2jags::jags(
      model.file = temp_model,
      data = data,
      parameters.to.save = spec$parameters,
      n.chains = n_chains,
      n.iter = n_iter,
      n.burnin = n_burnin,
      inits = inits,
      DIC = TRUE,
      quiet = TRUE,
      RNGname = "Wichmann-Hill",
      jags.seed = model_seed
    )
  }

  # -------------------------------------------------------------------
  # Fit all models
  # -------------------------------------------------------------------

  model_names <- names(model_specs)

  model_seeds <- seed + seq_along(model_names) - 1

  if (parallel) {

    models <- furrr::future_map2(
      model_names,
      model_seeds,
      ~ fit_model(
        model_name = .x,
        spec = model_specs[[.x]],
        model_seed = .y
      ),
      .options = furrr::furrr_options(
        seed = seed
      )
    )

  } else {

    models <- purrr::map2(
      model_names,
      model_seeds,
      ~ fit_model(
        model_name = .x,
        spec = model_specs[[.x]],
        model_seed = .y
      )
    )
  }

  names(models) <- model_names

  # -------------------------------------------------------------------
  # Model comparison metrics
  # -------------------------------------------------------------------

  dics <- data.frame(
    model_names = model_names,
    DIC = vapply(
      models,
      function(model) model$BUGSoutput$DIC,
      numeric(1)
    ))

  waics <- waic_comparison(models)

  loos <- loo_comparison(models)
  rownames(dics) <- NULL
  rownames(waics) <- NULL
  rownames(loos) <- NULL
  # -------------------------------------------------------------------
  # Return results
  # -------------------------------------------------------------------

  list(
    models = models,
    dics = dics,
    waics = waics,
    loos = loos,
    dic_best = dic_choice(dics, thresh = thresh),
    waic_best = waic_choice(waics, thresh = thresh),
    loo_best = loo_choice(loos, thresh = thresh)
  )
}

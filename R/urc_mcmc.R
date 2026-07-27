#' @title Fit Models for Underreported Count Data Using JAGS
#'
#' @description
#' Fits Poisson, zero-inflated Poisson (ZIP), and negative binomial (NB) models
#' for underreported count data using JAGS. The function selects the most
#' parsimonious model based on the Deviance Information Criterion (DIC).
#'
#' @param x A named list containing the data(observed counts, true "unobserved counts" and validation set) to be passed to the JAGS models.
#'
#' @param thresh Numeric. Threshold for deciding between models when DICs are close.
#'
#' @param prior_lambda A prior distribution for the Poisson or ZIP rate parameter \code{lambda}.
#'
#' @param prior_p A prior distribution for the reporting probability \code{p}.
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
#' @return A named list with the following components:
#' \describe{
#'   \item{models}{A list of the three fitted model objects (class \code{rjags}).}
#'   \item{dics}{A named numeric vector of DIC values for each model.}
#'   \item{waics}{A named numeric vector of WAIC values for each model.}
#'   \item{loos}{A named numeric vector of LOO values for each model.}
#'   \item{dic_best}{The parsimonous or the model with the lowest DIC.}
#'   \item{waic_best}{The parsimonous or the model with the lowest WAIC}
#'   \item{loo_best}{The parsimonous or the model with the lowest PSIS-LOO}
#' }
#'\code{vignette("vignette", package = "BUCM")}.
#' @export

urc_mcmc <- function(x,
                     thresh = 2,
                     prior_lambda = "dgamma(0.1, 0.1)",
                     prior_c = "dgamma(0.1, 0.1)",
                     prior_p = "dbeta(1, 1)",
                     prior_pi = "dbeta(1, 1)",
                     n_iter = 8e3,
                     n_chains = 2,
                     n_burnin = floor(n_iter / 2),
                     seed = 123,
                     inits = NULL,
                     parallel = FALSE,
                     quiet = TRUE,
                     ...) {

  # ---Input Validation ---
  if (!is.list(x) || !all(c("yobs", "ystar", "yval") %in% names(x))) {
    stop("Argument 'x' must be a named list containing 'yobs', 'ystar', and 'yval'")
  }

  # Logic check to prevent JAGS Inconsistent Node errors
  # yval (confirmed counts) cannot be greater than ystar (total true counts)
  if (any(x$yval > x$ystar, na.rm = TRUE)) {
    stop("Logical error: 'yval' cannot be greater than 'ystar' in validation data.")
  }

  # Check parallel plan
  if (parallel && inherits(future::plan(), "sequential")) {
    warning("Parallel = TRUE but no plan set. Running sequentially. Run plan(multisession) first.")
  }

  # ---Setup Paths and Parameters ---
  model_filenames <- c("underreported_poisson.jags", "underreported_zip.jags", "underreported_nb.jags")
  model_paths <- purrr::map_chr(model_filenames, ~{
    path <- system.file("jags", .x, package = "BUCM")
    if (path == "") stop(paste("JAGS file not found:", .x))
    path
  })

  model_params <- list(
    c("mu", "lambda", "p", "loglik"),
    c("lambda", "p", "pi", "loglik"),
    c("lambda", "c", "p", "loglik")
  )

  # ---Internal Fit Function ---
  fit_model <- function(file_path, parameters) {
    data_list <- x
    data_list$n_obs <- length(data_list$yobs)
    data_list$n_valdata <- length(data_list$ystar)

    lines <- readLines(file_path)

    # Priors by User
    if (grepl("nb", file_path)) {
      lines <- gsub("prior_c", prior_c, lines, fixed = TRUE)
    } else if (grepl("zip", file_path)) {
      lines <- gsub("prior_pi", prior_pi, lines, fixed = TRUE)
    }
    lines <- gsub("prior_lambda", prior_lambda, lines, fixed = TRUE)
    lines <- gsub("prior_p", prior_p, lines, fixed = TRUE)

    temp <- tempfile(fileext = ".jags")
    writeLines(lines, temp)
    on.exit(unlink(temp), add = TRUE)

    # Use modifyList to handle ellipsis (...) without double-argument errors
    jags_args <- list(
      model.file = temp,
      data = data_list,
      parameters.to.save = parameters,
      n.chains = n_chains,
      n.iter = n_iter,
      n.burnin = n_burnin,
      jags.seed = seed,
      quiet = quiet,
      DIC = TRUE
    )

    # Merges user ... into defaults; user choices override defaults
    final_args <- utils::modifyList(jags_args, list(...))

    do.call(R2jags::jags, final_args)
  }

  # ---Execution ---
  if (parallel) {
    model_outputs <- furrr::future_map2(
      model_paths, model_params, fit_model,
      .options = furrr::furrr_options(
        seed = seed,
        globals = TRUE,
        packages = c("R2jags", "BUCM")
      )
    )
  } else {
    model_outputs <- purrr::map2(model_paths, model_params, fit_model)
  }

  model_names <- c("poisson", "zip", "negbinom")
  models <- rlang::set_names(model_outputs, model_names)

  # --- Metrics and Selection ---
  dics <- data.frame(
    model_names,
    DIC = purrr::map_dbl(models, ~ .x$BUGSoutput$DIC)
  )
  waics <- waic_comparison(models) # creates a df with waic for each model
  loos <- loo_comparison(models)

  rownames(dics) <- NULL
  rownames(waics) <- NULL
  rownames(loos) <- NULL
  # named list of jags model and metrics
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

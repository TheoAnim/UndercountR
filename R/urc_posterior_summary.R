#' @title Summarize Posterior Distributions
#'
#' @description
#' Produces a posterior summary for a fitted JAGS model using
#' \code{MCMCvis::MCMCsummary()}. Parameters associated with the
#' log-likelihood are excluded automatically. Additional arguments
#' can be passed directly to \code{MCMCvis::MCMCsummary()}.
#'
#' @param model A fitted JAGS model object returned by \code{R2jags::jags()}.
#'
#' @param exclude Optional character vector specifying additional parameters
#'   to exclude. Exact parameter names or base parameter names can be used.
#'   For example, \code{"lambda[1]"} excludes only that parameter, while
#'   \code{"lambda"} excludes all indexed versions of lambda.
#'
#' @param ... Additional arguments passed to
#'   \code{MCMCvis::MCMCsummary()}.
#'
#' @return A data frame containing posterior summaries.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(x = mydata)
#'
#' # Posterior summary
#' urc_posterior_summary(output$models$poisson)
#'
#' # Exclude additional parameters
#' urc_posterior_summary(
#'   output$models$zip,
#'   exclude = c("pi")
#' )
#'
#' # Exclude indexed parameters
#' urc_posterior_summary(
#'   output$models$poisson,
#'   exclude = c("lambda[1]")
#' )
#'
#' # Pass additional arguments to MCMCsummary
#' urc_posterior_summary(
#'   output$models$poisson,
#'   Rhat = TRUE,
#'   n.eff = TRUE
#' )
#' }
urc_posterior_summary <- function(
    model,
    exclude = NULL,
    ...
) {

  # ---------------------------------------------------------------
  # Check model
  # ---------------------------------------------------------------

  if (is.null(model$BUGSoutput)) {
    stop(
      "`model` does not appear to be a valid R2jags model object.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Get available parameters
  # ---------------------------------------------------------------

  available_parameters <- colnames(
    model$BUGSoutput$sims.matrix
  )

  if (is.null(available_parameters)) {
    stop(
      "No posterior samples were found in the model.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Automatically exclude log-likelihood parameters
  # ---------------------------------------------------------------

  loglik_parameters <- available_parameters[
    grepl("^loglik(?:\\[|$)", available_parameters)
  ]

  # ---------------------------------------------------------------
  # Build exclusion vector
  # ---------------------------------------------------------------

  exclude_parameters <- unique(
    c(
      loglik_parameters,
      exclude
    )
  )

  # ---------------------------------------------------------------
  # Remove parameters from posterior summary
  # ---------------------------------------------------------------

  parameters <- setdiff(
    available_parameters,
    exclude_parameters
  )

  if (length(parameters) == 0) {
    stop(
      "No parameters remain after applying `exclude`.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------
  # Pass fitted R2jags object to MCMCsummary
  # ---------------------------------------------------------------

  MCMCvis::MCMCsummary(
    object = model,
    params = parameters,
    ...
  )
}
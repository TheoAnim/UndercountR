#' Compare Models Using WAIC
#'
#' Calculates the Widely Applicable Information Criterion (WAIC) for multiple
#' Bayesian models fit using JAGS and returns a comparison table.
#'
#' @param models A list containing JAGS `models` for Poisson, ZIP and Negative Binomial
#'   element which is a list of r2jags output objects, typically containing
#'   Poisson, ZIP (zero-inflated Poisson), and negative binomial models.
#' @param thresh Numeric threshold for model comparison (default = 2).
#'
#' @export
waic_comparison <- function(models, thresh = 2){
  #models <- jagsoutput$models
  model_names <- c("poisson", "zip", "negbinom")
  waic_values <- purrr::map(models, \(x) x$BUGSoutput$sims.list$loglik) |>
    purrr::map(loo::waic) |>
    purrr::map_dbl(\(x) x$estimates["waic", 'Estimate'])
  tibble(
    model_names = model_names,
    waic = waic_values
  )
}

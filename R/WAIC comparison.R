#' Compare Models Using WAIC
#'
#' Calculates the Widely Applicable Information Criterion (WAIC) for multiple
#' Bayesian models fit using JAGS and returns a comparison table.
#'
#' @param jagsoutput A list containing JAGS model outputs. Must have a `models`
#'   element which is a list of r2jags output objects, typically containing
#'   Poisson, ZIP (zero-inflated Poisson), and negative binomial models.
#' @param thresh Numeric threshold for model comparison (default = 2).
#'
#' #' @export
waic_comparison <- function(jagsoutput, thresh = 2){
  models <- jagsoutput$models
  model_names <- c("poisson", "zip", "negbinom")
  waic_values <- purrr::map(models, \(x) x$BUGSoutput$sims.list$ll) |>
    purrr::map(waic) |>
    purrr::map_dbl(\(x) x$estimates["waic", 'Estimate'])
  tibble(
    model = model_names,
    waic = waic_values
  )
}

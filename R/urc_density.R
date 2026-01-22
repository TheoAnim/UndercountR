#' @title Plot Posterior Densities
#'
#' @description Creates a density plot for each parameter in the BUGS MCMC output.
#'
#' @param model A model object of class \code{"rjags"} returned by \code{UndercountR::urc_mcmc()}.
#'
#' @param parameters Optional character vector of parameter names to include in the plot.
#'                   If \code{NULL} (default), all parameters are plotted except deviance
#'
#' @param deviance Logical specifying whether to include plot of posterior deviance. Default is \code{deviance = FALSE}.
#'
#' @return A \code{ggplot} object showing posterior densities for each parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(data = mydata)
#'
#' # Plot posterior densities for the Poisson model
#' urc_density(output$models$poisson)
#' }
urc_density <- function(model,
                        parameters = NULL,
                        deviance = FALSE) {
  samples <- as.data.frame(model$BUGSoutput$sims.matrix)
  if (!deviance) {
    samples <- dplyr::select(samples, -deviance)
  }
  samples <- samples |>
    tibble::rownames_to_column(var = ".iter") |>
    tidyr::pivot_longer(
      cols = -".iter",
      names_to = "parameter",
      values_to = "value"
    )
  if (!is.null(parameters)) {
    samples <- dplyr::filter(samples, with(samples, paramater %in% parameters))
  }

  # drop loglik used to compute loo and waic
  samples <- dplyr::filter(samples, with(samples, !grepl("^loglik\\[", parameter)))
  ggplot2::ggplot(samples, with(samples, aes(x = value))) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~ with(samples, parameter), scales = "free")
}

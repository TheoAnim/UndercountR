#' @title Plot Posterior Histogram
#'
#' @description Creates a histogram plot for each parameter in the BUGS MCMC output.
#'
#' @param model A model object returned by \code{UndercountR::urc_mcmc()}.
#'
#' @param parameters Optional character vector of parameter names to include in the plot.
#'                   If \code{NULL} (default), all parameters are plotted except deviance
#'
#' @param deviance Logical specifying whether to include plot of posterior deviance. Default is \code{deviance = FALSE}.
#'
#' @return A \code{ggplot} object showing posterior histogram for each parameter.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(data = mydata)
#' urc_density(output$models$poisson)
#' }
urc_hist <- function(model,
                     parameters = NULL,
                     deviance = FALSE) {
  samples <- as.data.frame(model$BUGSoutput$sims.matrix)
  if (!deviance) {
    samples <- samples[, colnames(samples) != "deviance"]
  }

  if (!is.null(parameters)) {
    samples <- samples[, parameters, drop = FALSE]
  }
  plot_data <- samples |>
    tibble::rownames_to_column(var = ".iter") |>
    tidyr::pivot_longer(
      cols = -".iter",
      names_to = "parameter",
      values_to = "value"
    )
  # drop loglik used to compute waic and loo
  plot_data <- dplyr::filter(plot_data, with(plot_data, !grepl("^loglik\\[", parameter)))

  ggplot2::ggplot(plot_data, with(plot_data, ggplot2::aes(x = value))) +
    ggplot2::geom_histogram(aes(y = ggplot2::after_stat(density))) +
    ggplot2::facet_wrap(~ with(plot_data, parameter), scales = "free")
}

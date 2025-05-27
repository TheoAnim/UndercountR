#' @title Plot Posterior Histogram
#' @description Creates a histogram plot for each parameter in the BUGS MCMC output.
#'
#' @param model A model object returned by \code{UndercountR::urc_mcmc()}.
#' @param parameters Optional character vector of parameter names to include in the plot.
#'                   If \code{NULL} (default), all parameters are plotted except deviance
#'
#' @return A \code{ggplot} object showing posterior histogram for each parameter.
#' @export
#'
#' @examples
#' \dontrun{
#' poisson_model <- urc_mcmc(data = mydata)
#' urc_hist(poisson_model)
#' }


urc_hist <- function(model,
                        paramaters = NULL,
                        deviance = FALSE) {
  samples <- as.data.frame(model$BUGSoutput$sims.matrix)
  if (!deviance) {
    samples <- dplyr::select(samples, -deviance)
  }

  samples <- samples |> tibble::rownames_to_column(var = ".iter") |>
    tidyr::pivot_longer(cols = -".iter",
                        names_to = "parameter",
                        values_to = "value")
  if (!is.null(paramaters)) {
    samples <- dplyr::filter(samples, paramater %in% parameters)
  }
  ggplot2::ggplot(samples, aes(x = value)) +
    ggplot2::geom_histogram(aes(y = after_stat(density))) +
    ggplot2::facet_wrap( ~ parameter, scales = "free")
}

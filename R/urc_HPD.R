#' @title Plot HPD Intervals
#' @description Visualizes the Highest Posterior Density (HPD) intervals for each parameter in the MCMC output.
#'
#' @param model A model object returned by \code{UndercountR::urc_mcmc()}.
#' @param params Optional character vector of parameter names to include. Default \code{'all'} plots posteriors of all parameters except \code{deviance}
#' @param level Credible level(default is 0.95).
#' @param deviance Logical specifying whether to include plot of posterior deviance. Default is \code{deviance = FALSE}.
#' @return A \code{ggplot} object showing HPD credible intervals.
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(data = mydata)
#' urc_HPD(output$models$poisson)
#' }

urc_HPD <- function(model,
                    parameters = NULL,
                    deviance = FALSE,
                    level = 0.95) {
  if (is.null(model$BUGSoutput$sims.matrix)) {
    stop("The provided model object does not contain 'sims.matrix' MCMC output.")
  }
  samples <- as.data.frame(model$BUGSoutput$sims.matrix)

  if (!is.null(parameters)) {
    samples <- samples[, parameters, drop = FALSE]
  }

  mcmc_samples <- coda::as.mcmc(samples)
  hpd <- coda::HPDinterval(mcmc_samples, prob = level)
  mean_vals <- colMeans(samples)

  plot_data <- tibble::tibble(
    parameter = factor(rownames(hpd), levels = rownames(hpd)),
    lower = hpd[, "lower"],
    upper = hpd[, "upper"],
    mean = mean_vals
  )
  if(!deviance){
    plot_data <- dplyr::filter(plot_data, parameter != "deviance")
  }
  ggplot2::ggplot(plot_data, ggplot2::aes(x = parameter)) +
    ggplot2::geom_pointrange(ggplot2::aes(y = mean, ymin = lower, ymax = upper)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = glue::glue("{level * 100}% HPD Credible Intervals"),
      x = NULL,
      y = "Parameter Value"
    )
}

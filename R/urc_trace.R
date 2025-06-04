#' @title Plot MCMC Trace Plots
#' @description Generates trace plots for each parameter from the BUGS MCMC output.
#' @param model A model object returned by \code{UndercountR::urc_mcmc()}.
#' @param parameters Optional character vector of parameter names to include.
#'        If \code{NULL}, all parameters are plotted with deviance excluded
#' @return A \code{ggplot} object showing trace plots for each parameter across iterations and chains.
#' @export
#' @examples
#' \dontrun{
#' output <- urc_mcmc(data = mydata)
#' urc_density(output$models$poisson)
#' }

urc_trace <- function(model, parameters = NULL, deviance = FALSE) {
  sims_array <- model$BUGSoutput$sims.array
  n_iter <- dim(sims_array)[1]
  n_chain <- dim(sims_array)[2]
  parameters <- dimnames(sims_array)[[3]]
  # Convert to long format
  trace_data <- purrr::map_dfr(
    .x = 1:n_chain,
    .f = function(chain) {
      df <- as.data.frame(sims_array[, chain, , drop = FALSE])
      colnames(df) <- parameters
      df$.chain <- as.factor(chain)
      df$.iter <- seq_len(n_iter)
      df
    }
  ) |>
    tidyr::pivot_longer(
      cols = -c(.iter, .chain),
      names_to = "parameter",
      values_to = "value"
    )

  if (!is.null(parameters)) {
    trace_data <- dplyr::filter(trace_data, parameter %in% parameters)
  }

  if (!deviance) {
    trace_data <- dplyr::filter(trace_data, parameter != "deviance")
  }

  ggplot2::ggplot(trace_data, ggplot2::aes(x = .iter, y = value, color = .chain)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~parameter, scales = "free") +
    ggplot2::labs(
      x = "Iteration",
      y = "Value",
      color = "Chain"
    )

}

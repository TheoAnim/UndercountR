#' @title Plot Posterior Densities
#'
#' @description
#' Creates posterior density plots from a JAGS MCMC model.
#'
#' @param model A fitted JAGS model object containing posterior samples.
#'
#' @param parameters Optional character vector specifying the parameters to plot.
#'   For indexed parameters, the base parameter name can be supplied. For example,
#'   \code{parameters = "theta"} selects \code{theta[1]}, \code{theta[2]}, etc.
#'   If \code{NULL}, all available parameters are plotted except \code{loglik}
#'   and, by default, \code{deviance}.
#'
#' @param deviance Logical; should posterior deviance be included?
#'   Defaults to \code{FALSE}.
#'
#' @return A \code{ggplot} object showing posterior densities.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(x = mydata)
#'
#' # Plot all parameters
#' urc_density(output$models$poisson)
#'
#' # Plot selected parameters
#' urc_density(
#'   output$models$zip,
#'   parameters = c("lambda", "pi")
#' )
#'
#' # Indexed parameters can be selected using their base name
#' urc_density(
#'   output$models$example,
#'   parameters = "theta"
#' )
#'
#' # Further customize the returned ggplot object
#' urc_density(output$models$poisson) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Posterior Densities")
#' }
urc_density <- function(
    model,
    parameters = NULL,
    deviance = FALSE
) {

  # Check model object
  if (is.null(model$BUGSoutput$sims.matrix)) {
    stop(
      "`model` does not contain a valid BUGS posterior sample matrix.",
      call. = FALSE
    )
  }

  # Extract posterior samples
  samples <- as.data.frame(model$BUGSoutput$sims.matrix)

  # Convert to long format and extract base parameter names
  samples <- samples |>
    tibble::rownames_to_column(var = ".iter") |>
    tidyr::pivot_longer(
      cols = -".iter",
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      base_parameter = sub("\\[.*$", "", parameter)
    )

  # Remove log-likelihood values used for WAIC and LOO
  samples <- samples |>
    dplyr::filter(base_parameter != "loglik")

  # Remove deviance unless requested
  if (!deviance) {
    samples <- samples |>
      dplyr::filter(base_parameter != "deviance")
  }

  # Select requested parameters
  if (!is.null(parameters)) {

    if (!is.character(parameters)) {
      stop(
        "`parameters` must be a character vector or NULL.",
        call. = FALSE
      )
    }

    available_parameters <- unique(samples$base_parameter)

    missing_parameters <- setdiff(
      parameters,
      available_parameters
    )

    if (length(missing_parameters) > 0) {
      warning(
        "The following parameters were not found: ",
        paste(missing_parameters, collapse = ", "),
        call. = FALSE
      )
    }

    samples <- samples |>
      dplyr::filter(base_parameter %in% parameters)
  }

  # Stop if no parameters remain after filtering
  if (nrow(samples) == 0) {
    stop(
      "No posterior samples available for the selected parameters.",
      call. = FALSE
    )
  }

  # Create and return plot
  ggplot2::ggplot(
    samples,
    ggplot2::aes(x = value)
  ) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(
      ~ parameter,
      scales = "free"
    ) +
    ggplot2::labs(
      x = "Posterior value",
      y = "Density"
    )
}

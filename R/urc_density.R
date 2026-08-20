#' @title Plot Posterior Densities
#'
#' @description
#' Creates posterior density plots from a JAGS MCMC model.
#'
#' @param model A fitted JAGS model object containing posterior samples.
#'
#' @param parameters Optional character vector specifying the parameters to plot.
#'   If \code{NULL}, all available parameters are plotted except \code{loglik}
#'   and, by default, \code{deviance}. Parameters can be specified by their
#'   exact name (e.g., \code{"lambda[1]"}) or by their base name
#'   (e.g., \code{"lambda"}), which selects all indexed parameters such as
#'   \code{"lambda[1]"}, \code{"lambda[2]"}, etc.
#'
#' @param exclude Optional character vector specifying parameters to exclude
#'   from the plot. Exact parameter names can be used (e.g., \code{"lambda[1]"})
#'   or base parameter names (e.g., \code{"lambda"}) to exclude all indexed
#'   parameters.
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
#' # Plot a specific indexed parameter
#' urc_density(
#'   output$models$zip,
#'   parameters = "lambda[1]"
#' )
#'
#' # Plot selected indexed parameters
#' urc_density(
#'   output$models$zip,
#'   parameters = c("lambda[1]", "lambda[3]")
#' )
#'
#' # Plot all lambda parameters
#' urc_density(
#'   output$models$zip,
#'   parameters = "lambda"
#' )
#'
#' # Exclude a specific indexed parameter
#' urc_density(
#'   output$models$zip,
#'   exclude = "lambda[1]"
#' )
#'
#' # Exclude all lambda parameters
#' urc_density(
#'   output$models$zip,
#'   exclude = "lambda"
#' )
#'
#' # Include posterior deviance
#' urc_density(
#'   output$models$poisson,
#'   deviance = TRUE
#' )
#'
#' # Customize the returned ggplot object
#' urc_density(output$models$poisson) +
#'   ggplot2::theme_minimal() +
#'   ggplot2::labs(title = "Posterior Densities")
#' }
urc_density <- function(
    model,
    parameters = NULL,
    exclude = NULL,
    deviance = FALSE
) {

  # -------------------------------------------------------------------
  # Check model object
  # -------------------------------------------------------------------

  if (is.null(model$BUGSoutput$sims.matrix)) {
    stop(
      "`model` does not contain a valid BUGS posterior sample matrix.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Extract posterior samples
  # -------------------------------------------------------------------

  samples <- as.data.frame(model$BUGSoutput$sims.matrix)

  # -------------------------------------------------------------------
  # Convert to long format
  # -------------------------------------------------------------------

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

  # -------------------------------------------------------------------
  # Remove log-likelihood
  # -------------------------------------------------------------------

  samples <- samples |>
    dplyr::filter(base_parameter != "loglik")

  # -------------------------------------------------------------------
  # Remove deviance unless requested
  # -------------------------------------------------------------------

  if (!deviance) {
    samples <- samples |>
      dplyr::filter(base_parameter != "deviance")
  }

  # -------------------------------------------------------------------
  # Select requested parameters
  # -------------------------------------------------------------------

  if (!is.null(parameters)) {

    if (!is.character(parameters)) {
      stop(
        "`parameters` must be a character vector or NULL.",
        call. = FALSE
      )
    }

    available_parameters <- unique(samples$parameter)
    available_base_parameters <- unique(samples$base_parameter)

    # A parameter is valid if it is either:
    # 1. an exact parameter name, e.g. lambda[1], or
    # 2. a base parameter name, e.g. lambda
    valid_parameters <- parameters[
      parameters %in% available_parameters |
        parameters %in% available_base_parameters
    ]

    missing_parameters <- setdiff(
      parameters,
      valid_parameters
    )

    if (length(missing_parameters) > 0) {
      warning(
        "The following parameters were not found: ",
        paste(missing_parameters, collapse = ", "),
        call. = FALSE
      )
    }

    samples <- samples |>
      dplyr::filter(
        parameter %in% valid_parameters |
          base_parameter %in% valid_parameters
      )
  }

  # -------------------------------------------------------------------
  # Exclude user-specified parameters
  # -------------------------------------------------------------------

  if (!is.null(exclude)) {

    if (!is.character(exclude)) {
      stop(
        "`exclude` must be a character vector or NULL.",
        call. = FALSE
      )
    }

    samples <- samples |>
      dplyr::filter(
        !(
          parameter %in% exclude |
            base_parameter %in% exclude
        )
      )
  }

  # -------------------------------------------------------------------
  # Check that parameters remain
  # -------------------------------------------------------------------

  if (nrow(samples) == 0) {
    stop(
      "No posterior samples available for the selected parameters.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Create plot
  # -------------------------------------------------------------------

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

#' @title Plot Posterior Autocorrelation Functions
#'
#' @description
#' Creates autocorrelation function (ACF) plots for posterior samples
#' from a JAGS MCMC model. Autocorrelation is calculated separately
#' for each MCMC chain and parameter.
#'
#' @param model A fitted JAGS model object containing posterior samples.
#'
#' @param parameters Optional character vector specifying the parameters
#'   to plot. If \code{NULL}, all available parameters are plotted except
#'   \code{loglik} and, by default, \code{deviance}. Parameters can be
#'   specified by their exact name (e.g., \code{"lambda[1]"}) or by their
#'   base name (e.g., \code{"lambda"}), which selects all indexed
#'   parameters.
#'
#' @param exclude Optional character vector specifying parameters to
#'   exclude from the plot. Exact parameter names or base parameter names
#'   can be used.
#'
#' @param deviance Logical; should posterior deviance be included?
#'   Defaults to \code{FALSE}.
#'
#' @param chains Optional numeric vector specifying the chains to plot.
#'   If \code{NULL}, all available chains are plotted.
#'
#' @param max_lag Maximum lag to display. Defaults to \code{50}.
#'
#' @param labels Optional named character vector used to rename parameter
#'   labels in the facets. Names can correspond to exact parameter names
#'   or base parameter names.
#'
#' @param x_label Character string specifying the x-axis label.
#'   Defaults to \code{"Lag"}.
#'
#' @param y_label Character string specifying the y-axis label.
#'   Defaults to \code{"Autocorrelation"}.
#'
#' @param title Optional plot title.
#'
#' @param subtitle Optional plot subtitle.
#'
#' @param caption Optional plot caption.
#'
#' @param scales Character string specifying the scales used for the
#'   facets. Must be one of \code{"fixed"}, \code{"free"}, \code{"free_x"},
#'   or \code{"free_y"}. Defaults to \code{"fixed"}.
#'
#' @param line_args Named list of arguments passed to
#'   \code{ggplot2::geom_line()}.
#'
#' @param theme A ggplot2 theme object. Defaults to
#'   \code{ggplot2::theme_minimal()}.
#'
#' @param theme_args Named list of arguments passed to
#'   \code{ggplot2::theme()}.
#'
#' @param ... Additional ggplot2 layers or components added to the plot.
#'
#' @return A \code{ggplot} object showing posterior autocorrelation
#'   functions for each parameter and MCMC chain.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(x = mydata)
#'
#' # Plot ACF for all parameters and all chains
#' urc_acf(output$models$poisson)
#'
#' # Plot selected parameters
#' urc_acf(
#'   output$models$zip,
#'   parameters = c("lambda", "pi")
#' )
#'
#' # Plot selected chains
#' urc_acf(
#'   output$models$poisson,
#'   chains = 1
#' )
#'
#' # Change maximum lag
#' urc_acf(
#'   output$models$poisson,
#'   max_lag = 100
#' )
#'
#' # Rename parameters
#' urc_acf(
#'   output$models$zip,
#'   labels = c(
#'     lambda = "Mean Count",
#'     pi = "Zero-Inflation Probability"
#'   )
#' )
#'
#' # Customize the plot
#' urc_acf(
#'   output$models$poisson,
#'   title = "Posterior Autocorrelation",
#'   line_args = list(linewidth = 1),
#'   theme = ggplot2::theme_classic()
#' )
#'
#' # Add additional ggplot components
#' urc_acf(output$models$poisson) +
#'   ggplot2::geom_hline(
#'     yintercept = 0,
#'     linetype = "dashed"
#'   )
#' }
urc_acf <- function(
    model,
    parameters = NULL,
    exclude = NULL,
    deviance = FALSE,
    chains = NULL,
    max_lag = 50,
    labels = NULL,
    x_label = "Lag",
    y_label = "Autocorrelation",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    scales = "fixed",
    line_args = list(),
    theme = ggplot2::theme_minimal(),
    theme_args = list(),
    ...
) {

  # -------------------------------------------------------------------
  # Check model object
  # -------------------------------------------------------------------

  if (is.null(model$BUGSoutput$sims.array)) {
    stop(
      "`model` does not contain a valid BUGS posterior sample array.",
      call. = FALSE
    )
  }

  sims <- model$BUGSoutput$sims.array

  # -------------------------------------------------------------------
  # Check dimensions
  # -------------------------------------------------------------------

  if (length(dim(sims)) != 3) {
    stop(
      "`model$BUGSoutput$sims.array` must be a three-dimensional array.",
      call. = FALSE
    )
  }

  n_iter <- dim(sims)[1]
  n_chains <- dim(sims)[2]
  parameter_names <- dimnames(sims)[[3]]

  # -------------------------------------------------------------------
  # Validate max_lag
  # -------------------------------------------------------------------

  if (!is.numeric(max_lag) ||
      length(max_lag) != 1 ||
      is.na(max_lag) ||
      max_lag < 0) {

    stop(
      "`max_lag` must be a single non-negative numeric value.",
      call. = FALSE
    )
  }

  max_lag <- min(
    as.integer(max_lag),
    n_iter - 1
  )

  # -------------------------------------------------------------------
  # Validate chains
  # -------------------------------------------------------------------

  if (!is.null(chains)) {

    if (!is.numeric(chains) ||
        any(is.na(chains)) ||
        any(chains < 1) ||
        any(chains > n_chains) ||
        any(chains != as.integer(chains))) {

      stop(
        "`chains` must contain valid chain numbers.",
        call. = FALSE
      )
    }

    chains <- as.integer(chains)

  } else {

    chains <- seq_len(n_chains)
  }

  # -------------------------------------------------------------------
  # Validate scales
  # -------------------------------------------------------------------

  if (!is.character(scales) ||
      length(scales) != 1 ||
      !scales %in% c(
        "fixed",
        "free",
        "free_x",
        "free_y"
      )) {

    stop(
      paste0(
        "`scales` must be one of: ",
        "'fixed', 'free', 'free_x', or 'free_y'."
      ),
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Validate labels
  # -------------------------------------------------------------------

  if (!is.null(labels)) {

    if (!is.character(labels) ||
        is.null(names(labels)) ||
        any(names(labels) == "")) {

      stop(
        "`labels` must be a named character vector.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------
  # Validate plotting arguments
  # -------------------------------------------------------------------

  if (!is.list(line_args)) {
    stop(
      "`line_args` must be a named list.",
      call. = FALSE
    )
  }

  if (!is.list(theme_args)) {
    stop(
      "`theme_args` must be a named list.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Identify available parameters
  # -------------------------------------------------------------------

  parameter_df <- tibble::tibble(
    parameter = parameter_names,
    base_parameter = sub(
      "\\[.*$",
      "",
      parameter_names
    )
  )

  # Remove log-likelihood
  parameter_df <- parameter_df |>
    dplyr::filter(
      base_parameter != "loglik"
    )

  # Remove deviance unless requested
  if (!deviance) {

    parameter_df <- parameter_df |>
      dplyr::filter(
        base_parameter != "deviance"
      )
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

    available_parameters <- parameter_df$parameter

    available_base_parameters <- unique(
      parameter_df$base_parameter
    )

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
        paste(
          missing_parameters,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    parameter_df <- parameter_df |>
      dplyr::filter(
        parameter %in% valid_parameters |
          base_parameter %in% valid_parameters
      )
  }

  # -------------------------------------------------------------------
  # Exclude parameters
  # -------------------------------------------------------------------

  if (!is.null(exclude)) {

    if (!is.character(exclude)) {
      stop(
        "`exclude` must be a character vector or NULL.",
        call. = FALSE
      )
    }

    parameter_df <- parameter_df |>
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

  if (nrow(parameter_df) == 0) {
    stop(
      "No posterior samples available for the selected parameters.",
      call. = FALSE
    )
  }

  selected_parameters <- parameter_df$parameter

  # -------------------------------------------------------------------
  # Calculate ACF separately for every parameter and chain
  # -------------------------------------------------------------------

  acf_data <- purrr::map_dfr(
    selected_parameters,
    function(parameter_name) {

      purrr::map_dfr(
        chains,
        function(chain_number) {

          values <- sims[
            ,
            chain_number,
            parameter_name
          ]

          acf_result <- stats::acf(
            values,
            lag.max = max_lag,
            plot = FALSE
          )

          tibble::tibble(
            parameter = parameter_name,
            chain = factor(
              paste0("Chain ", chain_number),
              levels = paste0("Chain ", chains)
            ),
            lag = as.numeric(
              acf_result$lag
            ),
            acf = as.numeric(
              acf_result$acf
            )
          )
        }
      )
    }
  )

  # -------------------------------------------------------------------
  # Apply parameter labels
  # -------------------------------------------------------------------

  parameter_df <- parameter_df |>
    dplyr::mutate(
      parameter_label = dplyr::coalesce(
        if (!is.null(labels)) {
          unname(labels[parameter])
        } else {
          NA_character_
        },
        if (!is.null(labels)) {
          unname(labels[base_parameter])
        } else {
          NA_character_
        },
        parameter
      )
    )

  acf_data <- acf_data |>
    dplyr::left_join(
      parameter_df |>
        dplyr::select(
          parameter,
          parameter_label
        ),
      by = "parameter"
    )

  # -------------------------------------------------------------------
  # Create line layer
  # -------------------------------------------------------------------

  line_layer <- do.call(
    ggplot2::geom_line,
    c(
      list(
        mapping = ggplot2::aes(
          x = lag,
          y = acf
        )
      ),
      line_args
    )
  )

  # -------------------------------------------------------------------
  # Create plot
  # -------------------------------------------------------------------

  p <- ggplot2::ggplot(
    acf_data
  ) +
    line_layer +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(chain),
      cols = ggplot2::vars(parameter_label),
      scales = scales
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme

  # -------------------------------------------------------------------
  # Apply theme arguments
  # -------------------------------------------------------------------

  if (length(theme_args) > 0) {

    p <- p +
      do.call(
        ggplot2::theme,
        theme_args
      )
  }

  # -------------------------------------------------------------------
  # Add additional ggplot components
  # -------------------------------------------------------------------

  additional_layers <- list(...)

  if (length(additional_layers) > 0) {

    for (layer in additional_layers) {
      p <- p + layer
    }
  }

  # -------------------------------------------------------------------
  # Return plot
  # -------------------------------------------------------------------

  p
}
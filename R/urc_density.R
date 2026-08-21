#' @title Plot Posterior Densities
#'
#' @description
#' Creates posterior density plots from posterior samples obtained from
#' a JAGS MCMC model. Parameters can be selected individually or by their
#' base parameter name, and the resulting \code{ggplot2} object can be
#' extensively customized.
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
#' @param labels Optional named character vector used to rename parameter
#'   labels in the facets. Names can correspond to exact parameter names
#'   or base parameter names.
#'
#' @param fill Character string specifying the fill color of the density.
#'   Defaults to \code{"#333333"}.
#'
#' @param color Character string specifying the outline color of the
#'   density. Defaults to \code{NA}, meaning no outline is drawn.
#'
#' @param alpha Numeric value between 0 and 1 specifying density
#'   transparency. Defaults to \code{0.25}.
#'
#' @param linewidth Numeric value specifying the width of the density
#'   outline. Defaults to \code{0.6}.
#'
#' @param adjust Numeric value used to adjust the bandwidth of the
#'   density estimate. Values greater than 1 produce a smoother density,
#'   while values less than 1 produce a more detailed density.
#'   Defaults to \code{1}.
#'
#' @param scales Character string specifying the scales used for the
#'   facets. Must be one of \code{"fixed"}, \code{"free"}, \code{"free_x"},
#'   or \code{"free_y"}. Defaults to \code{"free"}.
#'
#' @param ncol Number of columns in the facet layout.
#'
#' @param nrow Number of rows in the facet layout.
#'
#' @param x_label Character string specifying the x-axis label.
#'   Defaults to \code{"Posterior"}.
#'
#' @param y_label Character string specifying the y-axis label.
#'   Defaults to \code{"Density"}.
#'
#' @param title Optional plot title.
#'
#' @param subtitle Optional plot subtitle.
#'
#' @param caption Optional plot caption.
#'
#' @param theme A ggplot2 theme object. Defaults to
#'   \code{ggplot2::theme_minimal()}.
#'
#' @param theme_args Named list of arguments passed to
#'   \code{ggplot2::theme()}.
#'
#' @param density_args Named list of additional arguments passed to
#'   \code{ggplot2::geom_density()}.
#'
#' @param ... Additional ggplot2 layers or components added to the plot.
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
#' # Plot all lambda parameters
#' urc_density(
#'   output$models$zip,
#'   parameters = "lambda"
#' )
#'
#' # Rename parameters
#' urc_density(
#'   output$models$zip,
#'   labels = c(
#'     lambda = "Mean Count",
#'     pi = "Zero-Inflation Probability"
#'   )
#' )
#'
#' # Publication-style plot
#' urc_density(
#'   output$models$poisson,
#'   fill = "#333333",
#'   alpha = 0.25,
#'   color = "#333333",
#'   linewidth = 0.6,
#'   theme = ggplot2::theme_classic()
#' )
#'
#' # Customize density estimation
#' urc_density(
#'   output$models$poisson,
#'   adjust = 1.5
#' )
#'
#' # Add additional ggplot components
#' urc_density(output$models$poisson) +
#'   ggplot2::geom_vline(
#'     xintercept = 0,
#'     linetype = "dashed"
#'   )
#' }
urc_density <- function(
    model,
    parameters = NULL,
    exclude = NULL,
    deviance = FALSE,
    labels = NULL,
    fill = "#333333",
    color = NA,
    alpha = 0.25,
    linewidth = 0.6,
    adjust = 1,
    scales = "free",
    ncol = NULL,
    nrow = NULL,
    x_label = "Posterior",
    y_label = "Density",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    theme = ggplot2::theme_minimal(),
    theme_args = list(),
    density_args = list(),
    ...
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

  samples <- as.data.frame(
    model$BUGSoutput$sims.matrix
  )

  # -------------------------------------------------------------------
  # Convert to long format
  # -------------------------------------------------------------------

  samples <- samples |>
    tibble::rownames_to_column(
      var = ".iter"
    ) |>
    tidyr::pivot_longer(
      cols = -".iter",
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      base_parameter = sub(
        "\\[.*$",
        "",
        parameter
      )
    )

  # -------------------------------------------------------------------
  # Remove log-likelihood
  # -------------------------------------------------------------------

  samples <- samples |>
    dplyr::filter(
      base_parameter != "loglik"
    )

  # -------------------------------------------------------------------
  # Remove deviance unless requested
  # -------------------------------------------------------------------

  if (!deviance) {

    samples <- samples |>
      dplyr::filter(
        base_parameter != "deviance"
      )
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
  # Validate alpha
  # -------------------------------------------------------------------

  if (!is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha < 0 ||
      alpha > 1) {

    stop(
      "`alpha` must be a single value between 0 and 1.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Validate adjust
  # -------------------------------------------------------------------

  if (!is.numeric(adjust) ||
      length(adjust) != 1 ||
      is.na(adjust) ||
      adjust <= 0) {

    stop(
      "`adjust` must be a positive numeric value.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Validate density_args and theme_args
  # -------------------------------------------------------------------

  if (!is.list(density_args)) {
    stop(
      "`density_args` must be a named list.",
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
  # Select requested parameters
  # -------------------------------------------------------------------

  if (!is.null(parameters)) {

    if (!is.character(parameters)) {
      stop(
        "`parameters` must be a character vector or NULL.",
        call. = FALSE
      )
    }

    available_parameters <- unique(
      samples$parameter
    )

    available_base_parameters <- unique(
      samples$base_parameter
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

    samples <- samples |>
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
  # Apply parameter labels
  # -------------------------------------------------------------------

  parameter_labels <- samples |>
    dplyr::distinct(
      parameter,
      base_parameter
    ) |>
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

  samples <- samples |>
    dplyr::left_join(
      parameter_labels,
      by = c(
        "parameter",
        "base_parameter"
      )
    )

  # -------------------------------------------------------------------
  # Create density layer
  # -------------------------------------------------------------------

  density_layer <- do.call(
    ggplot2::geom_density,
    c(
      list(
        mapping = ggplot2::aes(
          x = value
        ),
        fill = fill,
        color = color,
        alpha = alpha,
        linewidth = linewidth,
        adjust = adjust
      ),
      density_args
    )
  )

  # -------------------------------------------------------------------
  # Create plot
  # -------------------------------------------------------------------

  p <- ggplot2::ggplot(
    samples
  ) +
    density_layer +
    ggplot2::facet_wrap(
      ~ parameter_label,
      scales = scales,
      ncol = ncol,
      nrow = nrow
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
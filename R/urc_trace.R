#' @title Plot MCMC Trace Plots
#'
#' @description
#' Creates trace plots for posterior samples from a JAGS MCMC model.
#' Trace plots are displayed separately for each parameter, with MCMC
#' chains distinguished by color.
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
#' @param labels Optional named character vector used to rename parameter
#'   labels in the facets. Names can correspond to exact parameter names
#'   or base parameter names.
#'
#' @param palette Character vector of colors used for the MCMC chains.
#'   If \code{NULL}, a publication-friendly colorblind-safe palette is
#'   used. Colors are recycled if fewer colors are supplied than chains.
#'
#' @param alpha Numeric value between 0 and 1 controlling the transparency
#'   of the trace lines. Defaults to \code{0.6}.
#'
#' @param x_label Character string specifying the x-axis label.
#'   Defaults to \code{"Iteration"}.
#'
#' @param y_label Character string specifying the y-axis label.
#'   Defaults to \code{"Value"}.
#'
#' @param title Optional plot title.
#'
#' @param subtitle Optional plot subtitle.
#'
#' @param caption Optional plot caption.
#'
#' @param scales Character string specifying the scales used for the
#'   facets. Must be one of \code{"fixed"}, \code{"free"}, \code{"free_x"},
#'   or \code{"free_y"}. Defaults to \code{"free"}.
#'
#' @param ncol Number of columns in the facet layout.
#'
#' @param nrow Number of rows in the facet layout.
#'
#' @param line_args Named list of additional arguments passed to
#'   \code{ggplot2::geom_line()}. These arguments override the
#'   corresponding defaults, including \code{alpha}.
#'
#' @param theme A ggplot2 theme object. Defaults to
#'   \code{ggplot2::theme_minimal()}.
#'
#' @param theme_args Named list of arguments passed to
#'   \code{ggplot2::theme()}.
#'
#' @param ... Additional ggplot2 layers or components added to the plot.
#'
#' @return A \code{ggplot} object showing MCMC trace plots.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' output <- urc_mcmc(x = mydata)
#'
#' # Plot all parameters
#' urc_trace(output$models$poisson)
#'
#' # Plot selected parameters
#' urc_trace(
#'   output$models$zip,
#'   parameters = c("lambda", "pi")
#' )
#'
#' # Plot a specific indexed parameter
#' urc_trace(
#'   output$models$zip,
#'   parameters = "lambda[1]"
#' )
#'
#' # Plot all lambda parameters
#' urc_trace(
#'   output$models$zip,
#'   parameters = "lambda"
#' )
#'
#' # Exclude a parameter
#' urc_trace(
#'   output$models$zip,
#'   exclude = "pi"
#' )
#'
#' # Plot only selected chains
#' urc_trace(
#'   output$models$poisson,
#'   chains = c(1, 2)
#' )
#'
#' # Rename parameters
#' urc_trace(
#'   output$models$zip,
#'   labels = c(
#'     lambda = "Mean Count",
#'     pi = "Zero-Inflation Probability"
#'   )
#' )
#'
#' # Change transparency
#' urc_trace(
#'   output$models$poisson,
#'   alpha = 0.3
#' )
#'
#' # Customize chain colors
#' urc_trace(
#'   output$models$poisson,
#'   palette = c("#0072B2", "#D55E00")
#' )
#'
#' # Use grayscale
#' urc_trace(
#'   output$models$poisson,
#'   palette = c("black", "grey50")
#' )
#'
#' # Customize line appearance
#' urc_trace(
#'   output$models$poisson,
#'   line_args = list(
#'     linewidth = 0.4,
#'     alpha = 0.3
#'   )
#' )
#'
#' # Publication-style theme
#' urc_trace(
#'   output$models$poisson,
#'   theme = ggplot2::theme_classic()
#' )
#'
#' # Add additional ggplot layers
#' urc_trace(output$models$poisson) +
#'   ggplot2::geom_hline(
#'     yintercept = 0,
#'     linetype = "dashed"
#'   )
#' }
urc_trace <- function(
    model,
    parameters = NULL,
    exclude = NULL,
    deviance = FALSE,
    chains = NULL,
    labels = NULL,
    palette = NULL,
    alpha = 0.6,
    x_label = "Iteration",
    y_label = "Value",
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    scales = "free",
    ncol = NULL,
    nrow = NULL,
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
  # Validate alpha
  # -------------------------------------------------------------------

  if (!is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      alpha < 0 ||
      alpha > 1) {

    stop(
      "`alpha` must be a single numeric value between 0 and 1.",
      call. = FALSE
    )
  }

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

  # Remove duplicate chains
  chains <- unique(chains)

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
  # Validate palette
  # -------------------------------------------------------------------

  if (!is.null(palette)) {

    if (!is.character(palette) ||
        length(palette) == 0) {

      stop(
        "`palette` must be a non-empty character vector.",
        call. = FALSE
      )
    }

  } else {

    # Okabe-Ito colorblind-safe palette
    palette <- c(
      "#0072B2",  # blue
      "#D55E00",  # vermilion
      "#009E73",  # bluish green
      "#E69F00",  # orange
      "#CC79A7",  # reddish purple
      "#56B4E9",  # sky blue
      "#000000"   # black
    )
  }

  # -------------------------------------------------------------------
  # Validate line arguments
  # -------------------------------------------------------------------

  if (!is.list(line_args)) {

    stop(
      "`line_args` must be a list.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Validate theme arguments
  # -------------------------------------------------------------------

  if (!is.list(theme_args)) {

    stop(
      "`theme_args` must be a list.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------
  # Create parameter information
  # -------------------------------------------------------------------

  parameter_df <- tibble::tibble(
    parameter = parameter_names,
    base_parameter = sub(
      "\\[.*$",
      "",
      parameter_names
    )
  )

  # -------------------------------------------------------------------
  # Remove log-likelihood
  # -------------------------------------------------------------------

  parameter_df <- parameter_df |>
    dplyr::filter(
      base_parameter != "loglik"
    )

  # -------------------------------------------------------------------
  # Remove deviance unless requested
  # -------------------------------------------------------------------

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
  # Create trace data
  # -------------------------------------------------------------------

  trace_data <- purrr::map_dfr(
    chains,
    function(chain_number) {

      df <- as.data.frame(
        sims[
          ,
          chain_number,
          selected_parameters,
          drop = FALSE
        ]
      )

      colnames(df) <- selected_parameters

      df$.chain <- factor(
        paste0("Chain ", chain_number),
        levels = paste0("Chain ", chains)
      )

      df$.iter <- seq_len(n_iter)

      df |>
        tidyr::pivot_longer(
          cols = -c(.iter, .chain),
          names_to = "parameter",
          values_to = "value"
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

  trace_data <- trace_data |>
    dplyr::left_join(
      parameter_df |>
        dplyr::select(
          parameter,
          parameter_label
        ),
      by = "parameter"
    )

  # -------------------------------------------------------------------
  # Assign colors to chains
  # -------------------------------------------------------------------

  chain_colors <- rep(
    palette,
    length.out = length(chains)
  )

  names(chain_colors) <- paste0(
    "Chain ",
    chains
  )

  # -------------------------------------------------------------------
  # Set default alpha.
  #
  # If the user supplies alpha inside line_args, that value takes
  # precedence over the alpha argument.
  # -------------------------------------------------------------------

  if (is.null(line_args$alpha)) {
    line_args$alpha <- alpha
  }

  # -------------------------------------------------------------------
  # Create line layer
  # -------------------------------------------------------------------

  line_layer <- do.call(
    ggplot2::geom_line,
    c(
      list(
        mapping = ggplot2::aes(
          x = .iter,
          y = value,
          color = .chain
        )
      ),
      line_args
    )
  )

  # -------------------------------------------------------------------
  # Create plot
  # -------------------------------------------------------------------

  p <- ggplot2::ggplot(
    trace_data
  ) +
    line_layer +
    ggplot2::facet_wrap(
      ~ parameter_label,
      scales = scales,
      ncol = ncol,
      nrow = nrow
    ) +
    ggplot2::scale_color_manual(
      values = chain_colors
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label,
      color = "Chain",
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
  # Add additional ggplot layers/components
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
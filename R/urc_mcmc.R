#' @title model fitting for underreported count data
#' @param data A named list of data to pass to JAGS
#' @param thresh decision rule for choosing parsimonious model when DICs are very close
#' @param den_lambda prior on expected true counts, lambda
#' @param den_p prior on reporting probability, p
#' @param den_pi prior on zero-inflated parameter, pi of zip
#' @param den_c prior on dispersion parameter, c of negbinom
#' @return A named list of fitted models, DICs and model with smallest DIC
#' @export


urc_mcmc <- function(data,
                     thresh = 2,
                     den_lambda = dgamma(0.1, 0.1),
                     den_c = dunif(0, 20),
                     den_p = dunif(0, 1),
                     den_pi = dunif(0, 1)) {
  #setup to parallize
  future::plan(future::multisession)
  
  #define parameters
  parameters_poisson <-  c("mu", "lambda", "p")
  parameters_zip <- c("lambda", "p", "pi")
  parameters_nb <- c("lambda", "c", "p")
  
  
  #function to fit each model
  fit_model <- function(file_name, parameters) {
    data$n_obs <- length(data$yobs)
    data$n_valdata <- length(data$ystar)
    
    file_path <- system.file(file.path("jags", file_name),
                             package = "Rbayesucsel",
                             mustWork = TRUE)
    lines <- readLines(file_path)
    #alter priors in jags.txt
    if (grepl("nb", file_name)) {
      lines <- gsub(
        pattern = "prior_c",
        replacement = "den_c",
        x = lines,
        fixed = TRUE
      )
    } else if (grepl("zip", file_name)) {
      lines <- gsub("prior_pi", "den_pi", lines, fixed = TRUE)
    }
    lines <- stringr::str_replace_all(lines, 
                                      c("prior_lambda" = "den_lambda", 
                                        "prior_p" = "den_p"))
    
    temp <- tempfile()
    on.exit(unlink(temp, force = TRUE))
    writeLines(lines, temp)
    model <- R2jags::jags(
      model.file = temp,
      data = data,
      parameters.to.save = parameters,
      n.chains = 3,
      n.iter = 8e4,
      quiet = TRUE,
      DIC = TRUE,
      RNGname = "Wichmann-Hill",
      inits = NULL
    )
  }
  
  model_files <- c(
    "underreported_poisson.jags",
    "underreported_zip.jags",
    "underreported_nb.jags"
  )
  
  model_params <- list(parameters_poisson, parameters_zip, parameters_nb)
  
  # Parallel model fitting
  model_outputs <- purrr::map2(model_files, model_params, fit_model)
  model_names <- c("poisson", "zip", "negbinom")
  models <- rlang::set_names(model_outputs, model_names)
  DICs <- tibble(
    model_names,
    DIC = c(
      models$poisson$BUGSoutput$DIC,
      models$zip$BUGSoutput$DIC,
      models$negbinom$BUGSoutput$DIC
    )
  )
  list(
    models = models,
    DICs = DICs,
    best_model = model_choice(DICs, thresh = thresh)
  )
}

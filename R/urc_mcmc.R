#data is a list with observed y, validation and true count
urc_mcmc <- function(data) {
  data$n_obs <- length(data$yobs)
  data$n_valdata <- length(data$ystar)
  file <- "underreported_poisson.jags"
  parameters <- c("mu", "lambda", "p")

  file_package <- system.file(file.path("jags", file),
                              package = "Rbayesucsel",
                              mustWork = TRUE)
  lines <- readLines(file_package)
  temp <- tempfile()
  on.exit(unlink(temp, force = TRUE))
  writeLines(lines, temp)
  model <- R2jags::jags(
    model.file = temp,
    data = data,
    parameters.to.save = parameters,
    n.chains = 3,
    n.iter = 8e3,
    quiet = TRUE,
    DIC = TRUE,
    RNGname = "Wichmann-Hill",
    inits = NULL
  )
  model
}

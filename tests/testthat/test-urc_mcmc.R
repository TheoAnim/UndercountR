
generate_valid_data <- function(n_obs = 15, n_val = 10) {
  set.seed(123)
  y_star_val <- rpois(n_val, lambda = 5 * 0.7)
  list(
    yobs  = rpois(n_obs, lambda = 3),
    ystar = y_star_val,
    yval  = rbinom(n_val, size = y_star_val, prob = 0.7) # Guarantees yval <= ystar
  )
}

test_that("urc_mcmc fails gracefully with incorrect input", {
  # 1. Test missing names in list
  bad_x <- list(yobs = c(1, 2, 3), ystar = c(1, 1, 0)) # missing yval
  expect_error(urc_mcmc(bad_x), "must be a named list")

  # 2. Test non-list input
  expect_error(urc_mcmc(c(1, 2, 3)), "Argument 'x' must be a named list")

  # 3. Test logical inconsistency (yval > ystar)
  # This now passes because our updated urc_mcmc.R has a stop() guard.
  # We suppress warnings here because the metrics would normally complain before the stop.
  inconsistent_x <- list(yobs = rpois(5, 2), ystar = c(1, 1, 1, 1, 1), yval = c(2, 2, 2, 2, 2))

  suppressWarnings({
    expect_error(urc_mcmc(inconsistent_x), "Logical error")
  })
})

test_that("urc_mcmc fits all three models (Poisson, ZIP, NB) correctly", {
  mock_data <- generate_valid_data()

  # Run short chains for all models.
  # We suppress warnings because N=15 always triggers Pareto k warnings.
  suppressWarnings({
    result <- urc_mcmc(
      x = mock_data,
      n_iter = 100,
      n_burnin = 50,
      n_chains = 2,
      parallel = FALSE
    )
  })

  # 1. Structural Checks
  expect_type(result, "list")
  expect_named(result, c("models", "dics", "waics", "loos", "dic_best", "waic_best", "loo_best"))

  # 2. S3 Class Checks for ALL branches
  expect_s3_class(result$models$poisson, "rjags")
  expect_s3_class(result$models$zip, "rjags")
  expect_s3_class(result$models$negbinom, "rjags")

  # 3. Parameter Check
  expect_true("pi" %in% result$models$zip$parameters.to.save)
  expect_true("c" %in% result$models$negbinom$parameters.to.save)

  # 4. Metrics Check
  expect_s3_class(result$dics, "data.frame")
  expect_equal(nrow(result$dics), 3)
})

test_that("parallel execution handles ellipsis and custom settings", {
  skip_on_cran()

  # Set a plan for the test duration to clear the "No plan set" warning
  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)

  mock_data <- generate_valid_data()

  # Test if parallel works and ellipsis passes 'refresh' or 'quiet'
  suppressWarnings({
    expect_no_error({
      res_parallel <- urc_mcmc(
        x = mock_data,
        n_iter = 60,
        n_burnin = 20,
        parallel = TRUE,
        refresh = 0,    # Passed via ...
        quiet = FALSE   # Passed via ...
      )
    })
  })

  expect_s3_class(res_parallel$models$poisson, "rjags")
})

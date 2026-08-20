#' @title Function to generate data
#' @export
urcdata_gen <- function(lambda, p, pi = 0, c = 1, nobs = 100, nv = 0, fun) {
  if (fun == "rpois") {
    ystar <- stats::rpois(nv, lambda)
    yv <- stats::rbinom(nv, ystar, p)
    y <- stats::rpois(nobs, lambda * p)
  } else if (fun == "rzip") {
    ystar <- bizicount::rzip(nv, lambda, pi)
    yv <- stats::rbinom(nv, size = ystar, prob = p)
    y <- bizicount::rzip(nobs, lambda * p, pi)
  } else if (fun == "rnegbin") {
    ystar <- stats::rnbinom(nv, mu = lambda, size = c)
    yv <- stats::rbinom(nv, size = ystar, prob = p)
    y <- stats::rnbinom(nobs, mu = lambda * p, size = c)
  } else {
    stop("fun must be rpois, rzip or rnegbin", call. = FALSE)
  }
  list(
    yval = yv,
    ystar = ystar,
    yobs = y
  )
}

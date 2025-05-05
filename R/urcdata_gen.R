###############function to generate data###########################
####################################
urcdata_gen <- function(lambda, p, pi = 0, c = 1, nobs = 100, nv = 0, fun) {
  set.seed(2)
  if (fun == "rpois") {
    ystar <- rpois(nv, lambda)
    yv <- rbinom(nv, ystar, p)
    y <- rpois(nobs, lambda * p)
  } else if (fun == "rzip") {
    ystar <- bizicount::rzip(nv, lambda, pi)
    yv <- rbinom(nv, size = ystar, prob = p)
    y <- bizicount::rzip(nobs, lambda*p, pi)
  } else{
    ystar <- rnbinom(nv, mu = lambda, size = c)
    yv <- rbinom(nv, size = ystar, prob = p)
    y <- rnbinom(nobs, mu = lambda * p, size = c)
  }
  list(
    yval = yv,
    #n_obs = nobs,
    #nv = nv,
    ystar = ystar,
    yobs = y
  )
}



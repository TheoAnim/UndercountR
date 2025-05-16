#Idea: Each count is modeled as a Poisson with mean that is either 0
#      if the individual is part of the zero-inflated group or
#      gamma distributed(in other words, mean has gamma prior) for
#      the non-zero inflated group

###ZIP without validation set
zip <- "model{
  for(i in 1:n_obs){
    yobs[i] ~ dpois(mu[i])
    mu[i] = lambda*p*is_zero[i] + 0.0001
    is_zero[i] ~ dbern(1 - pi)
  }
   #hyperpriors
  pi ~ dunif(0, 1)
  lambda ~ dgamma(0.0001, 0.0001)
  p ~ dunif(0, 1)
"
writeLines(zip, "zip.txt")
model <- jags.model(zip.txt, data = data, n.chains = 3, quiet = TRUE)
zip <- coda.samples(model, variable.names = c("lambda", "p", "pi"))
update(zip, 4e3)


####ZIP with validation
zipval <- "model{
  #observed data
  for(i in 1:n_obs){
    yobs[i] ~ dpois(mu[i])
    mu[i] = lambda*p*is_zero[i] + 0.0001
    is_zero[i] ~ dbern(1 - pi)
  }
  #validation data
  for(j in 1:n_valdata){
    ystar[j] ~ dpois(mu_star[j])
    yval[j] ~ dbinom(p, ystar[j])
    mu_star[j] <- lambda * is_zerostar[j] + 0.0001
    is_zerostar[j] ~ dbern(1 - pi)
  }

  #hyperpriors
  pi ~ dunif(0, 1)
  lambda ~ dgamma(0.0001, 0.0001)
  p ~ dunif(0, 1)
}
"

writeLines(zipval, "zipwithval")
model <- jags.model(zipwithval, data)
zipwithval <- coda.samples(model, variable.names = c("lambda", "p", "pi"))
update(zipwithval, 4e3)

# BUCM

**BUCM** (Bayesian Underreported Count Models) is an R package for Bayesian analysis of count data with and without underreporting. The package provides a framework for fitting one-sample and two-sample count models using Markov chain Monte Carlo (MCMC) methods implemented through JAGS.

BUCM includes both **naive models**, which assume that the observed counts are fully reported, and **underreported models**, which account for the possibility that only a proportion of the true counts are observed.

Three count distributions are currently supported: Poisson, Zero-Inflated Poisson (ZIP), and Negative Binomial.

---

## Features

* Bayesian inference for count models with and without underreporting

* One-sample and two-sample analyses

* Multiple count distributions

  * Poisson

  * Zero-Inflated Poisson (ZIP)

  * Negative Binomial

* Naive models that assume no underreporting

* Underreported models with a binomial reporting mechanism

* MCMC estimation using JAGS

* Model comparison using Bayesian criteria

  * Deviance Information Criterion (DIC)

  * Watanabe-Akaike Information Criterion (WAIC)

  * Pareto-Smoothed Importance Sampling Leave-One-Out (PSIS-LOO)

* Parsimonious model selection based on the selected Bayesian criterion

* MCMC diagnostic tools

* Posterior summaries and credible intervals

* Compatibility with other Bayesian diagnostic and posterior summary packages
---

## Installation

Install the development version from GitHub.

```r
# Install pak if not already installed
install.packages("pak")

# Install BUCM from GitHub
pak::pkg_install("TheoAnim/BUCM")
```

---

## Required Software

BUCM relies on **JAGS** for Bayesian computation.

Install JAGS from:

https://mcmc-jags.sourceforge.io/

---

## Quick Example

Suppose the true count follows a Poisson distribution with mean $\lambda = 5$ and the probability that an event is reported is $p = 0.7$.

### Simulate underreported zero-inflated Poisson data

```r
library(BUCM)
library(MCMCVis)
library(loo)

set.seed(123)

# Simulation settings
lambda <- 5
p <- 0.7
pi <- 0.3
nv <- 100      # Validation sample size
nobs <- 300    # Non-validation sample size

# Validation sample: latent counts (Y*) and observed counts (Y)
ystar <- bizicount::rzip(nv, lambda = lambda, psi = pi)
yval  <- rbinom(nv, size = ystar, prob = p)

# observed counts subject to underreporting
yobs <- bizicount::rzip(
  nobs,
  lambda = lambda * p,
  psi = pi
)
```

### Fit the Bayesian underreported count models

When validation data are available, fit the models using:
```r
fit <- urc_mcmc(
  x = list(
    yobs = yobs,
    ystar = ystar,
    yval = yval
  ),
  prior_p = "dbeta(22, 18)"
)
```

If no validation data are available, simply set `ystar` and `yval` to `NA`.

```r
fit <- urc_mcmc(
  x = list(
    yobs = yobs,
    ystar = NA,
    yval = NA
  )
)
```


### Model Diagnostics

```r
fit$models$poisson |>
  urc_trace(
    parameters = c("lambda", "p")
  )
```

Support call to bayesplot package

```r
poisson_draws <- fit$models$poisson$BUGSoutput$sims.array

bayesplot::mcmc_trace(
  poisson_draws,
  pars = c("lambda", "p")
)
```


### View model comparison

Models comparison either by DIC, WAIC or PSIS-LOO

```r
fit$dics
```

### Selected model

Returns the plausible parsimonious model or the model with the smallest comparison metric.
```r
fit$dic_best  
```

### Posterior summary

```r
fit$models$poisson |> 
  MCMCvis::MCMCsummary(params = c("p", "lambda"))
```

## Model Comparison

BUCM provides model comparison using DIC, WAIC, and PSIS-LOO.

```r
fit$dics
fit$waics
fit$loos
```
### Selected Model

```r
fit$dic_best
fit$waic_best
fit$loo_best
```

The model-selection procedure uses a parsimony rule. For a given criterion, the underreported Poisson model is preferred when its criterion value is sufficiently close to the minimum across the candidate models. Otherwise, the model with the smallest criterion value is selected.

---

## Supported Models

| Distribution          | Naive | Underreported | One Sample | Two Sample |
| --------------------- | :---: | :-----------: | :--------: | :--------: |
| Poisson               |   ✓   |       ✓       |      ✓     |      ✓     |
| Zero-Inflated Poisson |   ✓   |       ✓       |      ✓     |      ✓     |
| Negative Binomial     |   ✓   |       ✓       |      ✓     |      ✓     |


---

## Package Structure

```
BUCM/
├── R/
├── inst/
│   └── jags/
├── man/
├── vignettes/
├── data/
├── tests/
└── README.md
```

---

### Package Vignette

A more detailed description of the model formulation, model fitting, MCMC diagnostics, posterior summaries, and model selection is provided in the package vignette.

After installing BUCM, the vignette can be accessed using:

```r
browseVignettes("BUCM")
```

## Citation

If you use BUCM in published research, please cite:

Anim Bediako, T., Roberman, J. L., Barth, J., & Stamey, J. D. (2026). *BUCM: An R package for Bayesian modeling of one and two sample count models with underreporting.*

(Citation will be updated after publication.)

---

## Contributing

Bug reports, feature requests, and pull requests are welcome.

If you encounter a bug, please open an issue describing:

* the problem,
* reproducible code,
* package version,
* session information.

---

## License

This package is released under the GPL-3 License.

---

## Author

**Theophilus Anim Bediako**

Department of Statistical Science
Baylor University

For questions or collaborations, please open a GitHub issue or contact the author.

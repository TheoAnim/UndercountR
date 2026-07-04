# BUCM

**BUCM** (Bayesian Underreported Count Models) is an R package for Bayesian analysis of underreported count data. The package provides a unified framework for fitting one-sample and two-sample count models when observed counts are subject to underreporting. Parameter estimation is performed using Markov chain Monte Carlo (MCMC) methods implemented through JAGS.

The package is designed for researchers and practitioners working with count data in epidemiology, public health, ecology, pharmacovigilance, clinical trials, manufacturing, and other fields where incomplete underreporting is common.

---

## Features

* Bayesian inference for underreported count models
* One-sample and two-sample analyses
* Multiple count distributions

  * Poisson
  * Zero-Inflated Poisson (ZIP)
  * Negative Binomial
* MCMC estimation using JAGS
* Simulation tools for generating underreported count data
* Model selection using Bayesian criteria

Compatible with other packages for generating:

* Posterior summaries and credible intervals
* Trace plots and posterior density plots
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

After installation, install the required R packages.

```r
install.packages(c(
  "R2jags",
  "coda",
  "tidyverse",
  "purrr"
))
```

---
## Quick Example

### Simulate underreported zero-inflated Poisson data

```r
library(BUCM)
library(bizicount)
library(MCVis)

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

# Non-validation sample: observed counts only
yobs <- bizicount::rzip(
  nobs,
  lambda = lambda * p,
  psi = pi
)
```

### Fit the Bayesian underreported count models

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

### View model comparison

```r
fit$DICs
```

### Selected model

```r
fit$best_model
```

### Posterior summary

```r
MCMCsummary(fit$models[[fit$best_model]])
```

---

## Supported Models

| Distribution          | One Sample | Two Sample |
| --------------------- | :--------: | :--------: |
| Poisson               |      ✓     |      ✓     |
| Zero-Inflated Poisson |      ✓     |      ✓     |
| Negative Binomial     |      ✓     |      ✓     |

Additional models will be added in future releases.

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


## Citation

If you use BUCM in published research, please cite:

Bediako, T. A., Roberman, J. L., Barth, J., & Stamey, J. D. (2026). *Bayesian modeling of one and two-sample count data with underreporting.*

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

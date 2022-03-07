# odemodeling

[![codecov](https://codecov.io/gh/jtimonen/odemodeling/branch/main/graph/badge.svg?token=YLMK3KO0L0)](https://codecov.io/gh/jtimonen/odemodeling)

R-package for building and fitting Bayesian ODE models in Stan.

##  Installation

* Install `cmdstanr` following the instructions [here](https://mc-stan.org/cmdstanr/).
* Install `odemodeling` using

```r
remotes::install_github("jtimonen/odemodeling", ref = "main", build_vignettes = TRUE)
```

Building the vignette takes around one minute. You can skip it with `build_vignettes = FALSE`.

## Getting started

If you built the vignette, you can view it using

```r
browseVignettes("odemodeling")
```

That is perhaps the best place to start. More detailed info is in documentation, that you can view with
```r
library(odemodeling)
?odemodeling
```

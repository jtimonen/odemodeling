# odemodeling

[![codecov](https://codecov.io/gh/jtimonen/odemodeling/branch/main/graph/badge.svg?token=YLMK3KO0L0)](https://codecov.io/gh/jtimonen/odemodeling)

R-package for building and fitting Bayesian ODE models in Stan. The package might still have some sharp corners but:
1. once you have learned how to use it, coding up new ODE models should be faster with it than raw Stan
2. once you have created a model with it, you can do the fitting using different ODE solvers (adaptive, non-adaptive) without needing to write new Stan code
3. once you have fitted a model with it, you can 
* quickly visualize the ODE solutions without needing to extract them yourself
* quickly solve and visualize the ODE solutions using a different solver or at a different set of time points without needing to write new Stan code yourself
* quickly assess whether the solver used during fitting was accurate enough, or if you need to do the fitting again with a more accurate one ([Timonen et al., 2023](https://onlinelibrary.wiley.com/doi/full/10.1002/sta4.614))


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

It is also available online [here](https://jtimonen.github.io/om.html). The
vignette is perhaps the best place to start. More detailed info is in documentation, that you can view with
```r
library(odemodeling)
?odemodeling
```

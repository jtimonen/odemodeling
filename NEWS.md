# Early development versions

## odemodeling 0.0.21
  * Fix a bug in `plot_r_eff()` which plotted the wrong metric.
  
## odemodeling 0.0.20
  * Edits the `$reliability()` method for the `OdeModelMCMC` class so that
  it only ever compares `OdeModelGQ` objects due to possible
  i/o information loss.
  
## odemodeling 0.0.19
  * Only internal improvements in tests.
  
## odemodeling 0.0.18
  * Adds `plot_metric()` and other plotting functions.

## odemodeling 0.0.17
  * Removes accidentally duplicated code and improves test coverage.

## odemodeling 0.0.16
  * Fixes the output of the `$sample_manyconf()` method of `OdeModel` class.
  * Adds the `compute_reliability_metrics()` function.
  * Adds an initial version of the `$reliability()` method for the
  `OdeModelMCMC` class.
  
## odemodeling 0.0.15
  * Adds more result extraction and plotting methods for `OdeModelFit`s.
  * Makes it easier to extract initial state `y0` always in the same format,
  no matter if it is a parameter, data, or transformation.
  
## odemodeling 0.0.14
  * Includes `lynxhare` data.
  * Adds Lotka-Volterra model to example models.
  
## odemodeling 0.0.13
  * Renames the `$simulate()` method to `$gqs()` and implement it also for
  `OdeModel`.
  * Implements the useful `$make_params()` method for `OdeModel`.

## odemodeling 0.0.12
  * Changes `data vector y0` to `vector y0` in ODE solver function signatures
  so it is possible to have initial state be parameter.
  * Adds the `name` argument to `example_ode_model()` and adds the `tmdd`
  example.
  
## odemodeling 0.0.11

  * Simplifies and makes `stan_param()` and `stan_transform()` less vulnerable
  to errors.
  * With `ode_model()`, it is now possible to create also a model where 
  `odefun_vars` and `loglik_vars` are empty lists.
  
## odemodeling 0.0.10

  * Includes `ode_model()` for creating models.
  * Includes `stan_var()`, `stan_param()` etc. for model specification.
  * Includes `rk45()`, `bdf()`, `midpoint()`, `rk4()` etc. for solver
  specification.
  * Includes the `OdeModel` and `OdeModelMCMC` and `OdeModelGQ` R6 classes.
  * Implements parameter sampling via the `$sample()` method of the
  `OdeModel` class.
  * Implements generating quantities with possibly different solver or solver
  configuration (the `$simulate()` method of `OdeModelMCMC` class)
  * Implemented other useful methods of `OdeModelMCMC` and `OdeModelGQ`
   classes are for example `$extract_unflattened()`, `$extract_odesol()`, 
   `$plot_odesol()`, `$dim()` and `$dim_odesol()`.
  * Implements some comparison functions between `OdeModelMCMC` and 
  `OdeModelGQ` objects, such as `log_ratios()`, `psis()`, 
  `max_abs_odesol_diff()` and `max_abs_loglik_diff()`.

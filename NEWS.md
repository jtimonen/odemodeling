# Early development versions

## odemodeling 0.0.13
  * Rename the `$simulate()` method to `$gqs()` and implement it also for
  `OdeModel`.
  * Implement the useful `$make_params()` method for `OdeModel`.

## odemodeling 0.0.12
  * Change `data vector y0` to `vector y0` in ODE solver function signatures
  so it is possible to have initial state be parameter.
  * Add the `name` argument to `example_ode_model()` and add the `tmdd`
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

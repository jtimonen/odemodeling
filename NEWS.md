# Early development versions

# odemodeling 0.0.10

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

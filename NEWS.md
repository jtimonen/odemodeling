# Early development versions

## odemodeling 0.0.3
  * Implements generating quantities with possibly different solver
  and solver configurations
  * Repurpose `OdeModelFit` class as a subclass of `OdeModelMCMC` and
  `OdeModelGQ`

## odemodeling 0.0.2
  * Includes `create_odemodel()`, `sample_odemodel()` and the model
  specification functions `stan_var()`, `stan_param()` etc.
  * Includes the `OdeModel` and `OdeModelFit` classes. Implemented useful
  methods of the latter are `$extract_unflattened()`, `$extract_odesol()`,
  `$dim()`, `$dim_odesol()`.
  * Includes a tutorial stub and some tests.

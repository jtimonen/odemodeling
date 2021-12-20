#' Create an ODE model
#'
#' @export
#' @description Generate model given the declarations of variables,
#' parameters, functions etc. The arguments `odefun_vars`,
#' `loglik_vars` and `other_vars` must be lists whose elements must have one
#' of the following three types:
#' \itemize{
#'  \item `StanDeclaration` - can be created using `stan_var()`,
#'  `stan_vector()`, `stan_array()` etc.
#'  \item `StanParameter` - can be created using `stan_param()`
#'  \item `StanTransformation` - can be created using `stan_transform()`
#' }
#'
#' These will go to different blocks of the 'Stan' model code so that
#' \itemize{
#'   \item `StanDeclaration`s go to `data`
#'   \item `StanParameter`s go to `parameters`
#'   \item `StanTransformation`s with origin `"data"` go to
#'   `transformed data`
#'   \item `StanTransformation`s with origin `"param"` go to
#'   `transformed parameters`
#'   \item `StanTransformation`s with origin `"model"` go to
#'   `generated quantities`
#' }
#' @export
#' @param N A `StanDimension` variable describing the number of time points.
#' @param odefun_vars Data and parameters needed by the ODE function. Must be a
#' list of `StanDeclaration`, `StanParameter`, or `StanTransformation` objects.
#' These will be defined in Stan model code blocks
#' @param odefun_body ODE function body (Stan code string).
#' @param odefun_init Initial value for ODE system at t0.
#' Has to be a `StanVector`, or alternatively a `StanParameter` or a
#' `StanTransformation` with `StanVector` base declaration.
#' @param loglik_vars Data and parameters needed by the log likelihood
#'  function.
#' @param loglik_body Log likelihood function body (Stan code string).
#' @param other_vars Other variables.
#' @param verbose Should this print more information?
#' @param compile Should the model be compiled?
#' @return An object of class `OdeModel`.
#' @family setup functions
create_odemodel <- function(N,
                            odefun_vars = list(),
                            odefun_body = "",
                            odefun_init = NULL,
                            loglik_vars = list(),
                            loglik_body = "",
                            other_vars = list(),
                            verbose = FALSE,
                            compile = TRUE) {

  # Argument checks
  choices_vars <- c("StanParameter", "StanTransformation", "StanDeclaration")
  checkmate::assert_class(N, "StanDimension")
  checkmate::assert_list(odefun_vars, choices_vars)
  checkmate::assert_list(loglik_vars, choices_vars)
  checkmate::assert_list(other_vars, choices_vars)
  checkmate::assert_string(odefun_body, min.chars = 1)
  checkmate::assert_string(loglik_body, min.chars = 1)

  # Check that odefun_init has correct type and name
  choices_init <- c("StanVector", "StanParameter", "StanTransformation")
  checkmate::assert_multi_class(odefun_init, choices_init)
  checkmate::assert_true(get_name(odefun_init) == "x0")

  # Generate full Stan model code
  code_prior <- generate_stancode_prior(odefun_vars, loglik_vars, other_vars)

  # Posterior
  code_posterior <- generate_stancode_posterior(
    N,
    odefun_vars,
    odefun_body,
    odefun_init,
    loglik_vars,
    loglik_body,
    other_vars
  )

  # Create the object
  OdeModel$new(
    code_prior = code_prior,
    code_posterior = code_posterior,
    compile = compile
  )
}

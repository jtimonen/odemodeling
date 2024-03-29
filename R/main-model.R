#' Create an ODE model
#'
#' @export
#' @description Generate model given the declarations of variables,
#' parameters, functions etc. The arguments `odefun_vars`,
#' `loglik_vars` and `other_vars` must be lists whose elements must have one
#' of the following three types:
#' \itemize{
#'  \item [StanDeclaration] - can be created using [stan_var()],
#'  [stan_vector()], [stan_array()] etc.
#'  \item [StanParameter] - can be created using [stan_param()]
#'  \item [StanTransformation] - can be created using [stan_transform()]
#' }
#'
#' These will go to different blocks of the 'Stan' model code so that
#' \itemize{
#'   \item [StanDeclaration]s go to `data`
#'   \item [StanParameter]s go to `parameters`
#'   \item [StanTransformation]s with origin `"data"` go to
#'   `transformed data`
#'   \item [StanTransformation]s with origin `"parameters"` go to
#'   `transformed parameters`
#'   \item [StanTransformation]s with origin `"model"` go to
#'   `generated quantities`
#' }
#' @export
#' @param N A [StanDimension] variable describing the number of time points.
#' @param odefun_vars Data and parameters needed by the ODE function. Must be a
#' list of [StanDeclaration], [StanParameter], or [StanTransformation] objects.
#' These will be defined in Stan model code blocks
#' @param odefun_body ODE function body (Stan code string).
#' @param odefun_init Initial value for ODE system at t0.
#' Has to be a [StanVector], or alternatively a [StanParameter] or a
#' [StanTransformation] with [StanVector] base declaration.
#' @param loglik_vars Data and parameters needed by the log likelihood
#'  function.
#' @param loglik_body Log likelihood function body (Stan code string).
#' @param other_vars Other variables.
#' @param verbose Should this print more information?
#' @param compile Should the model be compiled?
#' @param sig_figs Number of significant figures to use in all 'CmdStan'
#' calls.
#' @return An object of class [OdeModel].
#' @family model constructor functions
ode_model <- function(N,
                      odefun_vars = list(),
                      odefun_body = "",
                      odefun_init = NULL,
                      loglik_vars = list(),
                      loglik_body = "",
                      other_vars = list(),
                      verbose = FALSE,
                      compile = TRUE,
                      sig_figs = 18) {
  # Argument checks
  choices_vars <- c("StanParameter", "StanTransformation", "StanDeclaration")
  checkmate::assert_class(N, "StanDimension")
  checkmate::assert_list(odefun_vars, choices_vars)
  checkmate::assert_list(loglik_vars, choices_vars)
  checkmate::assert_list(other_vars, choices_vars)
  checkmate::assert_string(odefun_body, min.chars = 1)
  checkmate::assert_string(loglik_body, min.chars = 0)

  # Check that odefun_init has correct type and name
  choices_init <- c("StanVector", "StanParameter", "StanTransformation")
  checkmate::assert_multi_class(odefun_init, choices_init)
  y0_name <- get_name(odefun_init)
  if (y0_name != "y0") {
    stop(
      "the Stan variable in odefun_init must have name y0! found = ",
      y0_name
    )
  }

  # Generating prior or posterior model code?
  has_loglik <- nchar(loglik_body) > 0

  # ODE function signature
  odefun_add_signature <- generate_add_signature(odefun_vars, FALSE)
  odefun_add_args <- generate_add_signature(odefun_vars, TRUE)
  so_args <- "solver, rel_tol, abs_tol, max_num_steps, num_steps, y0, t0, t"
  solve_ode_args <- append_to_signature(so_args, odefun_add_args)

  # Log likelihood function signature
  if (has_loglik) {
    loglik_add_signature <- generate_add_signature(loglik_vars, FALSE)
    loglik_add_args <- generate_add_signature(loglik_vars, TRUE)
    loglik_args <- append_to_signature("y_sol_tpar", loglik_add_args)
  } else {
    loglik_add_signature <- ""
    loglik_add_args <- ""
    loglik_args <- ""
  }

  # All vars and their declarations
  base_vars <- list(
    stan_var("t0"), stan_array("t", dims = list(N)), odefun_init
  )
  solver_vars <- list(
    stan_var("abs_tol", lower = 0),
    stan_var("rel_tol", lower = 0),
    stan_var("max_num_steps", lower = 0, type = "int"),
    stan_var("num_steps", lower = 0, type = "int"),
    stan_var("solver", lower = 0, type = "int")
  )
  D <- get_dims(odefun_init)[[1]]
  if (!has_loglik) {
    y_sol_gq <- stan_transform(
      decl = stan_vector_array("y_sol_gq", dims = list(N), length = D),
      origin = "model",
      code = paste0("solve_ode(", solve_ode_args, ")")
    )
  } else {
    y_sol_tpar <- stan_transform(
      decl = stan_vector_array("y_sol_tpar", dims = list(N), length = D),
      origin = "parameters",
      code = paste0("solve_ode(", solve_ode_args, ")")
    )
    y_sol_gq <- stan_transform(
      decl = stan_vector_array("y_sol_gq", dims = list(N), length = D),
      origin = "model",
      code = paste0("{\n y_sol_gq = y_sol_tpar;  \n}")
    )
    log_lik_tpar <- stan_transform(
      decl = stan_var("log_lik_tpar", "real"),
      origin = "parameters",
      code = paste0("log_likelihood(", loglik_args, ")")
    )
    log_lik_gq <- stan_transform(
      decl = stan_var("log_lik_gq", "real"),
      origin = "model",
      code = "{\n log_lik_gq = log_lik_tpar; \n}"
    )
  }

  # Add y0 as a generated quantity too, so its easy to extract from results
  y0_gq <- stan_transform(
    stan_vector("y0_gq", length = D),
    origin = "model",
    code = "{\n y0_gq = y0; \n}"
  )

  if (!has_loglik) {
    other_vars_add <- list(y_sol_gq, y0_gq)
  } else {
    other_vars_add <- list(
      y_sol_tpar, y_sol_gq, y0_gq, log_lik_tpar, log_lik_gq
    )
  }

  other_vars <- c(other_vars_add, other_vars)
  all_vars <- c(odefun_vars, loglik_vars, other_vars, base_vars, solver_vars)
  all_vars <- unique(all_vars)
  all_decls <- lapply(all_vars, get_decl)
  dims <- dims_of_decls(all_decls)

  # Create most blocks
  data <- all_vars[sapply(all_vars, is_data)]
  tdata <- all_vars[sapply(all_vars, is_tdata)]
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]
  gqs <- all_vars[sapply(all_vars, is_gq)]
  data_b <- generate_data_block(all_decls, data)
  tdata_b <- generate_transform_block("transformed data", tdata, FALSE)
  pars_b <- generate_params_block(params)
  tpars_b <- generate_transform_block("transformed parameters", tparams, TRUE)
  model_b <- generate_model_block(params, prior_mode = !has_loglik)
  gq_b <- generate_transform_block("generated quantities", gqs, FALSE)

  # Functions block
  funs_b <- generate_functions_block(
    odefun_add_signature,
    odefun_add_args,
    odefun_body,
    loglik_add_signature,
    loglik_body
  )

  # Merge the blocks
  code <- paste(
    funs_b, data_b, tdata_b, pars_b, tpars_b, model_b, gq_b,
    sep = "\n"
  )
  code <- autoformat_stancode(code)

  # Create Stan model and create the OdeModel
  sm <- StanModelWithCode$new(
    code, dims, data, tdata, params, tparams, gqs, compile
  )
  OdeModel$new(
    has_likelihood = has_loglik,
    stanmodel = sm,
    sig_figs = sig_figs,
    t_dim = N,
    ode_dim = D
  )
}

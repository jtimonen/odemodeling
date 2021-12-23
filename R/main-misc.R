#' Get an unflattened array draw
#'
#' @export
#' @param fit A [cmdstanr::CmdStanMCMC] object.
#' @param iteration Iteration index of draw to get.
#' @param variable Name of the array variable
get_array_draw <- function(fit, variable, iteration) {
  checkmate::assert_class(fit, "CmdStanMCMC")
  checkmate::assert_string(variable)
  checkmate::assert_integerish(iteration)
  checkmate::assert_atomic(iteration)
  draws <- fit$draws(variable)
  arr <- posterior::subset_draws(draws, iteration = iteration)
  rvar <- posterior::as_draws_rvars(arr)[[variable]]
  mean(rvar)
}

#' Create a list of solver configurations (for adaptive stepsize solvers)
#'
#' @export
#' @param tols a numeric vector of tolerances (abs tol and rel tol will be same)
#' @param max_num_steps maximum number of steps (same in all confs)
create_solver_conf_list <- function(tols, max_num_steps) {
  checkmate::assert_numeric(tols)
  checkmate::assert_integerish(max_num_steps)
  L <- length(tols)
  out <- list()
  for (j in seq_len(L)) {
    out[[j]] <- list(
      abs_tol = tols[j],
      rel_tol = tols[j],
      max_num_steps = max_num_steps
    )
  }
  return(out)
}

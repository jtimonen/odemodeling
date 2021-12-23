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

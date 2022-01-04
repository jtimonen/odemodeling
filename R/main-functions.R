#' Compare two objects that contain ODE solutions and ODE model likelihoods
#'
#' @param x An object of class [OdeModelMCMC] or [OdeModelGQ].
#' @param y An object of class [OdeModelMCMC] or [OdeModelGQ].
#' @name compare_odefits
NULL

#' @describeIn compare_odefits Compute maximum absolute difference in
#' ODE solutions
#' @export
max_abs_odesol_diff <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  xx <- x$extract_odesol()
  yy <- y$extract_odesol()
  ad <- compute_abs_diff(xx, yy)
  max(ad)
}

#' @describeIn compare_odefits Compute maximum absolute differences in
#' log likelihoods
#' @export
max_abs_loglik_diff <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  xx <- x$loglik()
  yy <- y$loglik()
  ad <- compute_abs_diff(xx, yy)
  max(ad)
}

# Helper function
compute_abs_diff <- function(x, y) {
  checkmate::assert_true(all(dim(x) == dim(y)))
  abs(x - y)
}

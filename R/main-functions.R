#' Compare two objects that contain ODE solutions and ODE model likelihoods
#'
#' @param x An object of class [OdeModelMCMC] or [OdeModelGQ].
#' @param y An object of class [OdeModelMCMC] or [OdeModelGQ].
#' @name compare_odefits
NULL

#' @describeIn compare_odefits Compute maximum absolute difference in
#' ODE solutions of `x` and `y`.
#' @param include_y0 Should the ODE initial state be included in computations?
#' @export
max_abs_odesol_diff <- function(x, y, include_y0 = TRUE) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  xx <- x$extract_odesol(include_y0 = include_y0)
  yy <- y$extract_odesol(include_y0 = include_y0)
  ad <- compute_abs_diff(xx, yy)
  max(ad)
}

#' @describeIn compare_odefits Compute maximum absolute differences in
#' log likelihoods of `x` and `y`.
#' @export
max_abs_loglik_diff <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  xx <- x$loglik()
  yy <- y$loglik()
  ad <- compute_abs_diff(xx, yy)
  max(ad)
}

#' @describeIn compare_odefits Compute log likelihood ratios (on log scale),
#' i.e. just `x$loglik() - y$loglik()`.
#' @export
log_ratios <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  checkmate::assert_true(all(dim(x) == dim(y)))
  x$loglik() - y$loglik()
}

#' @describeIn compare_odefits Compute relative efficiency needed for PSIS,
#' using [loo::relative_eff()].
#' @export
psis_relative_eff <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  log_ratios <- log_ratios(x, y)
  reciproc_of_importance_ratios <- exp(-log_ratios)
  r_eff <- loo::relative_eff(x = reciproc_of_importance_ratios)
  as.numeric(r_eff)
}

#' @describeIn compare_odefits Call [loo::psis()] using the log likelihoods
#' from `x` and `y`.
#' @export
psis <- function(x, y) {
  checkmate::assert_class(x, "OdeModelFit")
  checkmate::assert_class(y, "OdeModelFit")
  r_eff <- psis_relative_eff(x, y)
  x <- log_ratios(x, y)
  loo::psis(log_ratios = x, r_eff = r_eff)
}

#' Compute reliability metrics
#'
#' @export
#' @param mcmc An object of class [OdeModelMCMC].
#' @param gq An object of class [OdeModelGQ].
#' @return A named numeric vector.
compute_reliability_metrics <- function(mcmc, gq) {
  checkmate::assert_class(mcmc, "OdeModelMCMC")
  checkmate::assert_class(gq, "OdeModelGQ")
  is <- psis(mcmc, gq)
  r_eff <- psis_relative_eff(mcmc, gq)
  pdiag <- is$diagnostics
  pd <- c(pdiag$pareto_k, pdiag$n_eff, r_eff)
  mad_loglik <- max_abs_loglik_diff(mcmc, gq)
  mad_odesol <- max_abs_odesol_diff(mcmc, gq)
  met <- c(pd, mad_loglik, mad_odesol)
  internal_assert_len(met, 5, "compute_reliability_metrics")
  names(met) <- c("pareto_k", "n_eff", "r_eff", "mad_loglik", "mad_odesol")
  return(met)
}

# Helper function
compute_abs_diff <- function(x, y) {
  checkmate::assert_true(all(dim(x) == dim(y)))
  abs(x - y)
}

# odesolvers --------------------------------------------------------------

#' Creating ODE solvers
#'
#' @description These constructors should be used for creating the `solver`
#' argument of the `sample()` method of the [OdeModel] class.
#' Each function here returns an [OdeSolver] which can be either
#' \itemize{
#'    \item An [AdaptiveOdeSolver] which can estimate its own error and
#'    adapts its step size according to given tolerances for the error
#'    \item A [FixedNumStepsOdeSolver] which always takes the same number of
#'    steps between consequent output time points
#' }
#' @param abs_tol Absolute tolerance (only for [AdaptiveOdeSolver]s). Will
#' be replaced by `tol` if it is not `NULL`.
#' @param rel_tol Relative tolerance (only for [AdaptiveOdeSolver]s). Will
#' be replaced by `tol` if it is not `NULL`.
#' @param tol Can be used to set both `abs_tol` and `rel_tol` to same value.
#' Have no effect if `NULL` (default).
#' @param max_num_steps Maximum number of steps between output time
#' points (only for [AdaptiveOdeSolver]s).
#' @param num_steps The number of steps between subsequent output time
#' points (only for [FixedNumStepsOdeSolver]s).
#' @name odesolvers
NULL

#' @describeIn odesolvers Create an RK45 solver ([AdaptiveOdeSolver])
#' @export
rk45 <- function(abs_tol = 1e-6, rel_tol = 1e-6, max_num_steps = 1e6,
                 tol = NULL) {
  abs_tol <- replace_tol(abs_tol, tol)
  rel_tol <- replace_tol(rel_tol, tol)
  AdaptiveOdeSolver$new(
    name = "rk45",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' @describeIn odesolvers Create a BDF solver ([AdaptiveOdeSolver])
#' @export
bdf <- function(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e8,
                tol = NULL) {
  abs_tol <- replace_tol(abs_tol, tol)
  rel_tol <- replace_tol(rel_tol, tol)
  AdaptiveOdeSolver$new(
    name = "bdf",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' @describeIn odesolvers Create an Adams solver ([AdaptiveOdeSolver])
#' @export
adams <- function(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e8,
                  tol = NULL) {
  abs_tol <- replace_tol(abs_tol, tol)
  rel_tol <- replace_tol(rel_tol, tol)
  AdaptiveOdeSolver$new(
    name = "adams",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' @describeIn odesolvers  Create a Cash-Karp solver ([AdaptiveOdeSolver]).
#' @export
ckrk <- function(abs_tol = 1e-6, rel_tol = 1e-6, max_num_steps = 1e6,
                 tol = NULL) {
  abs_tol <- replace_tol(abs_tol, tol)
  rel_tol <- replace_tol(rel_tol, tol)
  AdaptiveOdeSolver$new(
    name = "ckrk",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}


#' @describeIn odesolvers Create a forward Euler solver
#' ([FixedNumStepsOdeSolver])
#' @export
euler <- function(num_steps = 1) {
  FixedNumStepsOdeSolver$new(name = "euler", num_steps = num_steps)
}

#' @describeIn odesolvers Create an explicit midpoint solver
#' ([FixedNumStepsOdeSolver])
#' @export
midpoint <- function(num_steps = 1) {
  FixedNumStepsOdeSolver$new(name = "midpoint", num_steps = num_steps)
}

#' @describeIn odesolvers Create an RK4 solver ([FixedNumStepsOdeSolver])
#' @export
rk4 <- function(num_steps = 1) {
  FixedNumStepsOdeSolver$new(name = "rk4", num_steps = num_steps)
}


# odesolvers_lists --------------------------------------------------------

#' Creating lists of ODE solvers
#'
#' @description These constructors can be used for creating the `solvers`
#' argument of the `sample_manyconf()` method of the [OdeModel] class.
#' Each function here returns a list of [OdeSolver]s.
#' @param tols A vector of length `L` of tolerance values.
#' @param max_num_steps Maximum number of steps between output time
#' points (one number, same for all solvers in the output list).
#' @param num_steps A vector of length `L` of step amounts.
#' @name odesolvers_lists
NULL

#' @describeIn odesolvers_lists Create a list of RK45 solvers with different
#' tolerances.
#' @export
rk45_list <- function(tols, max_num_steps = 1e6) {
  checkmate::assert_numeric(tols)
  creator <- function(x) {
    rk45(tol = x, max_num_steps = max_num_steps)
  }
  sapply(tols, creator)
}

#' @describeIn odesolvers_lists Create a list of BDF solvers with
#' different tolerances.
#' @export
bdf_list <- function(tols, max_num_steps = 1e8) {
  checkmate::assert_numeric(tols)
  creator <- function(x) {
    bdf(tol = x, max_num_steps = max_num_steps)
  }
  sapply(tols, creator)
}

#' @describeIn odesolvers_lists Create a list of explicit midpoint
#' solvers with different fixed number of steps.
#' @export
midpoint_list <- function(num_steps) {
  checkmate::assert_numeric(num_steps)
  creator <- function(x) {
    midpoint(num_steps = x)
  }
  sapply(num_steps, creator)
}

#' @describeIn odesolvers_lists Create a list of RK4
#' solvers with different fixed number of steps.
#' @export
rk4_list <- function(num_steps) {
  checkmate::assert_numeric(num_steps)
  creator <- function(x) {
    rk4(num_steps = x)
  }
  sapply(num_steps, creator)
}

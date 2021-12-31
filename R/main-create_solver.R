#' Create an RK45 solver (adaptive step size)
#'
#' @param abs_tol Absolute tolerance.
#' @param rel_tol Relative tolerance.
#' @param max_num_steps Maximum number of steps.
#' @family ODE solver creation functions
#' @export
rk45 <- function(abs_tol = 1e-6, rel_tol = 1e-6, max_num_steps = 1e6) {
  AdaptiveOdeSolver$new(
    name = "rk45",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' Create a BDF solver (adaptive step size)
#'
#' @param abs_tol Absolute tolerance.
#' @param rel_tol Relative tolerance.
#' @param max_num_steps Maximum number of steps.
#' @family ODE solver creation functions
#' @export
bdf <- function(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e9) {
  AdaptiveOdeSolver$new(
    name = "bdf",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' Create an Adams solver (adaptive step size)
#'
#' @param abs_tol Absolute tolerance.
#' @param rel_tol Relative tolerance.
#' @param max_num_steps Maximum number of steps.
#' @family ODE solver creation functions
#' @export
adams <- function(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e9) {
  AdaptiveOdeSolver$new(
    name = "adams",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' Create a Cash-Karp solver (adaptive step size)
#'
#' @param abs_tol Absolute tolerance.
#' @param rel_tol Relative tolerance.
#' @param max_num_steps Maximum number of steps.
#' @family ODE solver creation functions
#' @export
ckrk <- function(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e9) {
  AdaptiveOdeSolver$new(
    name = "ckrk",
    abs_tol = abs_tol,
    rel_tol = rel_tol,
    max_num_steps = max_num_steps
  )
}

#' Create an RK4 solver (fixed number of steps)
#'
#' @param num_steps Number of steps per timepoint interval.
#' @family ODE solver creation functions
#' @export
rk4 <- function(num_steps = 1) {
  FixedNumStepsOdeSolver$new(name = "rk4", num_steps = num_steps)
}

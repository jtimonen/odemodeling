#' An ODE solver (an abstract base class)
#'
#' @field name name of the solver
OdeSolver <- R6::R6Class("OdeSolver",
  public = list(
    name = NULL,

    #' @description
    #' `OdeSolver` is an abstract class that can't be initialized.
    initialize = function() {
      stop("OdeSolver is an abstract class that can't be initialized.")
    }
  )
)

#' An ODE solver with adaptive step size
#'
#' @field abs_tol absolute tolerance
#' @field rel_tol relative tolerance
#' @field max_num_steps maximum number of steps
AdaptiveOdeSolver <- R6::R6Class("AdaptiveOdeSolver",
  inherit = OdeSolver,
  public = list(
    abs_tol = NULL,
    rel_tol = NULL,
    max_num_steps = NULL,

    #' @description
    #' Initialize
    #'
    #' @param name solver name
    #' @param abs_tol absolute tolerance
    #' @param rel_tol relative tolerance
    #' @param max_num_steps maximum number of steps
    initialize = function(name, abs_tol, rel_tol, max_num_steps) {
      checkmate::assert_number(abs_tol)
      checkmate::assert_number(rel_tol)
      checkmate::assert_number(max_num_steps)
      checkmate::assert_integerish(max_num_steps)
      self$name <- name
      self$abs_tol <- abs_tol
      self$rel_tol <- rel_tol
      self$max_num_steps <- max_num_steps
    }
  )
)

#' An ODE solver with fixed number of steps
#'
#' @field num_steps maximum number of steps per time point interval
FixedNumStepsOdeSolver <- R6::R6Class("FixedNumStepsOdeSolver",
  inherit = OdeSolver,
  public = list(
    num_steps = NULL,

    #' @description
    #' Initialize
    #' @param name solver name
    #' @param num_steps number of steps per time point interval
    initialize = function(name, num_steps) {
      checkmate::assert_number(num_steps)
      checkmate::assert_integerish(num_steps)
      self$name <- name
      self$num_steps <- num_steps
    }
  )
)

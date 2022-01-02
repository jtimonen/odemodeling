#' An ODE solver (an abstract base class)
#'
#' @field name name of the solver
#' @field stan_number integer that indexes the solver in the Stan code
OdeSolver <- R6::R6Class("OdeSolver",
  public = list(
    name = NULL,
    stan_number = NULL,

    #' @description
    #' `OdeSolver` is an abstract class that can't be initialized.
    initialize = function() {
      stop("OdeSolver is an abstract class that can't be initialized.")
    },

    #' @description
    #' Create Stan data fields.
    #' @return A list.
    standata = function() {
      stop("standata() must be overridden by inheriting class!")
    },

    #' @description
    #' String description of the solver.
    #' @return A string.
    to_string = function() {
      stop("to_string() must be overridden by inheriting class!")
    },

    #' @description
    #' Print info about the object.
    print = function() {
      cat(self$to_string())
      invisible(self)
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
      checkmate::assert_number(abs_tol, lower = 0)
      checkmate::assert_number(rel_tol, lower = 0)
      checkmate::assert_number(max_num_steps)
      MAX_INT <- 2^31 - 1 # max int Stan can handle
      checkmate::assert_integerish(max_num_steps, lower = 1, upper = MAX_INT)
      self$name <- name
      self$abs_tol <- abs_tol
      self$rel_tol <- rel_tol
      self$max_num_steps <- max_num_steps
      self$stan_number <- solver_to_num(name)
    },

    #' @description
    #' Create Stan data fields.
    #' @return A list.
    standata = function() {
      list(
        solver = self$stan_number,
        abs_tol = self$abs_tol,
        rel_tol = self$rel_tol,
        max_num_steps = self$max_num_steps,
        num_steps = 1 # dummy
      )
    },

    #' @description
    #' String description of the solver.
    #' @return A string.
    to_string = function() {
      paste0(
        self$name, "(abs_tol=", number_string(self$abs_tol), ", ",
        "rel_tol=", number_string(self$rel_tol), ", ",
        "max_num_steps=", number_string(self$max_num_steps), ")"
      )
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
      checkmate::assert_integerish(num_steps, lower = 1)
      self$name <- name
      self$num_steps <- num_steps
      self$stan_number <- solver_to_num(name)
    },

    #' @description
    #' Create Stan data fields.
    #' @return A list.
    standata = function() {
      list(
        solver = self$stan_number,
        abs_tol = 1, # dummy
        rel_tol = 1, # dummy
        max_num_steps = 1, # dummy
        num_steps = self$num_steps
      )
    },

    #' @description
    #' String description of the solver.
    #' @return A string.
    to_string = function() {
      paste0(
        self$name, "(num_steps=", number_string(self$num_steps), ")"
      )
    }
  )
)

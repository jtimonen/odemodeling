#' An ODE model fit (R6 class)
#'
#' @description
#' The fields `cmdstanr_time`, `cmdstanr_summary`,
#' and `cmdstanr_draws` store the output of `cmdstanr_fit`'s
#' methods `$time()`, `$summary()`, and `$draws()` methods, respectively,
#' in memory in case `cmdstanr_fit` gets corrupted (for example
#' if the CSV files that it reads the data from are destroyed).
#'
#' @export
#' @field model An object of class [OdeModel].
#' @field standata Full 'Stan' input data list.
#' @field cmdstanr_fit a [cmdstanr::CmdStanMCMC] object.
#' @field cmdstanr_time A list containing output of the  `$time()` method
#' of `cmdstanr_fit`.
#' @field cmdstanr_summary A tibble containing output of the `$summary()`
#' method of `cmdstanr_fit`.
#' @field cmdstanr_draws A [posterior::draws_array] object containing the
#' output of the `$draws()` method of `cmdstanr_fit`.
#' @field cmdstanr_metadata A list containing output of the `$metadata()`
#' method of `cmdstanr_fit`.
#' @field setup_time Time it took to call `$initialize()` when the
#' [OdeModelFit] object was created (in seconds).
OdeModelFit <- R6::R6Class("OdeModelFit", list(
  model = NULL,
  standata = NULL,
  cmdstanr_fit = NULL,
  cmdstanr_time = NULL,
  cmdstanr_summary = NULL,
  cmdstanr_draws = NULL,
  cmdstanr_metadata = NULL,
  setup_time = NULL,

  #' @description
  #' Create an [OdeModelFit] object.
  #'
  #' @param model An object of class [OdeModel] (will be deepcopied).
  #' @param cmdstanr_fit A [cmdstanr::CmdStanMCMC] object (will be deepcopied).
  #' @param standata Full 'Stan' input data list.
  initialize = function(model, cmdstanr_fit, standata) {
    start_time <- Sys.time()
    checkmate::assert_class(model, "OdeModel")
    checkmate::assert_class(cmdstanr_fit, "CmdStanMCMC")
    self$model <- model$clone(deep = TRUE)
    sf <- cmdstanr_fit$clone(deep = TRUE)
    self$cmdstanr_fit <- sf
    self$standata <- standata
    self$cmdstanr_time <- sf$time()
    self$cmdstanr_summary <- sf$summary()
    self$cmdstanr_draws <- sf$draws()
    self$cmdstanr_metadata <- sf$metadata()
    end_time <- Sys.time()
    self$setup_time <- as.numeric(end_time - start_time)
  },

  #' @description
  #' Get time information.
  #' @return A list.
  time = function() {
    self$cmdstanr_time
  },

  #' @description
  #' Get draws (parameters and generated quantities).
  #' @param variable Name of variable.
  #' @param iteration Index of iteration.
  #' @return A [posterior::draws_array] object.
  draws = function(variable = NULL, iteration = NULL) {
    posterior::subset_draws(self$cmdstanr_draws,
      variable = variable,
      iteration = iteration
    )
  },

  #' @description
  #' Get summary
  #' @return A `tibble`.
  summary = function() {
    self$cmdstanr_summary
  },

  #' @description
  #' Print information about the fit
  print = function() {
    cat("An object of class OdeModelFit. Type ?OdeModelFit for help. Summary:",
      "\n",
      sep = ""
    )
    print(self$summary())
    invisible(self)
  },

  #' @description
  #' Get size of the draws object in Mb.
  #' @return A string.
  draws_size = function() {
    format(object.size(self$draws), "Mb")
  },

  #' @description
  #' Get used 'CmdStan' version.
  #' @return A string.
  cmdstan_version = function() {
    md <- self$cmdstanr_metadata
    paste(md$stan_version_major, md$stan_version_minor,
      md$stan_version_patch,
      sep = "."
    )
  },

  #' @description
  #' Get used 'CmdStan' rng seed.
  #' @return A string.
  cmdstan_seed = function() {
    md <- self$cmdstanr_metadata
    md$seed
  },

  #' @description
  #' Get used 'CmdStan' init argument.
  #' @return A string.
  cmdstan_init = function() {
    md <- self$cmdstanr_metadata
    md$init
  },

  #' @description
  #' Extract the ODE solutions using each parameter draw, in an
  #' unflattened format.
  #' @return Equal to `self$extract_unflattened(variable = "y_sol")]`.
  extract_odesol = function() {
    self$extract_unflattened(variable = "y_sol")
  },

  #' @description
  #' Extract the dimensions of the ODE solution variable.
  #' @return A numeric vector of length 2, where first element is the
  #' number of time points and second element is the ODE system dimension.
  dim_odesol = function() {
    a <- self$dim(variable = "y_sol")
    if (length(a) != 2) {
      stop("Something went wrong in 'dim_odesol'. Please report a bug.")
    }
    return(a)
  },

  #' @description
  #' Extract array variable draws so that the array is unflattened.
  #'
  #' @param variable Name of variable.
  #' @return A base \R array of dimension `c(num_draws, ...)` where `num_draws`
  #' is the total number of draws and `...` is the 'Stan' variable dimension,
  #' obtained as `self$dim(variable)`.
  extract_unflattened = function(variable) {
    draws <- self$draws(variable = variable)
    if (variable == "y_sol") {
      stanvar_dim <- self$dim_odesol()
    } else {
      stanvar_dim <- self$dim(variable = variable)
    }
    A <- as.matrix(posterior::as_draws_matrix(d)) # to base R matrix
    num_draws <- dim(A)[1]
    array(data = A, dim = c(num_draws, stanvar_dim))
  },

  #' @description
  #' Get dimensions of a variable.
  #'
  #' @param variable Name of variable.
  #' @return A numeric vector, which is the 'Stan' variable dimension,
  #' obtained as `metadata$stan_variable_dims[[variable]]`, where
  #' `metadata` is the metadata of the [cmdstanr::CmdStanMCMC] object.
  dim = function(variable) {
    dims <- self$cmdstanr_metadata$stan_variable_dims
    dims[[variable]]
  }
))

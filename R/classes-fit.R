# OdeModelMCMC ------------------------------------------------------------

#' An ODE model MCMC fit (R6 class)
#'
#' @description Used for holding the output of the `$sample()` method  of the
#' [OdeModel] class. Users are not meant to instantiate
#' objects of this class directly.
#' @export
#' @family model fit classes
#' @seealso For more useful methods, see the methods inherited from
#' [OdeModelFit].
OdeModelMCMC <- R6::R6Class("OdeModelMCMC",
  inherit = OdeModelFit,
  public = list(

    #' @description
    #' Print information about the object.
    print = function() {
      cat(class_info("OdeModelMCMC"), "\n")
      cat(self$info())
      invisible(self)
    },

    #' @description
    #' Get used 'CmdStan' init argument.
    cmdstan_init = function() {
      md <- self$cmdstanr_metadata
      md$init
    },

    #' @description
    #' Simulate ODE solutions (and other possible generated quantities
    #' using) the model and fitted params. If any
    #' of the arguments are `NULL` (default), they are replaced with ones saved
    #' in the [OdeModelMCMC] object.
    #'
    #' @param t0 Initial time.
    #' @param t Vector of time points.
    #' @param data Additional data.
    #' @param solver ODE solver.
    #' @param fitted_params Equal to the `fitted_params` argument  of the
    #' `$generate_quantities()` method of the underlying
    #' [cmdstanr::CmdStanModel] object. If this is `NULL` (default),
    #' parameter draws of the [OdeModelFit] object are used.
    #' @param ... Arguments passed to the `$generate_quantities()` method of the
    #' underlying [cmdstanr::CmdStanModel] object.
    #' @return An object of class [OdeModelGQ].
    simulate = function(t0 = NULL,
                        t = NULL,
                        data = NULL,
                        solver = NULL,
                        fitted_params = NULL,
                        ...) {

      # Handle input
      t0 <- replace_if_null(t0, self$t0)
      t <- replace_if_null(t, self$t)
      solver <- replace_if_null(solver, self$solver)
      data <- replace_if_null(data, self$data)
      fitted_params <- replace_if_null(fitted_params, self$draws())

      # Full Stan data
      model <- self$model
      sd <- create_standata(model, t0, t, solver)
      full_data <- c(sd, data)

      # Ru Stan
      cmdstanr_gq <- model$stanmodel$generate_quantities(
        fitted_params = fitted_params,
        data = full_data,
        sig_figs = model$sig_figs, ...
      )

      # Return
      OdeModelGQ$new(
        model = model,
        t0 = t0,
        t = t,
        solver = solver,
        data = data,
        cmdstanr_fit = cmdstanr_gq
      )
    }
  )
)

# OdeModelGQ --------------------------------------------------------------

#' An ODE model GQ fit (R6 class)
#'
#' @description Used for holding the output of the `$simulate()`
#' method of the [OdeModelMCMC] class. Users are not meant to instantiate
#' objects of this class directly.
#' @export
#' @family model fit classes
#' @seealso For more useful methods, see the methods inherited from
#' [OdeModelFit].
OdeModelGQ <- R6::R6Class("OdeModelGQ",
  inherit = OdeModelFit,
  public = list(

    #' @description
    #' Print information about the object.
    print = function() {
      cat(class_info("OdeModelGQ"), "\n")
      cat(self$info())
      invisible(self)
    }
  )
)

# OdeModelFit -------------------------------------------------------------

#' An ODE model fit (R6 class)
#'
#' @description
#' The fields `cmdstanr_time`, `cmdstanr_summary`,
#' and `cmdstanr_draws` store the output of `cmdstanr_fit`'s
#' methods `$time()`, `$summary()`, and `$draws()` methods, respectively,
#' in memory in case `cmdstanr_fit` gets corrupted (for example
#' if the CSV files that it reads the data from are destroyed).
#'
#' @field model An object of class [OdeModel].
#' @field t0 Used initial time.
#' @field t Used time points.
#' @field solver Used solver.
#' @field data Given additional data.
#' @field cmdstanr_fit A [cmdstanr::CmdStanMCMC] or [cmdstanr::CmdStanGQ]
#' object.
#' @field cmdstanr_time A list containing output of the  `$time()` method
#' of `cmdstanr_fit`.
#' @field cmdstanr_summary A tibble containing output of the `$summary()`
#' method of `cmdstanr_fit`.
#' @field cmdstanr_draws A [posterior::draws_array] object containing the
#' output of the `$draws()` method of `cmdstanr_fit`.
#' @field cmdstanr_metadata A list containing output of the `$metadata()`
#' method of `cmdstanr_fit`.
#' @field cmdstanr_output A list containing output of the `$output()`
#' method of `cmdstanr_fit`.
#' @field setup_time Time it took to call `$initialize()` when the
#' [OdeModelFit] object was created (in seconds).
#' @family model fit classes
OdeModelFit <- R6::R6Class("OdeModelFit", list(
  model = NULL,
  t0 = NULL,
  t = NULL,
  solver = NULL,
  data = NULL,
  cmdstanr_fit = NULL,
  cmdstanr_time = NULL,
  cmdstanr_summary = NULL,
  cmdstanr_draws = NULL,
  cmdstanr_metadata = NULL,
  setup_time = NULL,
  cmdstanr_output = NULL,

  #' @description
  #' Create an [OdeModelFit] object.
  #'
  #' @param model An object of class [OdeModel] (will be deepcopied).
  #' @param t0 Used initial time.
  #' @param t Used time points.
  #' @param solver Used solver. An object of class [OdeSolver].
  #' @param data Given additional data.
  #' @param cmdstanr_fit A [cmdstanr::CmdStanMCMC] or [cmdstanr::CmdStanGQ]
  #' object (will be deepcopied).
  initialize = function(model, t0, t, solver, data, cmdstanr_fit) {
    start_time <- Sys.time()
    checkmate::assert_class(model, "OdeModel")
    checkmate::assert_class(solver, "OdeSolver")
    checkmate::assert_multi_class(cmdstanr_fit, c("CmdStanMCMC", "CmdStanGQ"))
    self$model <- model$clone(deep = TRUE)
    sf <- cmdstanr_fit$clone(deep = TRUE)
    self$t0 <- t0
    self$t <- t
    self$solver <- solver
    self$data <- data
    self$cmdstanr_fit <- sf
    self$cmdstanr_time <- sf$time()
    self$cmdstanr_summary <- sf$summary()
    self$cmdstanr_draws <- sf$draws()
    self$cmdstanr_metadata <- sf$metadata()
    self$cmdstanr_output <- sf$output()
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
  #' Get various information.
  #' @return A string.
  info = function() {
    tt <- self$time()$total
    s1 <- number_string(self$nchains())
    s2 <- number_string(self$niterations())
    s3 <- number_string(round(tt, 3))
    s4 <- self$solver$to_string()
    str <- paste0(" * Number of chains: ", s1)
    str <- paste0(str, "\n * Number of iterations: ", s2)
    str <- paste0(str, "\n * Total time: ", s3, " seconds.")
    str <- paste0(str, "\n * Used solver: ", s4)
    return(str)
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
  #' Get number of post-warmup iterations per MCMC chain.
  niterations = function() {
    posterior::niterations(self$cmdstanr_draws)
  },

  #' @description
  #' Get number of MCMC chains.
  nchains = function() {
    posterior::nchains(self$cmdstanr_draws)
  },

  #' @description
  #' Get total number of post-warmup draws.
  ndraws = function() {
    self$nchains() * self$niterations()
  },

  #' @description
  #' Get size of the draws object in Mb.
  #' @return A string.
  draws_size = function() {
    format(object.size(self$draws), "Mb")
  },

  #' @description
  #' Get used 'CmdStan' rng seed.
  cmdstan_seed = function() {
    md <- self$cmdstanr_metadata
    md$seed
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
  #' Get timepoints where the model was fitted.
  #'
  #' @return A numeric vector.
  get_t = function() {
    self$t
  },

  #' @description
  #' Get used initial time point t0.
  #'
  #' @return A numeric value.
  get_t0 = function() {
    self$t0
  },

  #' @description
  #' Get dimensions of a variable.
  #'
  #' @param variable Name of variable.
  #' @return A numeric vector, which is the 'Stan' variable dimension,
  #' obtained as `metadata$stan_variable_dims[[variable]]`, where
  #' `metadata` is the metadata of the [cmdstanr::CmdStanMCMC] or
  #' [cmdstanr::CmdStanGQ] object.
  dim = function(variable) {
    dims <- self$cmdstanr_metadata$stan_variable_dims
    dims[[variable]]
  },

  #' @description
  #' Extract the dimensions of the ODE solution variable.
  #' @return A numeric vector of length 2, where first element is the
  #' number of time points and second element is the ODE system dimension.
  dim_odesol = function() {
    a <- self$dim(variable = "y_sol_gq")
    internal_assert_len(a, 2, "dim_odesol")
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
    if (variable == "y_sol_gq") {
      stanvar_dim <- self$dim_odesol()
    } else {
      stanvar_dim <- self$dim(variable = variable)
    }
    A <- as.matrix(posterior::as_draws_matrix(draws)) # to base R matrix
    num_draws <- dim(A)[1]
    array(data = A, dim = c(num_draws, stanvar_dim))
  },

  #' @description
  #' Extract the ODE solutions using each parameter draw, in an
  #' unflattened base \R array format.
  #' @return A base \R array of dimension `c(num_draws, N, D)` where
  #' `num_draws` is the total number of draws and `N` is the number of
  #' time points and `D` is the number of ODE system dimensions.
  extract_odesol = function() {
    arr <- self$extract_unflattened(variable = "y_sol_gq")
    internal_assert_len(dim(arr), 3, "extract_odesol_unflattened")
    return(arr)
  },

  #' @description
  #' Extract the log likelihood using each parameter draw.
  #' @return A [posterior::draws_array].
  loglik = function() {
    hl <- self$model$has_likelihood
    if (!hl) {
      stop("The fitted model has no likelihood function specified.")
    }
    self$draws("log_lik_gq")
  },

  #' @description
  #' Extract the ODE solutions using each parameter draw, in a
  #' flattened data frame format that is easy to pass as data
  #' to for example [ggplot2::ggplot()].
  #' @param draw_inds If this is not `NULL`, returns ode solutions
  #' corresponding only to given draws.
  #' @return A `data.frame`.
  extract_odesol_df = function(draw_inds = NULL) {
    arr <- self$extract_odesol()
    num_draws <- dim(arr)[1]
    N <- dim(arr)[2]
    D <- dim(arr)[3]
    ysol <- as.vector(arr)
    idx <- as.factor(rep(c(1:num_draws), N * D))
    t <- rep(rep(self$get_t(), D), each = num_draws)
    YDIM <- paste0("y", c(1:D))
    ydim <- as.factor(rep(rep(YDIM, each = N), each = num_draws))
    df <- data.frame(idx, t, ydim, ysol)
    if (!is.null(draw_inds)) {
      inds <- which(df$idx %in% as.character(draw_inds))
      df <- df[inds, ]
    }
    rownames(df) <- NULL
    return(df)
  },

  #' @description
  #' A quick way to plot the ODE solutions.
  #'
  #' @param draw_inds If this numeric and positive, plots ODE solutions
  #' corresponding only to given draws. If this is `0`, all draws are plotted.
  #' If this is `NULL`, a random subset of at most 100 draws are plotted.
  #' @param linealpha line alpha
  #' @param linecolor line color
  #' @return A `ggplot` object.
  plot_odesol = function(draw_inds = NULL, linealpha = 0.75,
                         linecolor = "firebrick") {
    num_draws <- self$ndraws()
    if (!is.null(draw_inds)) {
      if (length(draw_inds) == 1) {
        if (draw_inds == 0) {
          draw_inds <- 1:num_draws
        }
      }
    }
    if (num_draws >= 100 && is.null(draw_inds)) {
      message(
        "Randomly selecting a subset of 100 draws to plot. ",
        "Set draw_inds=0 to plot all ", num_draws,
        " draws."
      )
      draw_inds <- sample.int(num_draws, 100, replace = FALSE)
    }
    df <- self$extract_odesol_df(draw_inds = draw_inds)
    wf <- as.formula(". ~ ydim")
    aesth <- aes_string(x = "t", y = "ysol", group = "idx")
    ggplot(df, aesth) +
      geom_line(alpha = linealpha, color = linecolor) +
      facet_wrap(wf) +
      ylab("ODE solution")
  }
))

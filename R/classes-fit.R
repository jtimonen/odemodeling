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
    #' using) the model and fitted params. This If any
    #' of the arguments are `NULL` (default), they are replaced with ones saved
    #' in the [OdeModelFit] object.
    #'
    #' @param t0 Initial time.
    #' @param t Vector of time points.
    #' @param data Additional data.
    #' @param solver ODE solver.
    #' @param fitted_params Will be passed as the `fitted_params` argument
    #' to the `$generate_quantities()` method of the underlying
    #' [cmdstanr::CmdStanModel] object. If this is `NULL` (default),
    #' parameter draws of the [OdeModelFit] object are used.
    #' @param ... Arguments passed to the `$generate_quantities()` method of
    #' the underlying [cmdstanr::CmdStanModel] object.
    #' @return An object of class [OdeModelGQ].
    gqs = function(t0 = NULL,
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
      param_names <- self$model$stanmodel$param_names()
      existing_pars <- self$draws(variable = param_names)
      fitted_params <- replace_if_null(fitted_params, existing_pars)

      self$model$gqs(
        t0 = t0,
        t = t,
        data = data,
        solver = solver,
        params = fitted_params,
        ...
      )
    },

    #' @description
    #' Study reliability of results by running standalone generated
    #' quantities using more accurate ODE solver configurations
    #'
    #' @param solvers List of ODE solvers (possibly the same solver with
    #' more accurate configurations). See \code{\link{odesolvers_lists}} for
    #' creating this.
    #' @param savedir Directory where results are saved.
    #' @param basename Base name for saved files.
    #' @param force If this is `TRUE`, the procedure is continued for all
    #' given solvers even if it is found that some early stage that results
    #' are not reliable.
    #' @param ... Additional arguments passed to the `$generate_quantities()`
    #' method of the underlying [cmdstanr::CmdStanModel] object.
    #' @return A named list.
    reliability = function(solvers,
                           savedir = "results",
                           basename = "odegq",
                           force = FALSE,
                           ...) {
      if (!force) {
        stop("Set force=TRUE if you want to call this!")
      }
      create_dir_if_not_exist(savedir)
      checkmate::assert_list(solvers, "OdeSolver")
      L <- length(solvers)
      IS <- list()
      FN <- c()
      GT <- rep(0.0, L)
      metrics <- NULL

      # Base configuration
      cat("Running GQ using sampling-time configuration.\n")
      base_gq <- self$gqs(...) # everything will be computed against this

      # Other configurations
      for (j in seq_len(L)) {
        solver <- solvers[[j]]
        conf_str <- solver$to_string()
        cat("==============================================================\n")
        cat(" (", number_string(j), ") Running GQ with: ",
          conf_str, "\n",
          sep = ""
        )
        fn <- file.path(savedir, paste0(basename, "_", j, ".rds"))
        gq <- self$gqs(solver = solver, ...)
        cat("Saving result object to ", fn, "\n", sep = "")
        saveRDS(gq, file = fn)
        FN <- c(FN, fn)
        GT[j] <- gq$time()$total
        rel_met <- compute_reliability_metrics(base_gq, gq)
        metrics <- rbind(metrics, rel_met)
      }
      metrics <- data.frame(metrics)
      colnames(metrics) <- names(rel_met)
      rownames(metrics) <- NULL

      # Return
      list(
        times = GT, solvers = solvers, files = FN, metrics = metrics,
        base_gq = base_gq
      )
    }
  )
)


# OdeModelGQ --------------------------------------------------------------

#' An ODE model GQ fit (R6 class)
#'
#' @description Used for holding the output of the `$gqs()`
#' method of the [OdeModel] and [OdeModelMCMC] class. Users are not meant to
#' instantiate objects of this class directly.
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
  #' Get time points where the model was fitted.
  #' @param include_t0 Should the initial time point be included?
  #' @return A numeric vector of length `N`. If `include_t0` is `TRUE`, length
  #' will be `N+1`.
  get_t = function(include_t0 = FALSE) {
    t <- self$t
    if (include_t0) {
      t0 <- self$get_t0()
      t <- c(t0, t)
    }
    t
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
  #' @param include_y0 Should the initial state be included?
  #' @return A base \R array of dimension `c(num_draws, N, D)` where
  #' `num_draws` is the total number of draws and `N` is the number of
  #' time points and `D` is the number of ODE system dimensions. If
  #' `include_y0` is `TRUE`, then the `N` dimension grows to `N+1`.
  extract_odesol = function(include_y0 = FALSE) {
    arr <- self$extract_unflattened(variable = "y_sol_gq")
    internal_assert_len(dim(arr), 3, "extract_odesol_unflattened")
    if (include_y0) {
      y0 <- self$extract_y0()
      dims <- dim(y0)
      y0 <- array(y0, dim = c(dims[1], 1, dims[2]))
      arr <- abind::abind(y0, arr, along = 2)
    }
    arr
  },

  #' @description
  #' Extract the ODE initial states using each parameter draw, in a
  #' base \R array format.
  #' @return A base \R array of dimension `c(num_draws, D)` where
  #' `num_draws` is the total number of draws and `D` is the number of ODE
  #' system dimensions.
  extract_y0 = function() {
    arr <- self$extract_unflattened(variable = "y0_gq")
    return(arr)
  },

  #' @description
  #' Extract quantiles of the ODE solutions in a base \R array
  #' format.
  #' @param p Percentile. A number between 0 and 1. For example `p=0.5`
  #' corresponds to median.
  #' @param include_y0 Should the initial state be included?
  #' @return A base \R array of dimension `c(N, D)` where `N` is the number of
  #' time points and `D` is the number of ODE system dimensions. If
  #' `include_y0` is `TRUE`, then the `N` dimension grows to `N+1`.
  extract_odesol_quantile = function(p, include_y0 = FALSE) {
    checkmate::assert_number(p, lower = 0, upper = 1)
    get_q <- function(x) {
      stats::quantile(x, probs = p)
    }
    ysol <- self$extract_odesol(include_y0 = include_y0)
    # ysol has shape num_draws x num_timepoints x num_dims
    apply(ysol, c(2, 3), get_q)
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
  #' @param include_y0 Should the initial state be included?
  #' @param ydim_names Names of the ODE dimensioins. If `NULL`, these
  #' are automatically set as `"y1"`, `"y2"`, etc.
  #' @return A `data.frame`.
  extract_odesol_df = function(draw_inds = NULL,
                               include_y0 = FALSE,
                               ydim_names = NULL) {
    arr <- self$extract_odesol(include_y0 = include_y0)
    num_draws <- dim(arr)[1]
    N <- dim(arr)[2]
    D <- dim(arr)[3]
    ysol <- as.vector(arr)
    idx <- as.factor(rep(c(1:num_draws), N * D))
    t <- self$get_t(include_t0 = include_y0)
    t <- rep(rep(t, D), each = num_draws)
    YDIM <- create_ydim_names(ydim_names, D)
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
  #' Extract (quantiles of) the marginal distribution of ODE solutions in a
  #' data frame format that is easy to pass as data
  #' to for example [ggplot2::ggplot()].
  #' @param probs The percentile values. A numeric vector where all values
  #' are between 0 and 1.
  #' @param include_y0 Should the initial state be included?
  #' @param ydim_names Names of the ODE dimensioins. If `NULL`, these
  #' are automatically set as `"y1"`, `"y2"`, etc.
  #' @return A `data.frame`.
  extract_odesol_df_dist = function(probs = c(0.1, 0.5, 0.9),
                                    include_y0 = FALSE,
                                    ydim_names = NULL) {
    checkmate::assert_numeric(probs, lower = 0, upper = 1, min.len = 1)
    J <- length(probs)
    dims <- self$dim_odesol()
    D <- dims[2]
    t <- self$get_t(include_t0 = include_y0)
    N <- length(t)
    t <- rep(t, D)
    df_quant <- NULL
    for (j in seq_len(J)) {
      a <- self$extract_odesol_quantile(p = probs[j], include_y0 = include_y0)
      df_quant <- cbind(df_quant, as.vector(a))
    }

    YDIM <- create_ydim_names(ydim_names, D)
    ydim <- rep(YDIM, each = N)
    df <- data.frame(t, as.factor(ydim))
    df <- cbind(df, df_quant)
    colnames(df) <- c("t", "ydim", probs)
    return(df)
  },

  #' @description
  #' A quick way to plot the ODE solutions.
  #'
  #' @param draw_inds If this numeric and positive, plots ODE solutions
  #' corresponding only to given draws. If this is `0`, all draws are plotted.
  #' If this is `NULL`, a random subset of at most 100 draws are plotted.
  #' @param alpha line alpha
  #' @param color line color
  #' @param ... other arguments passed to `extract_odesol_df`
  #' @return A `ggplot` object.
  plot_odesol = function(draw_inds = NULL, alpha = 0.75,
                         color = "firebrick", ...) {
    linealpha <- alpha
    linecolor <- color
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
    df <- self$extract_odesol_df(draw_inds = draw_inds, ...)
    wf <- as.formula(". ~ ydim")
    aesth <- aes_string(x = "t", y = "ysol", group = "idx")
    ggplot(df, aesth) +
      geom_line(alpha = linealpha, color = linecolor) +
      facet_wrap(wf) +
      ylab("ODE solution")
  },

  #' @description
  #' A quick way to plot the marginal distribution of ODE solutions at
  #' each time point.
  #'
  #' @param p Which percentage central interval to plot?
  #' @param alpha fill alpha
  #' @param color line color
  #' @param fill_color fill color
  #' @param ... other arguments passed to `extract_odesol_df_dist`
  #' @return A `ggplot` object.
  plot_odesol_dist = function(p = 0.8,
                              alpha = 0.75, color = "firebrick",
                              fill_color = "firebrick", ...) {
    x <- 1 - p
    msg <- paste0("plotting medians and ", 100 * p, "% central intervals")
    message(msg)
    probs <- c(x / 2, 0.5, 1 - x / 2)
    df <- self$extract_odesol_df_dist(probs, ...)
    colnames(df)[3:5] <- c("lower", "median", "upper")
    wf <- as.formula(". ~ ydim")
    aesth <- aes_string(
      x = "t", y = "median", ymin = "lower", ymax = "upper"
    )
    ggplot(df, aesth) +
      geom_line(alpha = 1, color = color) +
      geom_ribbon(fill = fill_color, alpha = alpha) +
      facet_wrap(wf) +
      ylab("ODE solution")
  }
))

#' An ODE model (R6 class)
#'
#' @export
#' @field prior An object of class `StanModelWithCode`.
#' @field posterior An object of class `StanModelWithCode`.
#' @field odetuner_version of the package used to create the model
#' @field sig_figs Number of significant figures to use everywhere.
#' @param t_dim A `StanDimension` of the time points array.
#' @param ode_dim A `StanDimension` of the ODE system.
OdeModel <- R6::R6Class("OdeModel", list(
  prior = NULL,
  posterior = NULL,
  odetuner_version = NULL,
  sig_figs = NULL,
  t_dim = NULL,
  ode_dim = NULL,

  #' @description
  #' Create an `OdeModel` object.
  #'
  #' @param prior An object of class `StanModelWithCode`.
  #' @param posterior An object of class `StanModelWithCode`.
  #' @param compile Should the models be compiled.
  #' @param sig_figs Number of significant figures to use in all Stan i/o.
  #' @param t_dim Time points vector dimension variable.
  #' @param ode_dim ODE system dimension variable.
  initialize = function(prior, posterior, sig_figs, t_dim, ode_dim) {
    checkmate::assert_integerish(sig_figs, lower = 3)
    checkmate::assert_class(t_dim, "StanDimension")
    checkmate::assert_class(ode_dim, "StanDimension")
    self$prior <- prior
    self$posterior <- posterior
    self$odetuner_version <- pkg_version("odetuner")
    self$sig_figs <- sig_figs
    self$t_dim <- t_dim
    self$ode_dim <- ode_dim
  },

  #' @description
  #' Check that the Stan models have been initialized correctly
  assert_files_exist = function() {
    e1 <- self$prior$stan_file_exists()
    e2 <- self$prior$stan_file_exists()
    if (!(e1 && e2)) {
      stop("At least one Stan model file doesn't exist. Please call $reinit().")
    }
    TRUE
  },

  #' @description
  #' (Re)initialize the Stan models
  reinit = function() {
    self$prior$reinit()
    self$posterior$reinit()
    invisible(self)
  },

  #' @description
  #' Print information about the model
  print = function() {
    cat("An object of class OdeModel. See ?OdeModel for help. \n", sep = "")
    cat(" * ODE dimension: ")
    self$ode_dim$print()
    cat(" * Time points array dimension: ")
    self$t_dim$print()
    sf <- self$sig_figs
    cat(" * Number of significant figures in csv files: ")
    cat_number(sf)
    cat("\n")
    invisible(self)
  },

  #' @description
  #' Sample parameters
  #' @param t0 Initial time point.
  #' @param t Vector of time points.
  #' @param solver ODE solver name.
  #' @param solver_args List of ODE solver control arguments.
  #' @param data Other needed data as a list.
  #' @param prior_only Sample only from the prior? If this is true, ODE
  #' solves are done only in generated quantities, and there is no need to
  #' compute gradients for the solutions.
  #' @param ... Arguments passed to the `$sample()` method of the
  #' underlying `CmdStanModel` objects.
  sample = function(t0,
                    t,
                    solver = "rk45",
                    solver_args = NULL,
                    data = list(),
                    prior_only = FALSE,
                    ...) {
    checkmate::assertNumber(t0)
    checkmate::assert_vector(t)
    checkmate::assert_numeric(t)
    checkmate::assert_string(solver)
    if (is.null(solver_args)) {
      solver_num <- solver_to_num(solver)
      solver_args <- default_solver_args(solver_num)
    }
    solver_num <- solver_to_num(solver)
    if (solver_num <= 10) {
      nams <- c("abs_tol", "rel_tol", "max_num_steps")
      checkmate::assert_list(solver_args)
      checkmate::assert_set_equal(names(solver_args), nams)
      dummy <- list(num_steps = 1)
      solver_args <- c(solver_args, dummy)
    } else {
      checkmate::assert_list(solver_args)
      checkmate::assert_set_equal(names(solver_args), "num_steps")
      dummy <- list(abs_tol = 1, rel_tol = 1, max_num_steps = 1)
      solver_args <- c(solver_args, dummy)
    }
    N <- list(N = length(t))
    names(N) <- self$t_dim$name
    full_data <- c(
      N,
      list(t0 = t0, t = t, solver = solver_num),
      solver_args,
      data
    )
    model <- if (prior_only) self$prior else self$posterior
    model$sample(data = full_data, sig_figs = self$sig_figs, ...)
  },

  #' @description
  #' Sample from parameter posterior using several different ODE tolerances
  #' @param ... Arguments passed to `$sample()`.
  sample_posterior_many = function(res_dir, idx,
                                   tols, max_num_steps, chains = 4, ...) {
    L <- length(tols)
    WT <- matrix(0.0, L, chains)
    ST <- matrix(0.0, L, chains)
    TT <- matrix(0.0, L, chains)
    FN <- c()
    GT <- rep(0.0, L)
    j <- 0
    for (tol_j in tols) {
      cat(" (", j, ") Posterior sampling with tol = ", tol_j, "\n", sep = "")
      j <- j + 1
      fn <- file.path(res_dir, paste0("fit_", idx, "_", j, ".rds"))
      sargs <- list(
        abs_tol = tol_j,
        rel_tol = tol_j,
        max_num_steps = max_num_steps
      )
      post_fit <- self$sample_posterior(sargs,
        chains = chains,
        step_size = self$hmc_initial_step_size
      )
      cat("Saving fit to ", fn, "\n", sep = "")
      post_fit$save_object(fn)
      FN <- c(FN, fn)
      t <- post_fit$time()$chains$total
      gt <- post_fit$time()$total
      GT[j] <- gt
      cat("Grand total time = ", gt, "seconds. \n")
      WT[j, ] <- post_fit$time()$chains$warmup
      ST[j, ] <- post_fit$time()$chains$sampling
      TT[j, ] <- t
    }
    out <- list(
      warmup = WT, sampling = ST, total = TT, grand_total = GT,
      files = FN, tols = tols
    )
    return(out)
  }
))


# A model (R6 class)
StanModelWithCode <- R6::R6Class("StanModelWithCode",
  public = list(
    model = NULL,
    dims = NULL,
    data = NULL,
    tdata = NULL,
    params = NULL,
    tparams = NULL,
    gqs = NULL,
    code = "",
    get_model = function() {
      mod <- self$model
      if (is.null(mod)) {
        stop("Model not initialized. You need to call $reinit().")
      }
      return(mod)
    },
    get_decls = function(type) {
      dnames <- c("dims", "data", "tdata", "params", "tparams", "gqs")
      checkmate::assert_choice(type, dnames)
      self[[type]]
    },
    initialize = function(code, dims, data, tdata, params, tparams, gqs,
                          compile) {
      if (!compile) {
        message(
          "Not compiling the model. You need to call $reinit() before",
          " being able to sample."
        )
      }
      self$code <- code
      self$dims <- dims
      self$data <- data
      self$tdata <- tdata
      self$params <- params
      self$tparams <- tparams
      self$gqs <- gqs
      if (compile) {
        self$model <- stan_model_from_code(code)
      }
    },
    reinit = function() {
      self$model <- stan_model_from_code(self$code)
      invisible(self)
    },
    print = function() {
      cat_stancode(self$code)
      invisible(self)
    },
    stan_file_exists = function() {
      mod <- self$get_model()
      sf <- mod$stan_file()
      if (file.exists(sf)) {
        return(TRUE)
      }
      FALSE
    },
    names_of = function(type) {
      decls <- self$get_decls(type)
      nams <- sapply(decls, get_name)
      if (length(nams) == 0) {
        return(NULL)
      }
      nams
    },
    data_names = function() {
      nam1 <- self$names_of("dims")
      nam2 <- self$names_of("data")
      unique(c(nam1, nam2))
    },
    param_names = function(inc_transformed = FALSE) {
      nam <- self$names_of("params")
      if (inc_transformed) {
        nam <- c(nam, self$names_of("tparams"))
      }
      unique(nam)
    },
    gq_names = function() {
      self$names_of("gqs")
    },
    data_check = function(data) {
      checkmate::assert_list(data)
      needed <- self$data_names()
      given <- names(data)
      for (name in needed) {
        if (!(name %in% given)) {
          stop(paste0(name, " is missing from the data list!"))
        }
      }
      TRUE
    },
    sample = function(data, ...) {
      self$data_check(data)
      mod <- self$get_model()
      mod$sample(data = data, ...)
    },
    generate_quantities = function(data, fitted_params, ...) {
      self$data_check(data)
      mod <- self$get_model()
      mod$model$generate_quantities(
        fitted_params = fitted_params,
        data = data, ...
      )
    }
  )
)

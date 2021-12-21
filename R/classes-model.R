#' An ODE model (R6 class)
#'
#' @export
#' @field prior An object of class `StanModelWithCode`.
#' @field posterior An object of class `StanModelWithCode`.
#' @field odetuner_version of the package used to create the model
OdeModel <- R6::R6Class("OdeModel", list(
  prior = NULL,
  posterior = NULL,
  odetuner_version = NULL,

  #' @description
  #' Create an `OdeModel` object.
  #'
  #' @param prior An object of class `StanModelWithCode`.
  #' @param posterior An object of class `StanModelWithCode`.
  #' @param compile Should the models be compiled.
  initialize = function(prior, posterior) {
    self$prior <- prior
    self$posterior <- posterior
    self$odetuner_version <- pkg_version("odetuner")
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
  },

  #' @description
  #' Print information about the model
  print = function() {
    cat("An object of class OdeModel. See ?OdeModel for help. \n", sep = "")
    cat("\nModel for sampling from prior:\n")
    self$prior$print()
    cat("\nModel for sampling from posterior:\n")
    self$posterior$print()
    invisible(self)
  },

  #' @description
  #' Sample from parameter prior (no ODE solving)
  #' @param t0 initial time point
  #' @param t data time points
  #' @param D ODE system dimension
  #' @param add data list of additional data needed to run the model
  #' @param ... Arguments passed to `$sample()`.
  sample_prior = function(...) {
    self$prior$sample(
      ...
    )
  },

  #' @description
  #' Sample from parameter posterior
  #' @param ... Arguments passed to `$sample()`.
  sample_posterior = function(solver_args, ...) {
    stan_opts <- self$stan_opts
    data <- self$data
    init <- self$init
    sample_posterior(self$stanmodel, data, solver_args, stan_opts,
      init = init, ...
    )
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
      if (is.null(self$mod)) {
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
      needed <- self$needed_inputs()
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

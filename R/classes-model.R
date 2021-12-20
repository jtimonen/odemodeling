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
  sample_prior = function(t0 = 0.0, t = c(1, 2, 3), D = 1, add_data = list(),
                          ...) {
    data <- list(
      do_likelihood = FALSE,
      do_genquant = FALSE,
      t0 = t0,
      t = t,
      D = D,
      solver = 10,
    )

    self$stanmodel$sample(
      data = data,
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
StanModelWithCode <- R6::R6Class("StanModelWithCode", list(
  model = NULL,
  code = "",
  dims = NULL,
  data = NULL,
  tdata = NULL,
  params = NULL,
  tparams = NULL,
  gqs = NULL,
  initialize = function(code, dims, data, tdata, params, tparams, gqs,
                        compile) {
    if (!compile) {
      message(
        "Not compiling the models. You need to call $reinit() before",
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
    if (is.null(self$model)) {
      return(FALSE)
    }
    sf <- self$model$stan_file()
    if (file.exists(sf)) {
      return(TRUE)
    }
    FALSE
  }
))

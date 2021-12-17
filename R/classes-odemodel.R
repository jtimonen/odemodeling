#' An ODE model (R6 class)
#'
#' @export
#' @field name Name of model.
#' @field stanmodel An object of class `CmdStanModel`.
#' @field datasim Can the model simulate data?
#' @field stancode Full 'Stan' code as a string.
OdeModel <- R6::R6Class("OdeModel", list(
  name = NULL,
  datasim = NULL,
  stancode = NULL,
  stanmodel = NULL,

  #' @description
  #' Create an `OdeModel` object.
  #'
  #' @param stancode Full 'Stan' code as a string.
  #' @param datasim Can the model simulate data?
  #' @param compile Should the model be compiled?
  #' @param ... Arguments passed to `cmdstanr::write_stan_file()`.
  initialize = function(stancode, datasim, compile, ...) {
    self$datasim <- datasim
    self$stancode <- stancode
    file <- cmdstanr::write_stan_file(stancode, ...)
    model <- cmdstanr::cmdstan_model(stan_file = file, compile = compile)
    self$name <- model$model_name()
    self$stanmodel <- model
  },

  #' @description
  #' Print information about the model
  print = function() {
    cat("An object of class OdeModel. See ?OdeModel for help. \n", sep = "")
    cat(" - name: ", self$name, "\n", sep = "")
    cat(" - can simulate data: ", self$datasim, "\n", sep = "")
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

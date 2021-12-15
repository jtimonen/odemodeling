#' An ODE model.
#'
#' @param name Name of model.
#' @param posterior An object of class `CmdStanModel`.
#' @param prior An object of class `CmdStanModel`.
#' @param simulator An object of class `CmdStanModel`.
OdeModel <- R6::R6Class("OdeModel", list(
  name = NULL,
  posterior = NULL,
  prior = NULL,
  simulator = NULL,
  stan_opts = NULL,
  initialize = function(name, posterior, prior, simulator) {
    self$name <- name
    self$posterior <- posterior
    self$prior <- prior
    self$simulator <- simulator
  },
  print = function(...) {
    cat("OdeModel: ", self$name, "\n", sep = "")
    invisible(self)
  },
  sample_prior = function(...) {
    stan_opts <- self$stan_opts
    self$prior$sample(
      data = self$data,
      sig_figs = stan_opts$sig_figs,
      seed = stan_opts$seed,
      ...
    )
  },
  sample_posterior = function(solver_args, ...) {
    stan_opts <- self$stan_opts
    data <- self$data
    init <- self$init
    sample_posterior(self$stanmodels$posterior, data, solver_args, stan_opts,
      init = init, ...
    )
  },
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
  },
  plot = function(fit) {
    eval(call(paste0("plot_", self$name), fit, self$data))
  },
  set_data = function(data) {
    self$data <- data
  },
  set_init = function(init) {
    self$init <- init
  },
  set_hmc_initial_step_size = function(step_size) {
    self$hmc_initial_step_size <- step_size
  }
))

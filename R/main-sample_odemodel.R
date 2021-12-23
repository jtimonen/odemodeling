#' Sample parameters  of an ODE model
#'
#' @export
#' @param model An object of class [OdeModel].
#' @param t0 Initial time point.
#' @param t Vector of time points.
#' @param solver ODE solver name.
#' @param solver_conf List of ODE solver configuration arguments.
#' @param data Other needed data as a list.
#' @param prior_only Sample only from the prior? If this is true, ODE
#' solves are done only in generated quantities, and there is no need to
#' compute gradients for the solutions.
#' @param ... Arguments passed to the `$sample()` method of the
#' underlying [cmdstanr::CmdStanModel] objects.
sample_odemodel <- function(model,
                            t0,
                            t,
                            data = list(),
                            solver = "rk45",
                            solver_conf = NULL,
                            prior_only = FALSE,
                            ...) {
  checkmate::assert_class(model, "OdeModel")
  checkmate::assertNumber(t0)
  checkmate::assert_vector(t)
  checkmate::assert_numeric(t)
  checkmate::assert_string(solver)
  if (is.null(solver_conf)) {
    solver_num <- solver_to_num(solver)
    solver_conf <- default_solver_conf(solver_num)
  }
  solver_num <- solver_to_num(solver)
  if (solver_num <= 10) {
    MAX_INT <- 2^31 - 1
    nams <- c("abs_tol", "rel_tol", "max_num_steps")
    checkmate::assert_list(solver_conf)
    checkmate::assert_set_equal(names(solver_conf), nams)
    mns <- solver_conf$max_num_steps
    checkmate::assert_integerish(mns, lower = 1, upper = MAX_INT)
    dummy <- list(num_steps = 1)
    solver_args <- c(solver_conf, dummy)
  } else {
    checkmate::assert_list(solver_conf)
    checkmate::assert_set_equal(names(solver_conf), "num_steps")
    dummy <- list(abs_tol = 1, rel_tol = 1, max_num_steps = 1)
    solver_args <- c(solver_conf, dummy)
  }
  N <- list(N = length(t))
  names(N) <- model$t_dim$name
  full_data <- c(
    N,
    list(t0 = t0, t = t, solver = solver_num),
    solver_args,
    data
  )
  smc <- if (prior_only) model$prior else model$posterior
  smc$sample(data = full_data, sig_figs = model$sig_figs, ...)
}

#' Sample parameters  of an ODE model using many different ODE solver
#' configurations
#'
#' @export
#' @inheritParams sample_odemodel
#' @param solver_confs List of ODE solver configurations. See
#' [create_solver_conf_list()] for creating this.
#' @param savedir Directory where results are saved.
#' @param basename Base name for saved files.
#' @param chains Number of MCMC chains.
sample_odemodel_manyconf <- function(model,
                                     t0,
                                     t,
                                     data = list(),
                                     solver = "rk45",
                                     solver_confs = list(),
                                     prior_only = FALSE,
                                     savedir = "results",
                                     basename = "out",
                                     chains = 4,
                                     ...) {
  if (!dir.exists(savedir)) {
    message("directory '", savedir, "' doesn't exist, creating it")
    dir.create(savedir)
  }
  L <- length(solver_confs)
  WT <- matrix(0.0, L, chains)
  ST <- matrix(0.0, L, chains)
  TT <- matrix(0.0, L, chains)
  FN <- c()
  GT <- rep(0.0, L)
  j <- 0
  for (conf_j in solver_confs) {
    j <- j + 1
    conf_str <- list_to_str(conf_j)
    cat("=================================================================\n")
    cat(" (", j, ") Sampling with configuration: ", conf_str, "\n", sep = "")
    fn <- file.path(savedir, paste0(basename, "_", j, ".rds"))
    fit <- sample_odemodel(
      model =  model,
      t0 = t0,
      t = t,
      data  = data,
      solver = solver,
      solver_conf = conf_j,
      prior_only = prior_only,
      chains = chains,
      ...
    )
    cat("Saving fit to ", fn, "\n", sep = "")
    fit$save_object(fn)
    FN <- c(FN, fn)
    t_total <- fit$time()$chains$total
    gt <- fit$time()$total
    GT[j] <- gt
    WT[j, ] <- fit$time()$chains$warmup
    ST[j, ] <- fit$time()$chains$sampling
    TT[j, ] <- t_total
  }
  times <- list(warmup = WT, sampling = ST, total = TT, grand_total = GT)

  # Return
  list(
    times = times, files = FN, solver = solver, solver_confs = solver_confs
  )
}

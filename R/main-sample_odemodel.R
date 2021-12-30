#' Sample parameters of an ODE model
#'
#' @export
#' @param model An object of class [OdeModel].
#' @param t0 Initial time point.
#' @param t Vector of time points.
#' @param solver ODE solver name.
#' @param solver_conf List of ODE solver configuration arguments.
#' @param data Other needed data as a list.
#' @param ... Arguments passed to the `$sample()` method of the
#' underlying [cmdstanr::CmdStanModel] object.
#' @return An object of class [OdeModelMCMC].
sample_odemodel <- function(model,
                            t0,
                            t,
                            data = list(),
                            solver = "rk45",
                            solver_conf = NULL,
                            ...) {

  # Check and handle input
  full_data <- create_standata(model, t0, t, solver, solver_conf)
  full_data <- c(full_data, data)

  # Actual sampling
  sm <- model$stanmodel
  cmdstanr_mcmc <- sm$sample(data = full_data, sig_figs = model$sig_figs, ...)

  # Return
  OdeModelMCMC$new(
    model = model,
    t0 = t0,
    t = t,
    solver = solver,
    solver_conf = solver_conf,
    data = data,
    cmdstanr_fit = cmdstanr_mcmc
  )
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
                                     savedir = "results",
                                     basename = "odemodelfit",
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
      chains = chains,
      ...
    )
    cat("Saving OdeModelFit to ", fn, "\n", sep = "")
    saveRDS(fit, file = fn)
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

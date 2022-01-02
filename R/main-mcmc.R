#' Sample parameters of an ODE model
#'
#' @export
#' @param model An object of class [OdeModel].
#' @param t0 Initial time point.
#' @param t Vector of time points.
#' @param solver An object of class [OdeSolver].
#' @param data Other needed data as a list.
#' @param ... Arguments passed to the `$sample()` method of the
#' underlying [cmdstanr::CmdStanModel] object.
#' @return An object of class [OdeModelMCMC].
sample_odemodel <- function(model,
                            t0,
                            t,
                            data = list(),
                            solver = rk45(),
                            ...) {

  # Check and handle input
  sd <- create_standata(model, t0, t, solver)
  full_data <- c(sd$other, sd$solver_conf, data)

  # Actual sampling
  sm <- model$stanmodel
  cmdstanr_mcmc <- sm$sample(data = full_data, sig_figs = model$sig_figs, ...)

  # Return
  OdeModelMCMC$new(
    model = model,
    t0 = t0,
    t = t,
    solver = solver,
    data = data,
    cmdstanr_fit = cmdstanr_mcmc
  )
}


#' Sample parameters of an ODE model using many different ODE solver
#' configurations
#'
#' @export
#' @inheritParams sample_odemodel
#' @param solvers List of ODE solvers (possibly the same solver with
#' different configurations). See [create_solver_list()] for creating this.
#' @param savedir Directory where results are saved.
#' @param basename Base name for saved files.
#' @param chains Number of MCMC chains.
#' @return A named list.
sample_odemodel_manyconf <- function(solvers,
                                     model,
                                     t0,
                                     t,
                                     data = list(),
                                     savedir = "results",
                                     basename = "odemodelfit",
                                     chains = 4,
                                     ...) {
  if (!dir.exists(savedir)) {
    message("directory '", savedir, "' doesn't exist, creating it")
    dir.create(savedir)
  }
  checkmate::assert_list(solvers, "OdeSolver")
  L <- length(solvers)
  WT <- matrix(0.0, L, chains)
  ST <- matrix(0.0, L, chains)
  TT <- matrix(0.0, L, chains)
  FN <- c()
  GT <- rep(0.0, L)
  for (j in seq_len(L)) {
    solver <- solvers[[j]]
    conf_str <- solver$to_string()
    cat("=================================================================\n")
    cat(" (", j, ") Sampling with: ", conf_str, "\n", sep = "")
    fn <- file.path(savedir, paste0(basename, "_", j, ".rds"))
    fit <- sample_odemodel(
      model =  model,
      t0 = t0,
      t = t,
      data  = data,
      solver = solver,
      chains = chains,
      ...
    )
    cat("Saving OdeModelMCMC object to ", fn, "\n", sep = "")
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
    times = times, files = FN, solver = solver
  )
}

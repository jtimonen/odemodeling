# WORKFLOW ----------------------------------------------------------------

# Run the workflow
validate_fit <- function(setup, sampled, tols, max_num_steps, ...) {
  post_fit <- sampled$fit
  post_draws <- post_fit$draws(setup$param_names)
  sargs <- list(
    abs_tol = sampled$tol,
    rel_tol = sampled$tol,
    max_num_steps = max_num_steps
  )
  post_sim <- simulate(setup, post_draws, sargs)

  # Simulations
  sims_reference <- simulate_many(setup, post_draws, tols, max_num_steps)
  times <- sapply(sims_reference, function(x) {
    x$time()$total
  })

  # Tune the reference method so that it is reliable at post_draws
  cat("\nValidating tolerances...\n")
  tuning <- validate_tols(
    setup, post_sim, sims_reference
  )
  # tuning_plot <- plot_tuning(tuning)
  list(
    time = post_sim$time()$total,
    sims_reference = sims_reference,
    times_reference = times,
    tuning = tuning
  )
}

# UTILS -------------------------------------------------------------------

# Validate solver arguments
check_sa <- function(solver_args) {
  MAX_INT <- 2^31 - 1
  required <- c("rel_tol", "abs_tol", "max_num_steps")
  checkmate::assert_names(names(solver_args), permutation.of = required)
  checkmate::assertNumeric(solver_args$rel_tol, lower = 0)
  checkmate::assertNumeric(solver_args$abs_tol, lower = 0)
  checkmate::assertIntegerish(solver_args$max_num_steps,
    lower = 1,
    upper = MAX_INT
  )
  TRUE
}

# Print output if running Stan model failed
print_output_if_failed <- function(stan_out) {
  codes <- stan_out$return_codes()
  idx_failed <- which(codes > 0)
  for (idx in idx_failed) {
    cat("Chain ", idx, ", failed, printing its output:\n", sep = "")
    print(stan_out$output(idx))
  }
  if (length(idx_failed == 0)) {
    cat("All chains were successful.\n")
  }
}

# Determine speedup compared to full sampling
compute_speedup <- function(sim_tols, sampling_tols, sim_times, full_times) {
  t_sample_init <- full_times[, 1]
  tols <- intersect(as.character(sim_tols), as.character(sampling_tols))
  t_sim <- sim_times[, tols]
  t_sample <- full_times[, tols]
  tols <- as.numeric(tols)
  t_workflow <- t_sample_init + t_sim
  return(t_workflow / t_sample)
}

# RUNNING CMDSTAN -----------------------------------------------------

# Function for simulating ODE solutions and data given parameter(draws)s
simulate <- function(setup, params, solver_args) {
  stopifnot(is(setup, "OdeExperimentSetup"))
  stopifnot(is(params, "draws"))
  stopifnot(is(solver_args, "list"))
  check_sa(solver_args)
  data <- setup$data
  stan_opts <- setup$stan_opts
  model <- setup$stanmodels$simulator
  capture.output({
    out <- model$generate_quantities(
      data = c(data, solver_args),
      fitted_params = params,
      seed = stan_opts$seed,
      sig_figs = stan_opts$sig_figs
    )
  })
  print_output_if_failed(out)
  return(out)
}

# Run simulate with many tolerances
simulate_many <- function(setup, params, tols, max_num_steps) {
  out <- list()
  for (tol_j in tols) {
    cat(" * Simulating with tol = ", tol_j, "\n", sep = "")
    sargs <- list(
      abs_tol = tol_j,
      rel_tol = tol_j,
      max_num_steps = max_num_steps
    )
    sim <- simulate(setup, params, sargs)
    out <- c(out, sim)
  }
  names(out) <- tols
  return(out)
}

# Simulate but denser in time
simulate_dense <- function(setup, params, solver_args, new_t) {
  new_setup <- setup$clone()
  new_setup$data$t <- new_t
  new_setup$data$N <- length(new_t)
  new_setup$data$y <- rep(1.0, new_setup$data$N)
  simulate(new_setup, params, solver_args)
}

# Function for posterior sampling
sample_posterior <- function(model, data, solver_args, stan_opts, ...) {
  stopifnot(is(model, "CmdStanModel"))
  stopifnot(is(data, "list"))
  stopifnot(is(solver_args, "list"))
  check_sa(solver_args)
  fit <- model$sample(
    data = c(data, solver_args),
    sig_figs = stan_opts$sig_figs,
    seed = stan_opts$seed,
    ...
  )
  print_output_if_failed(fit)
  return(fit)
}

# Load fit from object returned by $sample_posterior_many()
load_fit <- function(post, idx) {
  fit <- readRDS(post$files[idx])
  time <- post$grand_total[idx]
  tol <- post$tols[idx]
  list(fit = fit, time = time, tol = tol)
}

# COMPUTING ERRORS --------------------------------------------------------

# Compute error in x compared to x_ref
compute_sol_error <- function(x, x_ref, fun) {
  abs_errors <- abs(as.vector(x_ref) - as.vector(x))
  eval(call(fun, abs_errors))
}

# Compute error to most accurate solution
compute_sol_errors <- function(XSIM, fun = "max") {
  J1 <- dim(XSIM)[1]
  J2 <- dim(XSIM)[2]
  ERR <- array(0, dim = c(J1, J2))
  x_ref <- XSIM[1, 1, , ]
  for (j1 in 1:J1) {
    for (j2 in 1:J2) {
      x <- XSIM[j1, j2, , ]
      ERR[j1, j2] <- compute_sol_error(x, x_ref, fun)
    }
  }
  return(ERR)
}

# Compute error in likelihood, compared to most accurate solution
compute_loglik_errors <- function(LL, fun = "max") {
  J1 <- dim(LL)[1]
  J2 <- dim(LL)[2]
  ERR <- array(0, dim = c(J1, J2))
  loglik_best <- LL[1, 1, ]
  for (j1 in 1:J1) {
    for (j2 in 1:J2) {
      loglik <- LL[j1, j2, ]
      abs_errors <- abs(as.vector(loglik_best) - as.vector(loglik))
      ERR[j1, j2] <- eval(call(fun, abs_errors))
    }
  }
  return(ERR)
}


# COMPUTING PARETO_K --------------------------------------------------------

# Compute log importance weights
log_importance_weights <- function(fit_high, fit_low) {
  stopifnot(is(fit_high, "CmdStanFit"))
  stopifnot(is(fit_low, "CmdStanFit"))
  LL_high <- fit_high$draws("log_lik")[, , 1, drop = TRUE]
  LL_low <- fit_low$draws("log_lik")[, , 1, drop = TRUE]
  return(LL_high - LL_low)
}

# Compute importance weights and pareto k diagnostic
use_psis <- function(fit_high, fit_low) {
  log_ratios <- log_importance_weights(fit_high, fit_low)
  chain_id <- rep(1:ncol(log_ratios), each = nrow(log_ratios))
  x <- as.vector(exp(-log_ratios))
  r_eff <- loo::relative_eff(x, chain_id)
  loo::psis(x, r_eff = r_eff)
}


# TUNING THE SOLVER -------------------------------------------------------

# Extract ODE solutions
get_x_sim <- function(sim) {
  posterior::merge_chains(sim$draws("x"))[, 1, , drop = TRUE]
}

# Run tuning
validate_tols <- function(setup, sim, sims_reference) {
  S <- posterior::ndraws(posterior::merge_chains(sim$draws()))
  x <- get_x_sim(sim)
  j <- 0
  res <- NULL
  for (sim_ref in sims_reference) {
    j <- j + 1
    tol_j <- as.numeric(names(sims_reference)[j])
    x_ref <- get_x_sim(sim_ref)
    err_j <- compute_sol_error(x_ref, x, "max")
    is <- use_psis(sim_ref, sim)
    k_j <- is$diagnostics$pareto_k
    n_eff <- is$diagnostics$n_eff
    t_j <- sim_ref$time()$total
    r_j <- n_eff / S
    res_j <- c(1 / tol_j, t_j, err_j, k_j, r_j)
    res <- rbind(res, res_j)
  }
  colnames(res) <- c("inv_tol", "time", "mae", "k_hat", "r_eff")
  res <- data.frame(res)
  rownames(res) <- NULL
  return(res)
}


# PLOTTING ----------------------------------------------------------------

# Plot tuning results
plot_tuning <- function(tuning, ...) {
  df <- tuning
  add_geoms <- function(x, breaks) {
    x + geom_line() + geom_point() + scale_x_log10(breaks = breaks) +
      xlab(expression(tol^"-1")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  tols <- df$inv_tol
  p_A <- add_geoms(ggplot(df, aes(x = inv_tol, y = mae)), breaks = tols)
  p_B <- add_geoms(ggplot(df, aes(x = inv_tol, y = k_hat)), breaks = tols) +
    ylab(expression(hat(k)))
  p_C <- add_geoms(ggplot(df, aes(x = inv_tol, y = r_eff)), breaks = tols) +
    ylab(expression(r[eff]))
  p_D <- add_geoms(ggplot(df, aes(x = inv_tol, y = time)), breaks = tols) +
    ylab("time (s)")
  plt <- ggpubr::ggarrange(p_A, p_B, p_C, p_D, labels = "auto", ...)
  return(plt)
}


# Plotting helper
create_ribbon_plot_df <- function(rvar) {
  alpha1 <- 0.1
  alpha2 <- 0.25
  c1 <- 100 * (1 - 2 * alpha1)
  c2 <- 100 * (1 - 2 * alpha2)
  message("Plotting median and central ", c1, "% and ", c2, "% intervals.",
    sep = ""
  )
  lower1 <- as.vector(quantile(rvar, probs = alpha1))
  upper1 <- as.vector(quantile(rvar, probs = 1 - alpha1))
  lower2 <- as.vector(quantile(rvar, probs = alpha2))
  upper2 <- as.vector(quantile(rvar, probs = 1 - alpha2))
  median <- as.vector(quantile(rvar, probs = 0.5))
  df <- data.frame(median, lower1, upper1, lower2, upper2)
  return(df)
}

# Useful data frame
create_useful_plot_df <- function(tols, values, mns = NULL) {
  if (is.null(tols)) {
    tols <- as.numeric(colnames(values))
  }
  values <- t(values)
  nrep <- ncol(values)
  ntols <- nrow(values)
  df <- data.frame(cbind(tols, values))
  colnames(df) <- c("tol", paste0("rep", 1:nrep))
  df <- pivot_longer(df, cols = starts_with("rep"))
  colnames(df) <- c("inv_tol", "rep", "value")
  df$inv_tol <- 1 / df$inv_tol

  if (!is.null(mns)) {
    mns <- format(mns, scientific = TRUE)
    df$max_num_steps <- rep(paste("max_num_steps =", mns), ntols)
  }
  return(df)
}

# Plot timing results
plot_timing <- function(tols, times, mns = NULL, unit = "seconds",
                        median_text = TRUE) {
  df <- create_useful_plot_df(tols, times, mns)
  df$time <- df$value
  df$value <- NULL
  if (unit == "hours") {
    df$time <- df$time / 3600
    ylabel <- "time (hours)"
  } else if (unit == "seconds") {
    ylabel <- "time (seconds)"
  } else {
    ylabel <- ""
  }
  plt <- ggplot(df, aes(x = inv_tol, y = time, group = inv_tol)) +
    geom_boxplot(fill = "firebrick2", col = "firebrick")
  plt <- plt + scale_x_log10(breaks = unique(df$inv_tol)) + xlab(expression(tol^"-1")) +
    ylab(ylabel) + ggtitle("Timing plot") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  if (!is.null(mns)) {
    plt <- plt + facet_grid(. ~ max_num_steps)
    vars <- c("inv_tol", "max_num_steps")
  } else {
    vars <- c("inv_tol")
  }

  if (median_text) {
    df_meds <- plyr::ddply(df, vars, plyr::summarise,
      t_med = round(median(time), 2)
    )
    plt <- plt + geom_text(
      data = df_meds, aes(
        x = inv_tol, y = t_med,
        label = t_med
      ),
      size = 3, vjust = -2, col = "steelblue3"
    )
  }

  return(plt)
}

# Plot any metric
plot_metric <- function(values, mns = NULL) {
  df <- create_useful_plot_df(NULL, values, mns = mns)
  plt <- ggplot(df, aes(x = inv_tol, y = value, group = rep, col = max_num_steps)) +
    geom_line(alpha = 0.7) +
    scale_x_log10(breaks = unique(df$inv_tol)) +
    geom_point(pch = 20, alpha = 0.7)
  return(plt)
}

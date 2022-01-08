# Setup -------------------------------------------------------------------

SEED <- 353

# Create data
num_tp <- 15
G <- 3
t0 <- 0
t <- c(1:num_tp)
dat <- list(
  G = G,
  D = 2 * G,
  pop_sizes = rep(1000, G),
  contacts = matrix(c(1, 0.5, 0.2, 0.5, 1, 0.5, 0.2, 0.5, 1.0), 3, 3),
  I0 = c(5, 0, 0),
  delta = 0.001,
  I_data = matrix(1, num_tp, G)
)


# Model compilation -------------------------------------------------------

# Create models
prior <- example_ode_model(compile = F, prior_only = TRUE)
prior$reinit()
post <- example_ode_model()


# Prior sampling ----------------------------------------------------------

# Sample from prior
fit <- prior$sample(
  t0 = t0, t = t,
  data = dat,
  iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0,
  seed = SEED
)

# Get data
idx <- 322
y_sol <- fit$extract_odesol()[idx, , ]
I_gen <- fit$extract_unflattened(variable = "I_gen")[idx, , ]
dat$I_data <- I_gen

test_that("prior sampling works", {
  expect_false(prior$has_likelihood)
  expect_true(is(fit, "OdeModelMCMC"))
  expect_equal(dim(y_sol), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
  expect_output(fit$print())
  expect_gt(fit$setup_time, 0.0)
  expect_gt(fit$time()$total, 0.0)
  expect_gt(nchar(fit$draws_size()), 2)
  expect_gt(nchar(fit$cmdstan_version()), 5)
  expect_true(fit$model$assert_stanfile_exists())
  expect_equal(dim(fit$summary()), c(146, 10))
  expect_equal(fit$cmdstan_seed(), SEED)
  expect_equal(fit$cmdstan_init(), 2)
  expect_error(fit$loglik(), "model has no likelihood function specified")
})

test_that("plotting ODE solutions works", {
  plt <- fit$plot_odesol()
  expect_s3_class(plt, "ggplot")
  plt <- fit$plot_odesol(draw_inds = c(2:9))
  expect_s3_class(plt, "ggplot")
})

# Posterior sampling ------------------------------------------------------

post_fit_solver <- bdf(abs_tol = 1e-4, rel_tol = 1e-4, max_num_steps = 1e3)
post_fit <- post$sample(
  t0 = t0, t = t,
  data = dat,
  iter_warmup = 10, iter_sampling = 10, chains = 2, refresh = 0,
  init = 0, step_size = 0.1,
  seed = SEED,
  solver = post_fit_solver
)

test_that("posterior sampling works", {
  expect_true(post$has_likelihood)
  idx <- 7
  y_sol <- post_fit$extract_odesol()[idx, , ]
  I_gen <- post_fit$extract_unflattened(variable = "I_gen")[idx, , ]
  expect_true(is(post_fit, "OdeModelMCMC"))
  expect_equal(dim(y_sol), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
  expect_equal(dim(post_fit$loglik()), c(10, 2, 1))
})

test_that("posterior sampling using many configurations works", {
  e_tols <- -c(2:5)
  confs <- rk45_list(tols = 10^e_tols, max_num_steps = 1000)
  res <- post$sample_manyconf(
    t0 = t0, t = t,
    data = dat,
    solvers = confs,
    iter_warmup = 10, iter_sampling = 10, chains = 1, refresh = 0,
    init = 0, step_size = 0.1,
    seed = SEED
  )
  expect_length(res$times$grand_total, 4)
  unlink("results", recursive = TRUE)
})


# Generating quantities ---------------------------------------------------

test_that("generating quantities works", {
  expect_error(
    fit$simulate(t0 = 4.5),
    "each value in t must be strictly larger than given t0"
  )
  tout <- c(1, 2, 3, 5)
  sims <- list()
  sims[[1]] <- fit$gqs(solver = rk45(), t = tout)
  sims[[2]] <- fit$gqs(solver = bdf(), t = tout)
  sims[[3]] <- fit$gqs(solver = adams(), t = tout)
  sims[[4]] <- fit$gqs(solver = ckrk(), t = tout)
  sims[[5]] <- fit$gqs(solver = euler(num_steps = 30), t = tout)
  sims[[6]] <- fit$gqs(solver = midpoint(num_steps = 30), t = tout)
  sims[[7]] <- fit$gqs(solver = rk4(num_steps = 30), t = tout)
  for (a in sims) {
    expect_output(print(a), "An object of class OdeModelGQ")
    idx <- 7
    y_sol <- a$extract_odesol()[idx, , ]
    I_gen <- a$extract_unflattened(variable = "I_gen")[idx, , ]
    expect_true(is(a, "OdeModelGQ"))
    expect_equal(dim(y_sol), c(4, 6))
    expect_equal(dim(I_gen), c(4, 3))
    expect_equal(a$get_t0(), 0.0)
    expect_equal(dim(post_fit$loglik()), c(10, 2, 1))
  }
})


# Workflow ----------------------------------------------------------------

sfun <- function(solver) {
  post_fit$gqs(
    t0 = t0, t = t,
    data = dat,
    seed = SEED,
    solver = solver
  )
}

test_that("workflow works", {
  post_sims <- list()
  post_sims[[1]] <- sfun(solver = rk45())
  post_sims[[2]] <- sfun(solver = bdf())
  post_sims[[3]] <- sfun(solver = adams())
  post_sims[[4]] <- sfun(solver = ckrk())
  post_sims[[5]] <- sfun(solver = euler(num_steps = 30))
  post_sims[[6]] <- sfun(solver = midpoint(num_steps = 30))
  post_sims[[7]] <- sfun(solver = rk4(num_steps = 30))
  N <- length(t)

  # Simulation works with all solvers
  for (a in post_sims) {
    expect_output(print(a), "An object of class OdeModelGQ")
    expect_equal(a$cmdstan_seed(), SEED)
    idx <- 7
    y_sol <- a$extract_odesol()[idx, , ]
    I_gen <- a$extract_unflattened(variable = "I_gen")[idx, , ]
    expect_true(is(a, "OdeModelGQ"))
    expect_equal(dim(y_sol), c(N, 6))
    expect_equal(dim(I_gen), c(N, 3))
    expect_equal(a$get_t0(), 0.0)
    expect_equal(dim(a$loglik()), c(10, 2, 1))

    # PSIS works
    expect_warning(
      {
        is <- psis(post_fit, a)
      },
      "Not enough tail samples"
    )
    expect_equal(names(is), c("log_weights", "diagnostics"))
    expect_equal(length(is$log_weights), 20) # 10 times 2
  }

  # Simulation actually gives different result for each solver
  for (j in 2:length(post_sims)) {
    d1 <- max_abs_loglik_diff(post_sims[[1]], post_sims[[j]])
    d2 <- max_abs_odesol_diff(post_sims[[1]], post_sims[[j]])
    expect_gt(d1, 1e-10)
    expect_gt(d2, 1e-10)
  }
})

test_that("sim with same solver as during sampling gives same output", {
  post_sim_same <- sfun(post_fit_solver)
  d1 <- max_abs_loglik_diff(post_fit, post_sim_same)
  d2 <- max_abs_odesol_diff(post_fit, post_sim_same)
  expect_lt(d1, 1e-6) # should be 0 but CSV conversion messes this up
  expect_lt(d2, 1e-6) # should be 0 but CSV conversion messes this up
})

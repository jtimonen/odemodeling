SEED <- 555

# Create model
a <- example_odemodel(compile = F)
a$reinit()

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

# Sample from prior
fit <- sample_odemodel(
  model = a,
  t0 = t0, t = t,
  data = dat,
  prior_only = TRUE,
  iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0,
  seed = SEED
)

# Get data
idx <- 322
x_ode <- get_array_draw(fit, variable = "x_ode", iteration = idx)
I_gen <- get_array_draw(fit, variable = "I_gen", iteration = idx)
dat$I_data <- I_gen

test_that("prior sampling works", {
  expect_true(is(fit, "CmdStanMCMC"))
  expect_equal(dim(x_ode), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
})

test_that("posterior sampling works", {

  # Posterior sampling
  post_fit <- sample_odemodel(
    model = a,
    t0 = t0, t = t,
    data = dat,
    iter_warmup = 10, iter_sampling = 10, chains = 1, refresh = 0,
    init = 0, step_size = 0.1,
    seed = SEED
  )

  idx <- 7
  x_ode <- get_array_draw(post_fit, variable = "x_ode", iteration = idx)
  I_gen <- get_array_draw(post_fit, variable = "I_gen", iteration = idx)
  expect_true(is(post_fit, "CmdStanMCMC"))
  expect_equal(dim(x_ode), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
})

test_that("posterior sampling using many configurations works", {
  confs <- create_solver_conf_list(tols = 10^(-c(2:5)), max_num_steps = 1000)
  res <- sample_odemodel_manyconf(
    model = a,
    t0 = t0, t = t,
    data = dat,
    solver_confs = confs,
    iter_warmup = 10, iter_sampling = 10, chains = 1, refresh = 0,
    init = 0, step_size = 0.1,
    seed = SEED
  )
  expect_length(res$times$grand_total, 4)
  unlink("results", recursive = TRUE)
})

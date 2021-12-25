SEED <- 555

# Create models
prior <- example_odemodel(compile = F, prior_only = TRUE)
prior$reinit()
post <- example_odemodel()

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
  model = prior,
  t0 = t0, t = t,
  data = dat,
  iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0,
  seed = SEED
)

# Get data
idx <- 322
y_sol <- fit$get_array_draw(variable = "y_sol", iteration = idx)
I_gen <- fit$get_array_draw(variable = "I_gen", iteration = idx)
dat$I_data <- I_gen

test_that("prior sampling works", {
  expect_true(is(fit, "OdeModelFit"))
  expect_equal(dim(y_sol), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
  expect_output(fit$print())
  expect_gt(fit$setup_time, 0.0)
  expect_gt(fit$time()$total, 0.0)
  expect_gt(nchar(fit$draws_size()), 2)
  expect_gt(nchar(fit$cmdstan_version()), 5)
  expect_true(fit$model$assert_stanfile_exists())
})

test_that("posterior sampling works", {

  # Posterior sampling
  post_fit <- sample_odemodel(
    model = post,
    t0 = t0, t = t,
    data = dat,
    iter_warmup = 10, iter_sampling = 10, chains = 1, refresh = 0,
    init = 0, step_size = 0.1,
    seed = SEED
  )

  idx <- 7
  y_sol <- post_fit$get_array_draw(variable = "y_sol", iteration = idx)
  I_gen <- post_fit$get_array_draw(variable = "I_gen", iteration = idx)
  expect_true(is(post_fit, "OdeModelFit"))
  expect_equal(dim(y_sol), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
})

test_that("posterior sampling using many configurations works", {
  confs <- create_solver_conf_list(tols = 10^(-c(2:5)), max_num_steps = 1000)
  res <- sample_odemodel_manyconf(
    model = post,
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

a <- example_odemodel(compile = F)
a$reinit()

test_that("prior and posterior sampling works", {
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
  fit <- sample_odemodel(
    model = a,
    t0 = t0, t = t,
    data = dat,
    prior_only = TRUE,
    iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0
  )

  idx <- 322
  x_ode <- get_array_draw(fit, variable = "x_ode", iteration = idx)
  I_gen <- get_array_draw(fit, variable = "I_gen", iteration = idx)
  expect_true(is(fit, "CmdStanMCMC"))
  expect_equal(dim(x_ode), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))

  # Posterior sampling
  dat$I_data <- I_gen
  post_fit <- sample_odemodel(
    model = a,
    t0 = t0, t = t,
    data = dat,
    iter_warmup = 10, iter_sampling = 10, chains = 1, refresh = 0
  )

  idx <- 2
  x_ode <- get_array_draw(post_fit, variable = "x_ode", iteration = idx)
  I_gen <- get_array_draw(post_fit, variable = "I_gen", iteration = idx)
  expect_true(is(post_fit, "CmdStanMCMC"))
  expect_equal(dim(x_ode), c(15, 6))
  expect_equal(dim(I_gen), c(15, 3))
})

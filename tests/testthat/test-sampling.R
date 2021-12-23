a <- example_odemodel(compile = F)
a$reinit()

test_that("prior can be used to simulate data", {
  num_tp <- 15
  G <- 3
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
    t0 = 0, t = 1:num_tp,
    data = dat,
    prior_only = TRUE,
    iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0
  )
  x_ode <- fit$draws("x_ode")
  I_gen <- fit$draws("I_gen")
  expect_true(is(fit, "CmdStanMCMC"))
  expect_equal(dim(x_ode), c(1000, 1, 90)) # 90 = 15 * 6
  expect_equal(dim(I_gen), c(1000, 1, 45)) # 45 = 15 * 3
})

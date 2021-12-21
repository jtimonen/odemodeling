a <- example_odemodel(compile = F)
a$reinit()

test_that("prior model can be compiled and sampled", {
  expect_error(a$sample_prior(), "G is missing from the data list")
  expect_error(a$sample_prior(dims = list(N = 2)), "G is missing from the data")
  dat <- list(G = 3)
  fit <- a$sample_prior(
    dims = dat, iter_warmup = 300, iter_sampling = 300, chains = 1, refresh = 0
  )
  expect_true(is(fit, "CmdStanMCMC"))
})

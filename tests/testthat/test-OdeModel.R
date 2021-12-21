a <- example_odemodel(compile = F)

test_that("OdeModel methods work correctly", {
  expect_error(a$assert_files_exist(), "Model not initialized")
  expect_gt(nchar(a$prior$code), 10)
  expect_gt(nchar(a$posterior$code), 10)
  expect_output(a$print())
})

test_that("prior model has correct names", {
  nam1 <- a$prior$param_names()
  nam2 <- a$prior$param_names(inc_transformed = T)
  nam3 <- a$prior$data_names()
  expect_equal(nam1, c("beta", "gamma", "phi_inv"))
  expect_equal(nam2, c("beta", "gamma", "phi_inv", "phi"))
  expect_equal(nam3, c("G"))
})

test_that("posterior model has correct names", {
  nam1 <- a$posterior$param_names()
  nam2 <- a$posterior$param_names(inc_transformed = T)
  nam3 <- a$posterior$data_names()
  expect_equal(nam1, c("beta", "gamma", "phi_inv"))
  expect_equal(nam2, c("beta", "gamma", "phi_inv", "phi", "x_ode", "log_lik"))
  nam3_real <- c(
    "G", "N", "D", "pop_sizes", "I0", "contacts",
    "delta", "I_data", "t0", "t", "abs_tol", "rel_tol",
    "max_num_steps", "num_steps", "solver"
  )
  expect_equal(nam3, nam3_real)
})


# Compile
a$reinit()

test_that("prior model can be compiled and sampled", {
  expect_error(a$sample_prior(), "is missing, with no default")
  expect_error(a$sample_prior(data = list(N = 2)), "G is missing from the data")
  dat <- list(G = 3)
  fit <- a$sample_prior(
    data = dat, iter_warmup = 300, iter_sampling = 300, chains = 1, refresh = 0
  )
  expect_true(is(fit, "CmdStanMCMC"))
})

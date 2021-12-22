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
  nam3_real <- c(
    "G", "N", "D", "pop_sizes", "I0", "contacts",
    "delta", "I_data", "t0", "t", "abs_tol", "rel_tol",
    "max_num_steps", "num_steps", "solver"
  )
  expect_equal(nam3, nam3_real)
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

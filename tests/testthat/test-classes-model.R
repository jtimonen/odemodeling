a <- example_odemodel(compile = F)

test_that("OdeModel methods work correctly", {
  expect_error(a$assert_stanfile_exists(), "Model not initialized")
  expect_gt(nchar(a$code()), 10)
  expect_output(a$print())
})

test_that("model has correct names", {
  sm <- a$stanmodel
  nam1 <- sm$param_names()
  nam2 <- sm$param_names(inc_transformed = T)
  nam3 <- sm$data_names()
  expect_equal(nam1, c("beta", "gamma", "phi_inv"))
  expect_equal(nam2, c("beta", "gamma", "phi_inv", "phi", "y_sol", "log_lik"))
  nam3_real <- c(
    "G", "N", "D", "pop_sizes", "I0", "contacts",
    "delta", "I_data", "t0", "t", "abs_tol", "rel_tol",
    "max_num_steps", "num_steps", "solver"
  )
  expect_equal(nam3, nam3_real)
})

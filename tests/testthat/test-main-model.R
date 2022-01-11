test_that("odemodel creation gives correct message", {
  expect_message(example_ode_model("gsir", compile = F), "Not compiling")
})

test_that("ode_model() works without any variables", {
  a <- ode_model(
    N = stan_dim("N"),
    odefun_body = "return(rep_vector(0.0, 2));",
    loglik_body = "return(0);",
    odefun_init = stan_vector("y0", length = stan_dim("K")),
    compile = FALSE
  )
  expect_error(a$assert_stanfile_exists(), "Model not initialized")
  expect_gt(nchar(a$code()), 1000)
})

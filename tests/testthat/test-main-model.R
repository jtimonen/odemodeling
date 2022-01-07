test_that("odemodel creation gives correct message", {
  expect_message(example_ode_model(compile = F), "Not compiling")
})

test_that("Stan code can be created for gsir example", {
  gsir <- example_ode_model(name = "gsir", compile = F)
  expect_true(gsir$has_likelihood)
})

test_that("Stan code can be created for tmdd example", {
  tmdd <- example_ode_model(name = "tmdd", compile = F)
  expect_true(tmdd$has_likelihood)
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

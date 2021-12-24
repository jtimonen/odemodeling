test_that("model with only prior sampling (no ODE solving) can be created", {
  par <- stan_param(stan_var("dummy"), "dummy ~ normal(0,1);")
  gq <- stan_transform(stan_var("foo"),
    origin = "model",
    code = "foo = 2*dummy;"
  )
  odefun_vars <- list(par)
  other_vars <- list(gq)
  expect_output(par$print())
  expect_output(gq$print())
  code <- generate_stancode_prior(odefun_vars, list(), other_vars, FALSE)
  expect_equal(nchar(code$code), 64)
})

test_that("odemodel creation gives correct message", {
  expect_message(example_ode_model(compile = F), "Not compiling")
})

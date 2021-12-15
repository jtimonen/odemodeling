test_that("example model is valid", {
  model <- example_odemodel(verbose = F)
  expect_true(model$stanmodel$check_syntax())
  expect_true(model$datasim)
  expect_output(model$print())
})

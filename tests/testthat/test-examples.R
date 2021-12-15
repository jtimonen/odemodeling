test_that("example model has valid syntax", {
  model <- example_model(verbose = F, compile = F)
  expect_true(model$check_syntax())
})

test_that("odemodel creation gives correct message", {
  expect_message(example_odemodel(compile = F), "Not compiling")
})

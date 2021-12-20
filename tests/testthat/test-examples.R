test_that("example model creation works", {
  expect_message(example_odemodel(compile = F), "Not compiling")
  a <- example_odemodel(compile = F)
  expect_error(a$assert_files_exist(), "At least one Stan model file")
  expect_gt(nchar(a$prior$code), 10)
  expect_gt(nchar(a$posterior$code), 10)
  expect_output(a$print())
})

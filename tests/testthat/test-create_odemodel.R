test_that("Stan template can be read", {
  expect_gt(nchar(stan_template()), 100)
})

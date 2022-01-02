test_that("rk45() works correctly", {
  tols <- exp(rnorm(2))
  mns <- 999
  a <- rk45(abs_tol = tols[1], rel_tol = tols[2], max_num_steps = mns)
  expect_true("AdaptiveOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "rk45")
  sd <- a$standata()
  expect_equal(sd$abs_tol, tols[1])
  expect_equal(sd$rel_tol, tols[2])
  expect_equal(sd$max_num_steps, mns)
  expect_equal(sd$solver, 1)
})

test_that("bdf() works correctly", {
  tols <- exp(rnorm(2))
  mns <- 993
  a <- bdf(abs_tol = tols[1], rel_tol = tols[2], max_num_steps = mns)
  expect_true("AdaptiveOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "bdf")
  sd <- a$standata()
  expect_equal(sd$abs_tol, tols[1])
  expect_equal(sd$rel_tol, tols[2])
  expect_equal(sd$max_num_steps, mns)
  expect_equal(sd$solver, 2)
})

test_that("adams() works correctly", {
  tols <- exp(rnorm(2))
  mns <- 991
  a <- adams(abs_tol = tols[1], rel_tol = tols[2], max_num_steps = mns)
  expect_true("AdaptiveOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "adams")
  sd <- a$standata()
  expect_equal(sd$abs_tol, tols[1])
  expect_equal(sd$rel_tol, tols[2])
  expect_equal(sd$max_num_steps, mns)
  expect_equal(sd$solver, 3)
})


test_that("ckrk() works correctly", {
  tols <- exp(rnorm(2))
  mns <- 991
  a <- ckrk(abs_tol = tols[1], rel_tol = tols[2], max_num_steps = mns)
  expect_true("AdaptiveOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "ckrk")
  sd <- a$standata()
  expect_equal(sd$abs_tol, tols[1])
  expect_equal(sd$rel_tol, tols[2])
  expect_equal(sd$max_num_steps, mns)
  expect_equal(sd$solver, 4)
})

test_that("rk4() works correctly", {
  ns <- 3
  a <- rk4(num_steps = ns)
  expect_true("FixedNumStepsOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "rk4")
  sd <- a$standata()
  expect_equal(sd$num_steps, ns)
  expect_equal(sd$solver, 11)
})

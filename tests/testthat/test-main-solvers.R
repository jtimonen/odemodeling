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

  expect_output(a$print(), "rk45")
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

  expect_output(a$print(), "bdf")
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

  expect_output(a$print(), "adams")
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

  expect_output(a$print(), "ckrk")
})

test_that("euler() works correctly", {
  ns <- 2
  a <- euler(num_steps = ns)
  expect_true("FixedNumStepsOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "euler")
  sd <- a$standata()
  expect_equal(sd$num_steps, ns)
  expect_equal(sd$solver, 101)

  expect_output(a$print(), "euler")
})

test_that("midpoint() works correctly", {
  ns <- 5
  a <- midpoint(num_steps = ns)
  expect_true("FixedNumStepsOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "midpoint")
  sd <- a$standata()
  expect_equal(sd$num_steps, ns)
  expect_equal(sd$solver, 102)

  expect_output(a$print(), "midpoint")
})

test_that("rk4() works correctly", {
  ns <- 3
  a <- rk4(num_steps = ns)
  expect_true("FixedNumStepsOdeSolver" %in% class(a))
  expect_true("OdeSolver" %in% class(a))
  expect_equal(a$name, "rk4")
  sd <- a$standata()
  expect_equal(sd$num_steps, ns)
  expect_equal(sd$solver, 103)

  expect_output(a$print(), "rk4")
})


test_that("rk45_list() works correctly", {
  tols <- c(0.1, 1e-5, 1e-15)
  mns <- 103
  a <- rk45_list(tols = tols, max_num_steps = mns)
  expect_equal(length(a), 3)
  for (j in seq_len(3)) {
    s <- a[[j]]
    expect_equal(s$name, "rk45")
    expect_equal(s$abs_tol, tols[j])
    expect_equal(s$rel_tol, tols[j])
    expect_equal(s$max_num_steps, mns)
  }
})

test_that("bdf_list() works correctly", {
  tols <- c(1, 1e-3, 1.343e-12)
  mns <- 101
  a <- bdf_list(tols = tols, max_num_steps = mns)
  expect_equal(length(a), 3)
  for (j in seq_len(3)) {
    s <- a[[j]]
    expect_equal(s$name, "bdf")
    expect_equal(s$abs_tol, tols[j])
    expect_equal(s$rel_tol, tols[j])
    expect_equal(s$max_num_steps, mns)
  }
})

test_that("midpoint_list() works correctly", {
  ns <- c(3:5)
  a <- midpoint_list(ns)
  expect_equal(length(a), 3)
  for (j in seq_len(3)) {
    s <- a[[j]]
    expect_equal(s$name, "midpoint")
    expect_equal(s$num_steps, ns[j])
  }
})

test_that("rk4_list() works correctly", {
  ns <- c(9:10)
  a <- rk4_list(ns)
  expect_equal(length(a), 2)
  for (j in seq_len(2)) {
    s <- a[[j]]
    expect_equal(s$name, "rk4")
    expect_equal(s$num_steps, ns[j])
  }
})

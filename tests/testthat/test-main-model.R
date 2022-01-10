library(posterior)

test_that("odemodel creation gives correct message", {
  expect_message(example_ode_model("gsir", compile = F), "Not compiling")
})

test_that("Stan code can be created for gsir example", {
  gsir <- example_ode_model("gsir", compile = F)
  expect_true(gsir$has_likelihood)
})

test_that("tmdd example can generate quantities", {

  # Create model
  tmdd <- example_ode_model(name = "tmdd", prior_only = TRUE)
  expect_false(tmdd$has_likelihood)

  # Define simulation parameters
  sim_k <- c(0.592, 0.900, 2.212, 0.823, 0.201, 0.024)
  sim_sigma <- 0.3
  sim_params <- tmdd$make_params(c(sim_k, sim_sigma))

  # Simulate and plot
  gq <- tmdd$gqs(
    t0 = 0,
    t = seq(0.2, 10, by = 0.2),
    data = list(L0 = 5, D = 3),
    params = sim_params
  )
  plt <- gq$plot_odesol()
  expect_s3_class(plt, "ggplot")
})


test_that("Lotka-Volterra example can be created", {
  lv <- example_ode_model(name = "lv", prior_only = TRUE)
  expect_false(lv$has_likelihood)
  expect_error(
    lv$make_params(c(1, 2)),
    "currently works only for models with only scalar parameters"
  )
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

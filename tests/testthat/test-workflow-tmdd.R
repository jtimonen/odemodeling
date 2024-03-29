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

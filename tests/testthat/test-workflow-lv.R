# Setup -------------------------------------------------------------------
SEED <- 123

# Create data
dat <- lynxhare
N <- nrow(dat) - 1
t <- dat$year[2:(N + 1)]
t0 <- dat$year[1]

# Model compilation -------------------------------------------------------

lv <- example_ode_model(name = "lv", prior_only = TRUE)

test_that("Lotka-Volterra example can be created", {
  expect_false(lv$has_likelihood)
  expect_error(
    lv$make_params(c(1, 2)),
    "currently works only for models with only scalar parameters"
  )
})

# Prior sampling ----------------------------------------------------------

# Sample from prior
fit <- lv$sample(
  t0 = t0, t = t, data = list(D = 2),
  iter_warmup = 1000, iter_sampling = 1000, chains = 1, refresh = 0,
  seed = SEED
)

test_that("prior sampling works", {
  expect_false(lv$has_likelihood)
  expect_true(is(fit, "OdeModelMCMC"))
  expect_output(fit$print())
  expect_gt(fit$setup_time, 0.0)
  expect_gt(fit$time()$total, 0.0)
  expect_gt(nchar(fit$draws_size()), 2)
  expect_gt(nchar(fit$cmdstan_version()), 5)
  expect_true(fit$model$assert_stanfile_exists())
  expect_equal(fit$cmdstan_seed(), SEED)
  expect_equal(fit$cmdstan_init(), 2)
  expect_error(fit$loglik(), "model has no likelihood function specified")
})

test_that("plotting ODE solutions works", {
  plt <- fit$plot_odesol()
  expect_s3_class(plt, "ggplot")
  plt <- fit$plot_odesol(draw_inds = c(2:9))
  expect_s3_class(plt, "ggplot")
})

test_that("extracting ODE solutions quantiles works", {
  df <- fit$extract_odesol_df_dist()
  expect_equal(dim(df), c(40, 5))
})

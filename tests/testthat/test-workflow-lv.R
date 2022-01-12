# Setup -------------------------------------------------------------------
SEED <- 123

# Create data
dat <- lynxhare
N <- nrow(dat) - 1
t <- dat$year[2:(N + 1)]
t0 <- dat$year[1]
y_obs <- cbind(dat$hare[2:(N + 1)], dat$lynx[2:(N + 1)])
y_obs_init <- c(dat$hare[1], dat$lynx[1])
add_data <- list(
  y_obs = y_obs,
  y_obs_init = y_obs_init,
  D = 2
)

# Model compilation -------------------------------------------------------

lv <- example_ode_model(name = "lv", prior_only = FALSE)

test_that("Lotka-Volterra example can be created", {
  expect_true(lv$has_likelihood)
  expect_error(
    lv$make_params(c(1, 2)),
    "currently works only for models with only scalar parameters"
  )
})

# Sampling ----------------------------------------------------------------

# Sample from posterior
fit <- lv$sample(
  t0 = t0, t = t, data = add_data,
  iter_warmup = 100, iter_sampling = 100, chains = 1, refresh = 0,
  seed = SEED, solver = midpoint(4), init = 0, step_size = 0.1,
  show_messages = FALSE
)

test_that("sampling works", {
  expect_true(lv$has_likelihood)
  expect_true(is(fit, "OdeModelMCMC"))
  expect_equal(dim(fit$extract_y0()), c(100, 2))
  expect_output(fit$print())
  expect_gt(fit$setup_time, 0.0)
  expect_gt(fit$time()$total, 0.0)
  expect_gt(nchar(fit$draws_size()), 2)
  expect_gt(nchar(fit$cmdstan_version()), 5)
  expect_true(fit$model$assert_stanfile_exists())
  expect_equal(fit$cmdstan_seed(), SEED)
  expect_equal(fit$cmdstan_init(), 0)
})

test_that("plotting ODE solutions works", {
  plt <- fit$plot_odesol()
  expect_s3_class(plt, "ggplot")
  plt <- fit$plot_odesol(draw_inds = c(2:9))
  expect_s3_class(plt, "ggplot")
})

test_that("extracting t and t0 works correctly", {
  x <- fit$get_t()
  y <- fit$get_t(include_t0 = TRUE)
  expect_equal(length(y), length(x) + 1)
})

test_that("extracting ODE solutions quantiles works", {
  df1 <- fit$extract_odesol_df_dist()
  df2 <- fit$extract_odesol_df_dist(include_y0 = TRUE)
  expect_equal(dim(df1), c(40, 5))
  expect_equal(dim(df2), c(42, 5))
})

test_that("plotting ODE solutions distributions works", {
  yn <- c("foo", "bar")
  plt1 <- fit$plot_odesol_dist(ydim_names = yn)
  plt2 <- fit$plot_odesol_dist(ydim_names = yn, include_y0 = TRUE)
  expect_s3_class(plt1, "ggplot")
  expect_s3_class(plt2, "ggplot")
})

test_that("plotting on a denser set of time points works", {
  t_dense <- seq(1901, 1920, by = 0.1)
  add_data_dense <- list(
    y_obs_init = add_data$y_obs_init,
    y_obs = matrix(0, length(t_dense), 2), # dummy
    D = 2
  )
  gq <- fit$gqs(t = t_dense, data = add_data_dense)
  plt <- gq$plot_odesol_dist(p = 0.8)
  expect_s3_class(plt, "ggplot")
})

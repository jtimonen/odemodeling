
test_that("stan_dim() works correctly", {
  N <- stan_dim("N")
  M <- stan_dim("M", lower = 1, upper = 100)
  expect_output(print(N), "int N;")
  expect_output(print(M), "int<lower=1, upper=100> M;")
})

test_that("stan_var() works correctly", {
  x <- stan_var("x", type = "real", lower = 0)
  y <- stan_var("y", type = "int")
  expect_output(print(x), "real<lower=0> x;")
  expect_output(print(y), "int y;")
})

test_that("stan_vector() works correctly", {
  x <- stan_vector("x", length = stan_dim("N", lower = 1), lower = 0)
  expect_output(print(x), "vector<lower=0>\\[N\\] x;")
})

test_that("stan_matrix() works correctly", {
  N <- stan_dim("N")
  M <- stan_dim("M", lower = 1, upper = 100)
  my_mat <- stan_matrix("A", nrow = N, ncol = M, lower = 0, upper = 3.2)
  expect_output(print(my_mat), "matrix<lower=0, upper=3\\.2>\\[N, M\\] A;")
})

test_that("stan_array() works correctly", {
  N <- stan_dim("N")
  M <- stan_dim("M")
  my_arr <- stan_array("A", dims = list(N, N, M), upper = 1, type = "int")
  expect_output(print(my_arr), "array\\[N, N, M\\] int<upper=1> A;")
})

test_that("stan_vector_array() works correctly", {
  N <- stan_dim("N")
  D <- stan_dim("D")
  vec_arr <- stan_vector_array("y", dims = list(N), length = D)
  expect_output(print(vec_arr), "array\\[N\\] vector\\[D\\] y;")
})

test_that("stan_param() works correctly", {
  # Scalar parameter
  my_par <- stan_param(stan_var("beta"), "beta ~ normal(0, 1);")
  expect_output(print(my_par), "Prior code:")

  # # Vector parameter
  my_vec <- stan_vector("alpha", stan_dim("D"), lower = 0)
  my_par <- stan_param(my_vec)
  expect_output(print(my_par), "No prior set.")
})


test_that("stan_transform() works correctly for gq", {
  N <- stan_dim("N")
  D <- stan_dim("D")
  decl <- stan_array("y", dims = list(N, D), type = "int")
  code <- "
    for(n in 1:N) {
      for(d in 1:D) {
        y[n, d] ~ poisson_rng(0.2);
      }
    }"
  y <- stan_transform(decl, code = code)
  expect_output(print(y), "Generated quantity:")
})

test_that("stan_param() works correctly for transformed param", {
  # Scalar parameter
  b <- stan_var("beta")
  y <- stan_transform(b, origin = "param", code = "beta = alpha + 1;")
  expect_output(print(y), "Transformed parameter:")
})

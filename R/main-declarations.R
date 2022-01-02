#' Create a [StanDimension] object
#'
#' @param name name of the dimension variable
#' @param lower lower bound
#' @param upper upper bound
#' @family Stan variable declaration functions
#' @export
#' @examples
#' N <- stan_dim("N")
#' M <- stan_dim("M", lower = 1, upper = 100)
#' print(N)
#' print(M)
stan_dim <- function(name, lower = NULL, upper = NULL) {
  StanDimension$new(name = name, lower = lower, upper = upper)
}

#' Create a [StanVariable] object.
#'
#' @param name name of the dimension variable
#' @param lower lower bound
#' @param upper upper bound
#' @param type variable type (real or int)
#' @family Stan variable declaration functions
#' @export
#' @examples
#' x <- stan_var("x", type = "real", lower = 0)
#' y <- stan_var("y", type = "int")
#' print(x)
#' print(y)
stan_var <- function(name, type = "real", lower = NULL, upper = NULL) {
  StanVariable$new(name = name, type = type, lower = lower, upper = upper)
}

#' Create a [StanVector] object.
#'
#' @param name name of the vector
#' @param lower lower bound
#' @param upper upper bound
#' @param length length of the vector, must be a [StanDimension] object
#' @family Stan variable declaration functions
#' @export
#' @examples
#' x <- stan_vector("x", length = stan_dim("N", lower = 1), lower = 0)
#' print(x)
stan_vector <- function(name, length, lower = NULL, upper = NULL) {
  StanVector$new(name = name, length = length, lower = lower, upper = upper)
}

#' Create a [StanMatrix] object.
#'
#' @param name name of the matrix
#' @param lower lower bound
#' @param upper upper bound
#' @param nrow number of rows, must be a [StanDimension] object
#' @param ncol number of columns, must be a [StanDimension] object
#' @family Stan variable declaration functions
#' @export
#' @examples
#' N <- stan_dim("N")
#' M <- stan_dim("M", lower = 1, upper = 100)
#' my_mat <- stan_matrix("A", nrow = N, ncol = M, lower = 0, upper = 3.2)
#' print(my_mat)
stan_matrix <- function(name, nrow, ncol, lower = NULL, upper = NULL) {
  StanMatrix$new(
    name = name, nrow = nrow, ncol = ncol,
    lower = lower, upper = upper
  )
}

#' Create a [StanArray] object.
#'
#' @param name name of the array
#' @param lower lower bound
#' @param upper upper bound
#' @param dims list of array dimensions, must be a list of
#'  `StanDimension` objects
#' @param type base type of the array
#' @family Stan variable declaration functions
#' @export
#' @examples
#' N <- stan_dim("N")
#' M <- stan_dim("M")
#' my_arr <- stan_array("A", dims = list(N, N, M), lower = 0, type = "int")
#' print(my_arr)
stan_array <- function(name, dims, type = "real", lower = NULL, upper = NULL) {
  StanArray$new(
    name = name, dims = dims, type = type,
    lower = lower, upper = upper
  )
}

#' Create a [StanVectorArray] object.
#'
#' @param name name of the vector array
#' @param lower lower bound
#' @param upper upper bound
#' @param dims list of array dimensions, must be a list of
#'  [StanDimension] objects
#' @param length length of the vector, must be a [StanDimension] object
#' @family Stan variable declaration functions
#' @export
#' @examples
#' N <- stan_dim("N")
#' D <- stan_dim("D")
#' vec_arr <- stan_vector_array("y", dims = list(N), length = D)
#' print(vec_arr)
stan_vector_array <- function(name, dims, length, lower = NULL, upper = NULL) {
  StanVectorArray$new(
    name = name, dims = dims, length = length,
    lower = lower, upper = upper
  )
}

#' Create a [StanParameter] object
#'
#' @param decl The Stan variable declaration from which the parameter is
#' created. Must be an object that inherits from [StanDeclaration] and
#' has a real or vector base type.
#' @param prior_code A string of Stan code that defines the prior for the
#' parameter. The default is empty string (no prior).
#' @family Stan variable declaration functions
#' @export
#' @examples
#' # Scalar parameter
#' my_par <- stan_param(stan_var("beta"), "beta ~ normal(0, 1);")
#' print(my_par)
#'
#' # Vector parameter
#' my_vec <- stan_vector("alpha", stan_dim("D"), lower = 0)
#' my_par <- stan_param(my_vec)
#' print(my_par)
stan_param <- function(decl, prior_code = "") {
  StanParameter$new(decl = decl, prior_code = prior_code)
}


#' Create a [StanTransformation] object
#'
#' @param decl The Stan variable declaration for the quantity.
#'  Must be an object that inherits from [StanDeclaration].
#' @param origin Must be either `"data"`, `"param"`, or `"model"`.
#' These correspond to the `transformed data`, `transformed parameters`, and
#' `generated quantities` blocks, respectively.
#' @param code A string of Stan code that defines how the quantity is
#' computed from other variables/parameters.
#' The default is empty string (no definition).
#' @family Stan variable declaration functions
#' @export
#' @examples
#' N <- stan_dim("N")
#' D <- stan_dim("D")
#' decl <- stan_array("y", dims = list(N, D), type = "int")
#' code <- "
#' for(n in 1:N) {
#'   for(d in 1:D) {
#'     y[n, d] ~ poisson_rng(0.2);
#'   }
#' }"
#' y <- stan_transform(decl, code = code)
#' print(y)
stan_transform <- function(decl, origin = "model", code = "") {
  StanTransformation$new(decl = decl, origin = origin, code = code)
}

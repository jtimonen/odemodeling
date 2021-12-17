#' Create a `StanDimension` object
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

#' Create a `StanVariable` object.
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

#' Create a `StanVector` object.
#'
#' @param name name of the vector
#' @param lower lower bound
#' @param upper upper bound
#' @param length length of the vector, must be a `StanDimension` object
#' @family Stan variable declaration functions
#' @export
#' @examples
#' x <- stan_vector("x", length = stan_dim("N", lower=1), lower = 0)
#' print(x)
stan_vector <- function(name, length, lower = NULL, upper = NULL) {
  StanVector$new(name = name, length = length, lower = lower, upper = upper)
}

#' Create a `StanMatrix` object.
#'
#' @param name name of the matrix
#' @param lower lower bound
#' @param upper upper bound
#' @param nrow number of rows, must be a `StanDimension` object
#' @param ncol number of columns, must be a `StanDimension` object
#' @family Stan variable declaration functions
#' @export
stan_matrix <- function(name, nrow, ncol, lower = NULL, upper = NULL) {
  StanMatrix$new(
    name = name, nrow = nrow, ncol = ncol,
    lower = lower, upper = upper
  )
}

#' Create a `StanArray` object.
#'
#' @param name name of the array
#' @param lower lower bound
#' @param upper upper bound
#' @param dims list of array dimensions, must be a list of
#'  `StanDimension` objects
#' @param type base type of the array
#' @family Stan variable declaration functions
#' @export
stan_array <- function(name, dims, type = "real", lower = NULL, upper = NULL) {
  StanArray$new(
    name = name, dims = dims, type = type,
    lower = lower, upper = upper
  )
}

#' Create a `StanVectorArray` object.
#'
#' @param name name of the vector array
#' @param lower lower bound
#' @param upper upper bound
#' @param dims list of array dimensions, must be a list of
#'  `StanDimension` objects
#' @param length length of the vector, must be a `StanDimension` object
#' @family Stan variable declaration functions
#' @export
stan_vector_array <- function(name, dims, length, lower = NULL, upper = NULL) {
  StanVectorArray$new(
    name = name, dims = dims, length = length,
    lower = lower, upper = upper
  )
}

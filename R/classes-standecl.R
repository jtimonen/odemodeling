
#' A dimension for a Stan vector or array
#'
#' @field name name of the dimension variable
#' @field lower lower bound
#' @field upper upper bound
StanDimension <- R6::R6Class("StanDimension", list(
  name = NULL,
  lower = NULL,
  upper = NULL,

  #' @description
  #' Create a `StanDimension` object
  #'
  #' @param name name of the dimension variable
  #' @param lower lower bound
  #' @param upper upper bound
  initialize = function(name, lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    if (!is.null(lower)) {
      checkmate::assert_integerish(lower, lower = 0)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_integerish(upper, lower = 0)
      self$upper <- upper
    }
    self$name <- name
  },

  #' @description
  #' Create a data block declaration for the dimension.
  declaration = function() {
    decl <- "int"
    decl <- add_bounds(decl, self$lower, self$upper)
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))


#' A Stan variable
#'
#' @field name name of the variable
#' @field lower lower bound
#' @field upper upper bound
#' @field type type of the variable
StanVariable <- R6::R6Class("StanVariable", list(
  name = NULL,
  lower = NULL,
  upper = NULL,
  type = NULL,

  #' @description
  #' Create a `StanVariable` object.
  #'
  #' @param name name of the dimension variable
  #' @param lower lower bound
  #' @param upper upper bound
  #' @param type variable type (real or int)
  initialize = function(name, type = "real", lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    if (!is.null(lower)) {
      checkmate::assert_numeric(lower, lower = 0)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_numeric(upper, lower = 0)
      self$upper <- upper
    }
    checkmate::assert_choice(type, choices = c("real", "int"))
    self$name <- name
    self$lower <- lower
    self$upper <- upper
    self$type <- type
  },

  #' @description
  #' Create a declaration for the variable
  #'
  #' @return a string
  declaration = function() {
    decl <- self$type
    decl <- add_bounds(decl, self$lower, self$upper)
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))


#' A Stan vector
#'
#' @field name name of the vector
#' @field lower lower bound
#' @field upper upper bound
#' @field length length of the vector
StanVector <- R6::R6Class("StanVector", list(
  name = NULL,
  lower = NULL,
  upper = NULL,
  length = NULL,

  #' @description
  #' Create a `StanVector` object.
  #'
  #' @param name name of the vector
  #' @param lower lower bound
  #' @param upper upper bound
  #' @param length length of the vector, must be a `StanDimension` object
  initialize = function(name, length, lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    checkmate::assert_class(length, "StanDimension")
    if (!is.null(lower)) {
      checkmate::assert_numeric(lower)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_numeric(upper)
      self$upper <- upper
    }
    self$name <- name
    self$lower <- lower
    self$upper <- upper
    self$length <- length
  },

  #' @description
  #' Create a declaration for the vector
  #'
  #' @return a string
  declaration = function() {
    decl <- paste0("vector")
    decl <- add_bounds(decl, self$lower, self$upper)
    decl <- paste0(decl, "[", self$length$name, "]")
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))


#' A Stan matrix
#'
#' @field name name of the matrix
#' @field lower lower bound
#' @field upper upper bound
#' @field nrow number of rows
#' @field ncol number of columns
StanMatrix <- R6::R6Class("StanMatrix", list(
  name = NULL,
  lower = NULL,
  upper = NULL,
  nrow = NULL,
  ncol = NULL,

  #' @description
  #' Create a `StanMatrix` object.
  #'
  #' @param name name of the matrix
  #' @param lower lower bound
  #' @param upper upper bound
  #' @param nrow number of rows, must be a `StanDimension` object
  #' @param ncol number of columns, must be a `StanDimension` object
  initialize = function(name, nrow, ncol, lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    checkmate::assert_class(nrow, "StanDimension")
    checkmate::assert_class(ncol, "StanDimension")
    if (!is.null(lower)) {
      checkmate::assert_numeric(lower)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_numeric(upper)
      self$upper <- upper
    }
    self$name <- name
    self$lower <- lower
    self$upper <- upper
    self$nrow <- nrow
    self$ncol <- ncol
  },

  #' @description
  #' Create a declaration for the matrix
  #'
  #' @return a string
  declaration = function() {
    decl <- "matrix"
    decl <- add_bounds(decl, self$lower, self$upper)
    decl <- paste0(decl, "[", self$nrow$name, ", ", self$ncol$name, "]")
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))


#' A Stan array
#'
#' @field name name of the array
#' @field lower lower bound
#' @field upper upper bound
#' @field dims list of array dimensions
#' @field type base type of the array
StanArray <- R6::R6Class("StanArray", list(
  name = NULL,
  lower = NULL,
  upper = NULL,
  dims = list(),
  type = NULL,

  #' @description
  #' Create a `StanArray` object.
  #'
  #' @param name name of the array
  #' @param lower lower bound
  #' @param upper upper bound
  #' @param dims list of array dimensions, must be a list of
  #'  `StanDimension` objects
  #' @param type base type of the array
  initialize = function(name, dims, type = "real", lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    checkmate::assert_list(dims, types = "StanDimension", min.len = 1)
    if (!is.null(lower)) {
      checkmate::assert_numeric(lower)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_numeric(upper)
      self$upper <- upper
    }
    checkmate::assert_choice(type, choices = c("real", "int"))
    self$name <- name
    self$lower <- lower
    self$upper <- upper
    self$dims <- dims
    self$type <- type
  },

  #' @description
  #' Create a declaration for the array
  #'
  #' @return a string
  declaration = function() {
    decl <- "array["
    j <- 0
    for (dim in self$dims) {
      j <- j + 1
      if (j == 1) {
        decl <- paste0(decl, dim$name)
      } else {
        decl <- paste0(decl, ", ", dim$name)
      }
    }
    decl <- paste0(decl, "] ", self$type)
    decl <- add_bounds(decl, self$lower, self$upper)
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))



#' A Stan array of vectors
#'
#' @field name name of the array
#' @field lower lower bound
#' @field upper upper bound
#' @field dims list of array dimensions
#' @field length length of the vector
StanVectorArray <- R6::R6Class("StanVectorArray", list(
  name = NULL,
  lower = NULL,
  upper = NULL,
  dims = list(),
  length = NULL,

  #' @description
  #' Create a `StanVectorArray` object.
  #'
  #' @param name name of the vector array
  #' @param lower lower bound
  #' @param upper upper bound
  #' @param dims list of array dimensions, must be a list of
  #'  `StanDimension` objects
  #' @param length length of the vector, must be a `StanDimension` object
  initialize = function(name, dims, length, lower = NULL, upper = NULL) {
    checkmate::assert_string(name, min.chars = 1)
    checkmate::assert_list(dims, types = "StanDimension", min.len = 1)
    checkmate::assert_class(length, "StanDimension")
    if (!is.null(lower)) {
      checkmate::assert_numeric(lower)
      self$lower <- lower
    }
    if (!is.null(upper)) {
      checkmate::assert_numeric(upper)
      self$upper <- upper
    }
    self$name <- name
    self$lower <- lower
    self$upper <- upper
    self$dims <- dims
    self$length <- length
  },

  #' @description
  #' Create a declaration for the array of vectors
  #'
  #' @return a string
  declaration = function() {
    decl <- "array["
    j <- 0
    for (dim in self$dims) {
      j <- j + 1
      if (j == 1) {
        decl <- paste0(decl, dim$name)
      } else {
        decl <- paste0(decl, ", ", dim$name)
      }
    }
    decl <- paste0(decl, "] vector")
    decl <- add_bounds(decl, self$lower, self$upper)
    decl <- paste0(decl, "[", self$length$name, "]")
    paste0(decl, " ", self$name, ";")
  },

  #' @description
  #' Print
  print = function() {
    cat(self$declaration(), "\n")
    invisible(self)
  }
))

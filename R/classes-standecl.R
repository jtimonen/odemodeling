
#' A Stan variable declaration (an abstract base class)
#'
#' @field name name of the variable
#' @field lower lower bound
#' @field upper upper bound
StanDeclaration <- R6::R6Class("StanDeclaration",
  public = list(
    name = NULL,
    lower = NULL,
    upper = NULL,

    #' @description
    #' `StanDeclaration` is an abstract class that can't be initialized.
    initialize = function() {
      stop("StanDeclaration is an abstract class that can't be initialized.")
    },

    #' @description
    #' The variable declaration as a string.
    declaration = function() {

    },

    #' @description
    #' The variable when used in function signature
    signature = function() {

    },

    #' @description
    #' Print
    print = function() {
      code <- paste0(self$declaration(), ";\n")
      cat_stancode(code)
      invisible(self)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      FALSE
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      list()
    }
  )
)

# Check that bound is positive integer or NULL
check_bound_posint <- function(bound) {
  if (!is.null(bound)) {
    checkmate::assert_integerish(bound, lower = 0)
  }
  bound
}

# Check that bound is a single number or NULL
check_bound_num <- function(bound) {
  if (!is.null(bound)) {
    checkmate::assert_number(bound)
  }
  bound
}

#' A dimension for a Stan vector or array
StanDimension <- R6::R6Class("StanDimension",
  inherit = StanDeclaration,
  public = list(

    #' @description
    #' Create a [StanDimension] object
    #'
    #' @param name name of the dimension variable
    #' @param lower lower bound
    #' @param upper upper bound
    initialize = function(name, lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      self$lower <- check_bound_posint(lower)
      self$upper <- check_bound_posint(upper)
      self$name <- name
    },

    #' @description
    #' Create a data block declaration for the dimension.
    declaration = function() {
      decl <- "int"
      decl <- add_bounds(decl, self$lower, self$upper)
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      paste0("int ", self$name)
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      list(self)
    }
  )
)


#' A Stan variable
#'
#' @field type type of the variable
StanVariable <- R6::R6Class("StanVariable",
  inherit = StanDeclaration,
  public = list(
    type = NULL,

    #' @description
    #' Create a [StanVariable] object.
    #'
    #' @param name name of the dimension variable
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param type variable type (real or int)
    initialize = function(name, type = "real", lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      self$lower <- check_bound_num(lower)
      self$upper <- check_bound_num(upper)
      checkmate::assert_choice(type, choices = c("real", "int"))
      self$name <- name
      self$type <- type
    },

    #' @description
    #' Create a declaration for the variable
    #'
    #' @return a string
    declaration = function() {
      decl <- self$type
      decl <- add_bounds(decl, self$lower, self$upper)
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      paste0(self$type, " ", self$name)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      self$type == "real"
    }
  )
)


#' A Stan vector
#'
#' @field length length of the vector
StanVector <- R6::R6Class("StanVector",
  inherit = StanDeclaration,
  public = list(
    length = NULL,

    #' @description
    #' Create a [StanVector] object.
    #'
    #' @param name name of the vector
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param length length of the vector, must be a [StanDimension] object
    initialize = function(name, length, lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_class(length, "StanDimension")
      self$lower <- check_bound_num(lower)
      self$upper <- check_bound_num(upper)
      self$name <- name
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
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      paste0("vector ", self$name)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      TRUE
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      list(self$length)
    }
  )
)


#' A Stan matrix
#'
#' @field nrow number of rows
#' @field ncol number of columns
StanMatrix <- R6::R6Class("StanMatrix",
  inherit = StanDeclaration,
  public = list(
    nrow = NULL,
    ncol = NULL,

    #' @description
    #' Create a [StanMatrix] object.
    #'
    #' @param name name of the matrix
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param nrow number of rows, must be a [StanDimension] object
    #' @param ncol number of columns, must be a [StanDimension] object
    initialize = function(name, nrow, ncol, lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_class(nrow, "StanDimension")
      checkmate::assert_class(ncol, "StanDimension")
      self$lower <- check_bound_num(lower)
      self$upper <- check_bound_num(upper)
      self$name <- name
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
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      paste0("matrix ", self$name)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      TRUE
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      list(self$nrow, self$ncol)
    }
  )
)


#' A Stan array
#'
#' @field dims list of array dimensions
#' @field type base type of the array
StanArray <- R6::R6Class("StanArray",
  inherit = StanDeclaration,
  public = list(
    dims = list(),
    type = NULL,

    #' @description
    #' Create a [StanArray] object.
    #'
    #' @param name name of the array
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param dims list of array dimensions, must be a list of
    #' [StanDimension] objects
    #' @param type base type of the array
    initialize = function(name, dims, type = "real",
                          lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_list(dims, types = "StanDimension", min.len = 1)
      self$lower <- check_bound_num(lower)
      self$upper <- check_bound_num(upper)
      checkmate::assert_choice(type, choices = c("real", "int"))
      self$name <- name
      self$dims <- dims
      self$type <- type
    },

    #' @description
    #' Create a declaration for the array
    #'
    #' @return a string
    declaration = function() {
      decl <- declare_array(self$name, self$dims, signature = FALSE)
      decl <- paste0(decl, " ", self$type)
      decl <- add_bounds(decl, self$lower, self$upper)
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      decl <- declare_array(self$name, self$dims, signature = TRUE)
      decl <- paste0(decl, " ", self$type)
      paste0(decl, " ", self$name)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      self$type == "real"
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      self$dims
    }
  )
)



#' A Stan array of vectors
#'
#' @field dims list of array dimensions
#' @field length length of the vector
StanVectorArray <- R6::R6Class("StanVectorArray",
  inherit = StanDeclaration,
  public = list(
    dims = list(),
    length = NULL,

    #' @description
    #' Create a [StanVectorArray] object.
    #'
    #' @param name name of the vector array
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param dims list of array dimensions, must be a list of
    #' [StanDimension] objects
    #' @param length length of the vector, must be a [StanDimension] object
    initialize = function(name, dims, length, lower = NULL, upper = NULL) {
      checkmate::assert_string(name, min.chars = 1)
      checkmate::assert_list(dims, types = "StanDimension", min.len = 1)
      checkmate::assert_class(length, "StanDimension")
      self$lower <- check_bound_num(lower)
      self$upper <- check_bound_num(upper)
      self$name <- name
      self$dims <- dims
      self$length <- length
    },

    #' @description
    #' Create a declaration for the array of vectors
    #'
    #' @return a string
    declaration = function() {
      decl <- declare_array(self$name, self$dims, signature = FALSE)
      decl <- paste0(decl, " ", "vector")
      decl <- add_bounds(decl, self$lower, self$upper)
      decl <- paste0(decl, "[", self$length$name, "]")
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      decl <- declare_array(self$name, self$dims, signature = TRUE)
      decl <- paste0(decl, " ", "vector")
      paste0(decl, " ", self$name)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      FALSE # not sure should this be TRUE
    },

    #' @description
    #' Get all declared dimensions related to the object.
    #' @return A list.
    get_dims = function() {
      c(self$dims, list(self$length))
    }
  )
)


#' A Stan parameter
#'
#' @field decl The variable declaration.
#' @field prior_code The prior declaration.
StanParameter <- R6::R6Class("StanParameter",
  public = list(
    decl = NULL,
    prior_code = NULL,

    #' @description
    #' Create a [StanParameter] object.
    #'
    #' @param decl The underlying variable.
    #' @param prior 'Stan' code that defines prior for the parameter.
    initialize = function(decl, prior = "") {
      checkmate::assert_class(decl, "StanDeclaration")
      checkmate::assert_true(decl$can_be_made_parameter())
      checkmate::assert_string(prior, min.chars = 0)
      code <- trimws(prior)
      code <- add_semicolon_if_missing(code)
      if (nchar(code) > 0) {
        code <- paste0(decl$name, " ~ ", code)
      }
      self$decl <- decl
      self$prior_code <- code
    },

    #' @description
    #' Print
    print = function() {
      cat("Parameter: ")
      self$decl$print()
      if (nchar(self$prior_code) > 0) {
        cat("Prior code: ")
        cat_stancode(self$prior_code)
        cat("\n")
      } else {
        cat("No prior set.\n")
      }
      invisible(self)
    }
  )
)


#' A Stan transformation
#'
#' @field decl The variable declaration.
#' @field code The code that assigns to the declared variable.
#' @field origin Either `"data"`, `"parameters"`, or `"model"`.
StanTransformation <- R6::R6Class("StanTransformation",
  public = list(
    decl = NULL,
    code = NULL,
    origin = NULL,

    #' @description
    #' Create a [StanTransformation] object.
    #'
    #' @param decl The underlying variable.
    #' @param origin Must be either `"data"`, `"parameters"`, or `"model"`.
    #' @param code The code that assigns to the declared variable. If this
    #' doesn't include any assignments (with `=`), the code is prepended
    #' with `paste0(decl$name, " = ")`. In this case also a semicolon is
    #' added to the end if it is missing.
    initialize = function(decl, origin = "model", code = "") {
      checkmate::assert_class(decl, "StanDeclaration")
      checkmate::assert_choice(origin, c("data", "parameters", "model"))
      checkmate::assert_string(code, min.chars = 0)
      has_assign <- grepl("=", code, fixed = TRUE)
      if (has_assign) {
        code <- trimws(code, which = "right")
      } else {
        code <- trimws(code)
        code <- add_semicolon_if_missing(code)
        code <- paste0(decl$name, " = ", code)
      }
      self$decl <- decl
      self$origin <- origin
      self$code <- code
    },

    #' @description
    #' Print
    print = function() {
      if (self$origin == "data") {
        type_desc <- "Transformed data"
      } else if (self$origin == "parameters") {
        type_desc <- "Transformed parameter"
      } else {
        type_desc <- "Generated quantity"
      }
      cat(type_desc, ": ", sep = "")
      self$decl$print()
      if (nchar(self$code) > 0) {
        cat("Code: ")
        cat_stancode(self$code)
        cat("\n")
      } else {
        cat("\nCode not defined.\n")
      }
      invisible(self)
    }
  )
)


#' A Stan variable declaration (base class)
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
    #' The variable declaration as a string.
    declaration = function() {
      return("UNDEFINED")
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      return("UNDEFINED")
    },

    #' @description
    #' Print
    print = function() {
      cat("Variable: \n")
      cat(" - name:", self$name, "\n")
      cat(" - declaration:", self$declaration(), "\n")
      cat(" - function signature:", self$signature(), "\n")
      invisible(self)
    },

    #' @description
    #' Can the object be made into a parameter?
    can_be_made_parameter = function() {
      FALSE
    }
  )
)

#' A dimension for a Stan vector or array
StanDimension <- R6::R6Class("StanDimension",
  inherit = StanDeclaration,
  public = list(

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
      paste0(decl, " ", self$name)
    },

    #' @description
    #' The variable when used in function signature
    signature = function() {
      paste0("int ", self$name)
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
    #' Create a `StanArray` object.
    #'
    #' @param name name of the array
    #' @param lower lower bound
    #' @param upper upper bound
    #' @param dims list of array dimensions, must be a list of
    #'  `StanDimension` objects
    #' @param type base type of the array
    initialize = function(name, dims, type = "real",
                          lower = NULL, upper = NULL) {
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
    }
  )
)


#' A Stan parameter
#'
#' @field var The variable declaration.
#' @field prior_code The prior declaration.
StanParameter <- R6::R6Class("StanParameter",
  public = list(
    var = NULL,
    prior_code = NULL,

    #' @description
    #' Create a `StanParameter` object.
    #'
    #' @param var The underlying variable.
    #' @param prior_code Code that defines prior for the parameter.
    initialize = function(var, prior_code = "") {
      checkmate::assert_class(var, "StanDeclaration")
      checkmate::assert_true(var$can_be_made_parameter())
      checkmate::assert_string(prior_code, min.chars = 0)
      self$var <- var
      self$prior_code <- prior_code
    },

    #' @description
    #' Print
    print = function() {
      cat("Parameter: ")
      self$var$print()
      if (nchar(self$prior_code) > 0) {
        cat("\nPrior code:\n")
        cat(self$prior_code)
        cat("\n")
      } else {
        cat("\nNo prior set.\n")
      }
      invisible(self)
    }
  )
)


#' A Stan transformation
#'
#' @field var The variable declaration.
#' @field code The code that assigns to the declared variable.
StanTransformation <- R6::R6Class("StanTransformation",
  public = list(
    var = NULL,
    code = NULL,

    #' @description
    #' Create a `StanTransformation` object.
    #'
    #' @param var The underlying variable.
    #' @param code The code that assigns to the declared variable.
    initialize = function(var, code = "") {
      checkmate::assert_class(var, "StanDeclaration")
      checkmate::assert_string(code, min.chars = 0)
      self$var <- var
      self$code <- code
    },

    #' @description
    #' Print
    print = function() {
      cat("Transformation: ")
      self$var$print()
      if (nchar(self$code) > 0) {
        cat("\nCode:\n")
        cat(self$code)
        cat("\n")
      } else {
        cat("\nCode not defined.\n")
      }
      invisible(self)
    }
  )
)

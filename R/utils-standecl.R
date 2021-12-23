# Helpers for filtering lists of StanDeclarations, StanParameters and
# StanTransforms

# Get decl
get_decl <- function(x) {
  if (is(x, "StanDeclaration")) {
    return(x)
  } else if (is(x, "StanParameter")) {
    return(x$decl)
  } else if (is(x, "StanTransformation")) {
    return(x$decl)
  } else {
    stop("Invalid type for get_decl().")
  }
}

# Get name
get_name <- function(x) {
  if (is(x, "StanDeclaration")) {
    return(x$name)
  } else if (is(x, "StanParameter")) {
    return(x$decl$name)
  } else if (is(x, "StanTransformation")) {
    return(x$decl$name)
  } else {
    stop("Invalid type for get_decl().")
  }
}

# Is data?
is_data <- function(x) {
  is(x, "StanDeclaration")
}

# Is parameter?
is_param <- function(x) {
  is(x, "StanParameter")
}

# Is transformed data?
is_tdata <- function(x) {
  is_t <- is(x, "StanTransformation")
  if (!is_t) {
    return(FALSE)
  }
  x$origin == "data"
}

# Is transformed parameter?
is_tparam <- function(x) {
  is_t <- is(x, "StanTransformation")
  if (!is_t) {
    return(FALSE)
  }
  x$origin == "param"
}

# Is generated quantitity
is_gq <- function(x) {
  is_t <- is(x, "StanTransformation")
  if (!is_t) {
    return(FALSE)
  }
  x$origin == "model"
}

# Get dimensions
get_dims <- function(x) {
  decl <- get_decl(x)
  decl$get_dims()
}

# Get prior code
get_prior_code <- function(x) {
  x$prior_code
}

# Get code
get_code <- function(x) {
  x$code
}

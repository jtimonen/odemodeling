# Create Stan model from model code
stan_model_from_code <- function(code) {
  file <- cmdstanr::write_stan_file(code)
  cmdstanr::cmdstan_model(file)
}

# Solver name to numeric encoding
solver_to_num <- function(solver) {
  ok <- c("rk45", "bdf", "rk4")
  checkmate::assert_choice(solver, ok)
  if (solver == "rk45") {
    return(1)
  }
  if (solver == "bdf") {
    return(2)
  }
  if (solver == "rk4") {
    return(11)
  }
}

# Solver numeric encoding to default args list
default_solver_conf <- function(solver_num) {
  if (solver_num == 1) {
    out <- list(abs_tol = 1e-6, rel_tol = 1e-6, max_num_steps = 1e6)
  } else if (solver_num == 2) {
    out <- list(abs_tol = 1e-10, rel_tol = 1e-10, max_num_steps = 1e9)
  } else if (solver_num == "rk4") {
    out <- list(num_steps = 1)
  } else {
    stop("unknown solver")
  }
  str <- list_to_str(out)
  msg <- paste0("solver_conf was NULL, defaulting to ", str)
  message(msg)
  return(out)
}

# Named list to string
list_to_str <- function(x) {
  str <- paste(names(x), x, sep = "=", collapse = ", ")
  paste0("{", str, "}")
}


# Print colored text
cat_colored <- function(x, col = "\033[95m") {
  if (interactive()) {
    x <- paste0(col, x, "\u001b[0m")
  }
  cat(x)
}

# Print a number
cat_number <- function(x) {
  col <- "\u001b[34;1m"
  cat_colored(x, col = col)
}

# Print Stan code
cat_stancode <- function(x) {
  col <- "\u001b[33m"
  cat_colored(x, col = col)
}

# Read lines from file
read_file_lines <- function(file) {
  a <- readLines(file)
  paste(a, collapse = "\n")
}

# Autoformat a 'Stan' code string
autoformat_stancode <- function(code) {
  tryCatch(
    {
      file <- cmdstanr::write_stan_file(code)
      model <- cmdstanr::cmdstan_model(file, compile = FALSE)
      res <- processx::run(
        file.path(cmdstanr::cmdstan_path(), "bin", "stanc"),
        args = c(model$stan_file(), "--auto-format")
      )
      return(res$stdout)
    },
    error = function(e) {
      cat("\nTried to format following Stan code:\n\n")
      cat(code)
      stop(e)
    }
  )
}

# Add boundaries to variable declaration string
add_bounds <- function(decl, lower, upper) {
  if (!is.null(lower) && !is.null(upper)) {
    add <- paste0("<lower=", lower, ", upper=", upper, ">")
  } else if (!is.null(lower) && is.null(upper)) {
    add <- paste0("<lower=", lower, ">")
  } else if (is.null(lower) && !is.null(upper)) {
    add <- paste0("<upper=", upper, ">")
  } else {
    add <- ""
  }
  paste0(decl, add)
}

# Beginning of array declaration or signature
declare_array <- function(name, dims, signature) {
  decl <- "array["
  j <- 0
  for (dim in dims) {
    if (signature) {
      dimname <- ""
    } else {
      dimname <- dim$name
    }
    j <- j + 1
    if (j == 1) {
      decl <- paste0(decl, dimname)
    } else {
      decl <- paste0(decl, ", ", dimname)
    }
  }
  decl <- paste0(decl, "]")
  return(decl)
}

# Append to comma separated list
append_to_signature <- function(code, add) {
  if (nchar(add) > 0) {
    if (nchar(code) > 0) {
      code <- paste0(code, ", ", add)
    } else {
      code <- add
    }
  }
  return(code)
}

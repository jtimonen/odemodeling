# Create Stan model from model code
stan_model_from_code <- function(code) {
  file <- cmdstanr::write_stan_file(code)
  cmdstanr::cmdstan_model(file)
}

# Print colored text
cat_colored <- function(x, col = "\033[95m") {
  x <- paste0(col, x, "\u001b[0m")
  cat(x)
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
}

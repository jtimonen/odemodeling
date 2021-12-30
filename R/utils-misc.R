# Internal assertion that should never fail
internal_assert_len <- function(vec, expected, source) {
  if (length(vec) != expected) {
    msg <- paste0("Unexpected length in '", source, "'. Please report a bug.")
    stop(msg)
  }
  TRUE
}

# Create Stan model from model code
stan_model_from_code <- function(code) {
  file <- cmdstanr::write_stan_file(code)
  cmdstanr::cmdstan_model(file)
}


# Named list to string
list_to_str <- function(x) {
  str <- paste(names(x), x, sep = "=", collapse = ", ")
  paste0("{", str, "}")
}

# Colorize string
colorize_string <- function(x, col) {
  if (interactive()) {
    x <- paste0(col, x, "\u001b[0m")
  }
  x
}

# Number string
number_string <- function(x) {
  col <- "\u001b[34;1m"
  colorize_string(x, col)
}

# Highlight string
highlight_string <- function(x) {
  col <- "\033[37m"
  colorize_string(x, col)
}

# Stan code string
stancode_string <- function(x) {
  col <- "\u001b[33m"
  colorize_string(x, col)
}

# Print colored text
cat_colored <- function(x, col = "\033[95m") {
  cat(colorize_string(x, col))
}

# Print a number
cat_number <- function(x) {
  cat(number_string(x))
}

# Print Stan code
cat_stancode <- function(x) {
  cat(stancode_string(x))
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

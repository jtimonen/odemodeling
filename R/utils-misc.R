# Create directory if it doesn't exist
create_dir_if_not_exist <- function(dir) {
  if (!dir.exists(dir)) {
    message("directory '", dir, "' doesn't exist, creating it")
    dir.create(dir)
  }
  invisible(dir)
}

# Name ODE dimensions
create_ydim_names <- function(names, D) {
  if (is.null(names)) {
    out <- paste0("y", c(1:D))
  } else {
    checkmate::assert_character(names, len = D)
    out <- names
  }
  return(out)
}

# Add leading comma to arguments string
add_leading_comma <- function(args) {
  args <- trimws(args)
  if (nchar(args) > 0) {
    args <- paste0(", ", args)
  }
  return(args)
}

# Add semicolon if not final character of string
add_semicolon_if_missing <- function(code) {
  code <- trimws(code)
  L <- nchar(code)
  if (L > 0) {
    last_char <- substr(code, L, L)
    if (last_char != ";") {
      code <- paste0(code, ";")
    }
  }
  return(code)
}

# Short class info as string
class_info <- function(class_name) {
  info <- paste0("An object of class ", class_name, ".")
  if (interactive()) {
    info <- paste0(info, " Type ?", class_name, " for help.")
  }
  return(info)
}

# Internal assertion that should never fail
internal_assert_len <- function(vec, expected, source) {
  if (length(vec) != expected) {
    stop(paste0("Unexpected length in '", source, "'. Please report a bug."))
  }
  TRUE
}

# Create Stan model from model code
stan_model_from_code <- function(code) {
  file <- cmdstanr::write_stan_file(code)
  cmdstanr::cmdstan_model(file)
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
  col <- "\u001b[34;1m" # bold blue
  colorize_string(x, col)
}

# Stan code string
stancode_string <- function(x) {
  col <- "\u001b[33m" # orange
  colorize_string(x, col)
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

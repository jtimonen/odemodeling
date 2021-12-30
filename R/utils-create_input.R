
# Create full Stan data for sampling or gq
create_standata <- function(model, t0, t, solver, solver_conf) {
  checkmate::assert_class(model, "OdeModel")
  checkmate::assertNumber(t0)
  checkmate::assert_vector(t)
  checkmate::assert_numeric(t)
  checkmate::assert_string(solver)
  if (any(t <= t0)) {
    stop("each value in t must be strictly larger than given t0!")
  }

  # Validate or create default solver configuration
  final_conf <- validate_solver_conf(solver, solver_conf)
  solver_conf <- final_conf$solver_conf

  # Create and return full Stan data
  N <- list(N = length(t))
  names(N) <- model$t_dim$name
  c(
    N,
    list(t0 = t0, t = t, solver = final_conf$solver_num),
    solver_conf
  )
}

# Create solver configuration if not given and validate if given
validate_solver_conf <- function(solver, solver_conf) {
  if (is.null(solver_conf)) {
    solver_num <- solver_to_num(solver)
    solver_conf <- default_solver_conf(solver_num)
  }
  solver_num <- solver_to_num(solver)
  if (solver_num <= 10) {
    MAX_INT <- 2^31 - 1
    nams <- c("abs_tol", "rel_tol", "max_num_steps")
    checkmate::assert_list(solver_conf)
    checkmate::assert_set_equal(names(solver_conf), nams)
    mns <- solver_conf$max_num_steps
    checkmate::assert_integerish(mns, lower = 1, upper = MAX_INT)
    dummy <- list(num_steps = 1)
    solver_args <- c(solver_conf, dummy)
  } else {
    checkmate::assert_list(solver_conf)
    checkmate::assert_set_equal(names(solver_conf), "num_steps")
    dummy <- list(abs_tol = 1, rel_tol = 1, max_num_steps = 1)
    solver_args <- c(solver_conf, dummy)
  }

  # Return
  list(
    solver_num = solver_num,
    solver_conf = solver_args
  )
}

# Return y if x is NULL, else return x
replace_if_null <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
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

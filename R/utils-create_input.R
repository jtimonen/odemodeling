# Create full Stan data for sampling or gq
create_standata <- function(model, t0, t, solver) {
  checkmate::assert_class(model, "OdeModel")
  checkmate::assertNumber(t0)
  checkmate::assert_vector(t)
  checkmate::assert_numeric(t)
  checkmate::assert_class(solver, "OdeSolver")
  if (any(t <= t0)) {
    stop("each value in t must be strictly larger than given t0!")
  }

  # Create and return full Stan data
  N <- list(N = length(t))
  names(N) <- model$t_dim$name
  c(
    N,
    list(t0 = t0, t = t),
    solver$standata()
  )
}

# Replace single_tol by tol if it is not NULL
replace_tol <- function(single_tol, tol) {
  if (!is.null(tol)) {
    single_tol <- tol
  }
  single_tol
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
  ok <- c("rk45", "bdf", "adams", "ckrk", "euler", "midpoint", "rk4")
  nums <- c(1, 2, 3, 4, 101, 102, 103)
  checkmate::assert_choice(solver, ok)
  nums[which(ok == solver)]
}

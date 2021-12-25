#' Create a list of solver configurations (for adaptive stepsize solvers)
#'
#' @export
#' @param tols a numeric vector of tolerances (abs tol and rel tol will be same)
#' @param max_num_steps maximum number of steps (same in all confs)
create_solver_conf_list <- function(tols, max_num_steps) {
  checkmate::assert_numeric(tols)
  checkmate::assert_integerish(max_num_steps)
  L <- length(tols)
  out <- list()
  for (j in seq_len(L)) {
    out[[j]] <- list(
      abs_tol = tols[j],
      rel_tol = tols[j],
      max_num_steps = max_num_steps
    )
  }
  return(out)
}

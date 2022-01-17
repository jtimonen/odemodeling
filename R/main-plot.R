#' Plotting different metrics
#'
#' @description Either `tols` or `num_steps` has to be `NULL`.
#' @param values A numeric vector of metric values.
#' @param name Name of metric
#' @param tols A numeric vector of tolerances. Must have same length as
#' `values`.
#' @param num_steps A numeric vector of step amounts. Must have same length as
#' `values`.
#' @param reliability The list returned by the `$reliability()` method of
#' [OdeModelFit] class.
#' @name plot_metric
NULL

#' @describeIn plot_metric Plot a generic metric.
#' @export
plot_metric <- function(values, name, tols = NULL, num_steps = NULL) {
  checkmate::assert_numeric(values)
  checkmate::assert_string(name)
  if (!is.null(tols)) {
    if (!is.null(num_steps)) {
      stop("either tols or num_steps must be NULL")
    }
    confs <- 1 / tols
    conf_name <- "inv_tol"
    log10 <- TRUE
  } else {
    if (is.null(num_steps)) {
      stop("either tols or num_steps must not be NULL")
    }
    confs <- num_steps
    conf_name <- "num_steps"
    log10 <- FALSE
  }
  plt <- plot_metric.create_plot(confs, values, conf_name, name, log10)
  if (log10) {
    plt <- add_inv_tol_xlab(plt)
  }
  return(plt)
}

#' @describeIn plot_metric Plot pareto-k metric.
#' @export
plot_pareto_k <- function(reliability, tols = NULL, num_steps = NULL) {
  values <- reliability$metrics[, "pareto_k"]
  plt <- plot_metric(values, "pareto_k", tols, num_steps)
  plt <- plt + geom_hline(yintercept = 0.5, lty = 2, color = "firebrick3")
  plt <- plt + geom_hline(yintercept = 0.7, lty = 2, color = "steelblue")
  plt + ylab("Pareto-k")
}

#' @describeIn plot_metric Plot relative efficiency.
#' @export
plot_r_eff <- function(reliability, tols = NULL, num_steps = NULL) {
  values <- reliability$metrics[, "pareto_k"]
  plt <- plot_metric(values, "r_eff", tols, num_steps)
  plt + ylab("Relative efficiency")
}

#' @describeIn plot_metric Plot maximum absolute difference.
#' @param loglik If `TRUE`, the maximum absolute difference in log likelihoods
#' is plotted. Otherwise the maximum absolute difference in ODE solutions
#' is plotted (default).
#' @export
plot_mad <- function(reliability, tols = NULL, num_steps = NULL,
                     loglik = FALSE) {
  if (loglik) {
    name <- "mad_loglik"
  } else {
    name <- "mad_odesol"
  }
  values <- reliability$metrics[, name]
  plt <- plot_metric(values, name, tols, num_steps)
  plt + ylab(name)
}

# Edit x label
add_inv_tol_xlab <- function(plt) {
  plt + xlab(expression(tol^{
    -1
  }))
}

# Plotting helper function
plot_metric.create_df <- function(confs, values, conf_name, metric_name) {
  checkmate::assert_numeric(confs, len = length(values))
  df <- data.frame(confs, values)
  colnames(df) <- c(conf_name, metric_name)
  return(df)
}

# Plotting helper function
plot_metric.create_plot <- function(confs, values, conf_name, metric_name,
                                    log10) {
  df <- plot_metric.create_df(confs, values, conf_name, metric_name)
  plt <- ggplot(df, aes_string(x = conf_name, y = metric_name))
  plt <- plt + geom_line() + geom_point() + theme_bw()
  if (log10) {
    plt <- plt + scale_x_log10(breaks = confs)
  } else {
    plt <- plt + scale_x_continuous(breaks = confs)
  }
  if (log10) {
    plt <- plt + theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  }
  plt + theme(
    panel.grid.minor = element_blank()
  )
}

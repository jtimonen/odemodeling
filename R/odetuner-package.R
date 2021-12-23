#' The 'odetuner' package
#'
#' @description Efficient tuning and validation of solver tolerances in Bayesian
#' ODE models using Pareto-smoothed importance sampling (PSIS) and
#' its diagnostics.
#'
#' @docType package
#' @name odetuner-package
#' @aliases odetuner
#'
#' @import R6
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils capture.output
#'
#' @section Main functions:
#' \itemize{
#'   \item Create a model using [create_odemodel].
#'   \item Create the input needed for [create_odemodel] using
#'   [stan_array()], [stan_dim()], [stan_matrix()], [stan_param()],
#'   [stan_transform()], [stan_vector_array()], and [stan_vector()].
#'   \item Sample the posterior or prior distribution of the model parameters,
#'   and generate corresponding ODE solutions using [sample_odemodel].
#' }
#'
NULL

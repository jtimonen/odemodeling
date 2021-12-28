#' The 'odemodeling' package
#'
#' @description Building and fitting models with different numerical
#' solvers using Stan. Includes efficient tuning and validation of
#' numerical solvers using Pareto-smoothed importance sampling (PSIS) and
#' its diagnostics.
#'
#' @docType package
#' @name odemodeling-package
#' @aliases odemodeling
#'
#' @import R6
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils capture.output
#'
#' @section Creating a model:
#' \itemize{
#'   \item Declare model data, parameters, and other variables using
#'   [stan_array()], [stan_dim()], [stan_matrix()], [stan_param()],
#'   [stan_transform()], [stan_vector_array()], and [stan_vector()].
#'   \item Create an [OdeModel] model using [create_odemodel()].
#' }
#' @section Fitting a model:
#' \itemize{
#'   \item Sample the posterior or prior distribution of the model parameters,
#'   and generate corresponding ODE solutions using [sample_odemodel()].
#'   \item See methods of the [OdeModelFit] class for studying the returned
#'   object.
#' }
#'
NULL
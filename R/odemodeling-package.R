#' The 'odemodeling' package
#'
#' @description Building and fitting ordinary differential equation (ODE)
#' models with different numerical solvers in 'Stan'. Designed for efficient
#' validation of the accuracy of numerical solvers in the Bayesian context.
#' Using Pareto-smoothed importance sampling (PSIS) and its diagnostics.
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
#'   \item Create an [OdeModel] model using [ode_model()].
#' }
#' @section Using different ODE solvers:
#' \itemize{
#'    \item See \code{\link{odesolvers}} for constructors that create objects
#'    of class [OdeSolver].
#' }
#' @section Fitting a model:
#' \itemize{
#'   \item Sample the posterior or prior distribution of the model parameters,
#'   and generate corresponding ODE solutions using the `$sample()` method
#'   of the [OdeModel] class.
#'   \item See methods of the [OdeModelMCMC] class for studying the returned
#'   object.
#' }
#' @section Additional simulation of ODE systems:
#' \itemize{
#'   \item See the `$gqs()` method of the [OdeModelMCMC] and [OdeModel]
#'   classes.
#'   \item See methods of the [OdeModelGQ] class for studying the returned
#'   object.
#'   \item See \code{\link{compare_odefits}} for functions to compare
#'   different ODE model simulations and fits.
#' }
#' @section Tutorial:
#' See the tutorial vignette.
#'
NULL


#' Hudson Bay Company lynx and hare pelt collection data (1900-1920)
#'
#' @format A data frame with 21 rows and 3 variables:
#' \describe{
#'   \item{year}{collection year}
#'   \item{lynx}{number of lynx pelts collected (in 1000's)}
#'   \item{hare}{number of hare pelts collected (in 1000's)}
#' }
#' @source \url{https://www.math.tamu.edu/~phoward/m442/modbasics.pdf},
#' downloaded 10th Jan, 2021.
"lynxhare"

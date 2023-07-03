#' The 'odemodeling' package
#'
#' @description Building and fitting ordinary differential equation (ODE)
#' models with different numerical solvers in 'Stan'. Designed for efficient
#' validation of the accuracy of numerical solvers in the Bayesian context.
#' Using Pareto-smoothed importance sampling (PSIS) and its diagnostics.
#' The package is based on the \code{R6} object oriented system and uses
#' \code{cmdstanr} as the interface to 'Stan'.
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
#' @section Importance sampling for reliable and efficient inference in Bayesian ODE models:
#' Our proposed workflow is to
#' \enumerate{
#' \item Select an initial ODE solver M.
#' \item Sample the parameter posterior using MCMC with M as the ODE solver.
#' \item  Compute certain metrics using a more accurate solver M∗.
#' \item Increase the accuracy of M∗ and repeat Step 3 until the metrics converge.
#'  If the Pareto-k metric converges to a value larger than 0.7, increase the accuracy
#' of M and go back to Step 2.
#' \item Compute any posterior estimates using final importance weights. See
#' Timonen et al. (2022) below.
#' }
#' The algorithm can be used to validate the reliability, and correct the errors
#' of a given method M, which can be for example a software default. On the other hand,
#' a smart initial selection of M can provide speed gains
#' compared to often rather conservatively set software defaults, while still maintaining
#' reliability of the inferences.
#'
#' We generally recommend selecting M initially so that sampling is as fast as possible.
#' For example, for non-adaptive ODE solvers, one can ﬁrst try using the smallest
#' sensible number of steps that does not result in immediate failure.
#' Selecting a good M is more diﬃcult in the case of adaptive solvers.
#' We have
#' observed that tolerances on the order of 1e-4 to 1e-3 generally work well.
#'
#' See the \code{reliability()} method of the \code{\link{OdeModelMCMC}} class.
#'
#' @section Tutorial:
#' See the tutorial vignette.
#' @references
#' \enumerate{
#'   \item Timonen, J. et al. (2022).
#'   \emph{An importance sampling approach for reliable and efficient
#'   inference in Bayesian ordinary differential equation models}.
#'   \href{https://arxiv.org/abs/2205.09059}{arXiv}.
#' }
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

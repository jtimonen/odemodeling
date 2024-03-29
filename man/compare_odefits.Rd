% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main-functions.R
\name{compare_odefits}
\alias{compare_odefits}
\alias{max_abs_odesol_diff}
\alias{max_abs_loglik_diff}
\alias{log_ratios}
\alias{psis_relative_eff}
\alias{psis}
\alias{compute_reliability_metrics}
\title{Compare two objects that contain ODE solutions and ODE model likelihoods}
\usage{
max_abs_odesol_diff(x, y, include_y0 = TRUE)

max_abs_loglik_diff(x, y)

log_ratios(x, y)

psis_relative_eff(x, y)

psis(x, y)

compute_reliability_metrics(x, y)
}
\arguments{
\item{x}{An object of class \link{OdeModelMCMC} or \link{OdeModelGQ}.}

\item{y}{An object of class \link{OdeModelMCMC} or \link{OdeModelGQ}.}

\item{include_y0}{Should the ODE initial state be included in computations?}
}
\value{
A named numeric vector.
}
\description{
Compare two objects that contain ODE solutions and ODE model likelihoods
}
\section{Functions}{
\itemize{
\item \code{max_abs_odesol_diff()}: Compute maximum absolute difference in
ODE solutions of \code{x} and \code{y}.

\item \code{max_abs_loglik_diff()}: Compute maximum absolute differences in
log likelihoods of \code{x} and \code{y}.

\item \code{log_ratios()}: Compute log likelihood ratios (on log scale),
i.e. just \code{x$loglik() - y$loglik()}.

\item \code{psis_relative_eff()}: Compute relative efficiency needed for PSIS,
using \code{\link[loo:relative_eff]{loo::relative_eff()}}.

\item \code{psis()}: Call \code{\link[loo:psis]{loo::psis()}} using the log likelihoods
from \code{x} and \code{y}.

\item \code{compute_reliability_metrics()}: Compute all metrics.

}}

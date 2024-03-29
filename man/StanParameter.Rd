% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-standecl.R
\name{StanParameter}
\alias{StanParameter}
\title{A Stan parameter}
\description{
A Stan parameter

A Stan parameter
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{decl}}{The variable declaration.}

\item{\code{prior_code}}{The prior declaration.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-StanParameter-new}{\code{StanParameter$new()}}
\item \href{#method-StanParameter-print}{\code{StanParameter$print()}}
\item \href{#method-StanParameter-clone}{\code{StanParameter$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanParameter-new"></a>}}
\if{latex}{\out{\hypertarget{method-StanParameter-new}{}}}
\subsection{Method \code{new()}}{
Create a \link{StanParameter} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanParameter$new(decl, prior = "")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{decl}}{The underlying variable.}

\item{\code{prior}}{'Stan' code that defines prior for the parameter.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanParameter-print"></a>}}
\if{latex}{\out{\hypertarget{method-StanParameter-print}{}}}
\subsection{Method \code{print()}}{
Print
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanParameter$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanParameter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-StanParameter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanParameter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}

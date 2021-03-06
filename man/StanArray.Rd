% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-standecl.R
\name{StanArray}
\alias{StanArray}
\title{A Stan array}
\description{
A Stan array

A Stan array
}
\section{Super class}{
\code{\link[odemodeling:StanDeclaration]{odemodeling::StanDeclaration}} -> \code{StanArray}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dims}}{list of array dimensions}

\item{\code{type}}{base type of the array}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{StanArray$new()}}
\item \href{#method-declaration}{\code{StanArray$declaration()}}
\item \href{#method-signature}{\code{StanArray$signature()}}
\item \href{#method-can_be_made_parameter}{\code{StanArray$can_be_made_parameter()}}
\item \href{#method-get_dims}{\code{StanArray$get_dims()}}
\item \href{#method-clone}{\code{StanArray$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="odemodeling" data-topic="StanDeclaration" data-id="print">}\href{../../odemodeling/html/StanDeclaration.html#method-print}{\code{odemodeling::StanDeclaration$print()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Create a \link{StanArray} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$new(name, dims, type = "real", lower = NULL, upper = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{name of the array}

\item{\code{dims}}{list of array dimensions, must be a list of
\link{StanDimension} objects}

\item{\code{type}}{base type of the array}

\item{\code{lower}}{lower bound}

\item{\code{upper}}{upper bound}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-declaration"></a>}}
\if{latex}{\out{\hypertarget{method-declaration}{}}}
\subsection{Method \code{declaration()}}{
Create a declaration for the array
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$declaration()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-signature"></a>}}
\if{latex}{\out{\hypertarget{method-signature}{}}}
\subsection{Method \code{signature()}}{
The variable when used in function signature
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$signature()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-can_be_made_parameter"></a>}}
\if{latex}{\out{\hypertarget{method-can_be_made_parameter}{}}}
\subsection{Method \code{can_be_made_parameter()}}{
Can the object be made into a parameter?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$can_be_made_parameter()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_dims"></a>}}
\if{latex}{\out{\hypertarget{method-get_dims}{}}}
\subsection{Method \code{get_dims()}}{
Get all declared dimensions related to the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$get_dims()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanArray$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

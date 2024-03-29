% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-standecl.R
\name{StanVectorArray}
\alias{StanVectorArray}
\title{A Stan array of vectors}
\description{
A Stan array of vectors

A Stan array of vectors
}
\section{Super class}{
\code{\link[odemodeling:StanDeclaration]{odemodeling::StanDeclaration}} -> \code{StanVectorArray}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{dims}}{list of array dimensions}

\item{\code{length}}{length of the vector}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-StanVectorArray-new}{\code{StanVectorArray$new()}}
\item \href{#method-StanVectorArray-declaration}{\code{StanVectorArray$declaration()}}
\item \href{#method-StanVectorArray-signature}{\code{StanVectorArray$signature()}}
\item \href{#method-StanVectorArray-can_be_made_parameter}{\code{StanVectorArray$can_be_made_parameter()}}
\item \href{#method-StanVectorArray-get_dims}{\code{StanVectorArray$get_dims()}}
\item \href{#method-StanVectorArray-clone}{\code{StanVectorArray$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="odemodeling" data-topic="StanDeclaration" data-id="print"><a href='../../odemodeling/html/StanDeclaration.html#method-StanDeclaration-print'><code>odemodeling::StanDeclaration$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-new"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-new}{}}}
\subsection{Method \code{new()}}{
Create a \link{StanVectorArray} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$new(name, dims, length, lower = NULL, upper = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{name of the vector array}

\item{\code{dims}}{list of array dimensions, must be a list of
\link{StanDimension} objects}

\item{\code{length}}{length of the vector, must be a \link{StanDimension} object}

\item{\code{lower}}{lower bound}

\item{\code{upper}}{upper bound}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-declaration"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-declaration}{}}}
\subsection{Method \code{declaration()}}{
Create a declaration for the array of vectors
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$declaration()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
a string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-signature"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-signature}{}}}
\subsection{Method \code{signature()}}{
The variable when used in function signature
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$signature()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-can_be_made_parameter"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-can_be_made_parameter}{}}}
\subsection{Method \code{can_be_made_parameter()}}{
Can the object be made into a parameter?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$can_be_made_parameter()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-get_dims"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-get_dims}{}}}
\subsection{Method \code{get_dims()}}{
Get all declared dimensions related to the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$get_dims()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanVectorArray-clone"></a>}}
\if{latex}{\out{\hypertarget{method-StanVectorArray-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanVectorArray$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes-standecl.R
\name{StanDeclaration}
\alias{StanDeclaration}
\title{A Stan variable declaration (an abstract base class)}
\description{
A Stan variable declaration (an abstract base class)

A Stan variable declaration (an abstract base class)
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{name of the variable}

\item{\code{lower}}{lower bound}

\item{\code{upper}}{upper bound}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-StanDeclaration-new}{\code{StanDeclaration$new()}}
\item \href{#method-StanDeclaration-declaration}{\code{StanDeclaration$declaration()}}
\item \href{#method-StanDeclaration-signature}{\code{StanDeclaration$signature()}}
\item \href{#method-StanDeclaration-print}{\code{StanDeclaration$print()}}
\item \href{#method-StanDeclaration-can_be_made_parameter}{\code{StanDeclaration$can_be_made_parameter()}}
\item \href{#method-StanDeclaration-get_dims}{\code{StanDeclaration$get_dims()}}
\item \href{#method-StanDeclaration-clone}{\code{StanDeclaration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-new"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-new}{}}}
\subsection{Method \code{new()}}{
\code{StanDeclaration} is an abstract class that can't be initialized.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-declaration"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-declaration}{}}}
\subsection{Method \code{declaration()}}{
The variable declaration as a string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$declaration()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-signature"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-signature}{}}}
\subsection{Method \code{signature()}}{
The variable when used in function signature
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$signature()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-print"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-print}{}}}
\subsection{Method \code{print()}}{
Print
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-can_be_made_parameter"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-can_be_made_parameter}{}}}
\subsection{Method \code{can_be_made_parameter()}}{
Can the object be made into a parameter?
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$can_be_made_parameter()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-get_dims"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-get_dims}{}}}
\subsection{Method \code{get_dims()}}{
Get all declared dimensions related to the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$get_dims()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A list.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StanDeclaration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-StanDeclaration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StanDeclaration$clone(deep = FALSE)}\if{html}{\out{</div>}}
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

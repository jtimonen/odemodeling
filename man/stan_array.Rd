% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main-declarations.R
\name{stan_array}
\alias{stan_array}
\title{Create a \link{StanArray} object.}
\usage{
stan_array(name, dims, type = "real", lower = NULL, upper = NULL)
}
\arguments{
\item{name}{name of the array}

\item{dims}{list of array dimensions, must be a list of
\code{StanDimension} objects}

\item{type}{base type of the array}

\item{lower}{lower bound}

\item{upper}{upper bound}
}
\description{
Create a \link{StanArray} object.
}
\examples{
N <- stan_dim("N")
M <- stan_dim("M")
my_arr <- stan_array("A", dims = list(N, N, M), lower = 0, type = "int")
print(my_arr)
}
\seealso{
Other Stan variable declaration functions: 
\code{\link{stan_dim}()},
\code{\link{stan_matrix}()},
\code{\link{stan_param}()},
\code{\link{stan_transform}()},
\code{\link{stan_var}()},
\code{\link{stan_vector}()},
\code{\link{stan_vector_array}()}
}
\concept{Stan variable declaration functions}

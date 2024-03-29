% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nthroot.R
\name{nthroot}
\alias{nthroot}
\title{nthroot function}
\usage{
nthroot(x, n)
}
\arguments{
\item{x}{The base.}

\item{n}{The n'th root.}
}
\description{
nthroot handles negatives, e.g. returning nthroot(-2,3) == -1.259921, rather than NA.
https://stackoverflow.com/a/66554084
}
\keyword{nth}
\keyword{nthroot}
\keyword{root}

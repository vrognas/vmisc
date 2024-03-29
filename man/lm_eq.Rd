% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_eq.R
\name{lm_eq}
\alias{lm_eq}
\title{lm_eq function}
\usage{
lm_eq()
}
\description{
This function allows you to output the equation as well as the adjusted
R-squared, for a linear model in a ggplot.
}
\examples{
\dontrun{
library(ggplot2)

cars \%>\%
ggplot(aes(x = speed, y = dist)) +
geom_smooth(method = "lm") +
lm_eq()
}
}
\keyword{equation}
\keyword{linear}
\keyword{lm}
\keyword{model}

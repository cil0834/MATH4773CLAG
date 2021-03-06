% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsq_xk1_xk2.R
\name{rsq_xk1_xk2}
\alias{rsq_xk1_xk2}
\title{rsq_xk1_xk2}
\usage{
rsq_xk1_xk2(xk1, xk2, data)
}
\arguments{
\item{xk1}{x knot 1}

\item{xk2}{x knot 2}

\item{data}{the data used in the linear model
This function returns the r^2 value of the linear model given x knot 1 and x knot 2}
}
\value{
the r^2 value of the linear model given the data and the x knots
}
\description{
rsq_xk1_xk2
}
\examples{
rsq_xk1_xk2(xk1, xk2, spruce.df)
}

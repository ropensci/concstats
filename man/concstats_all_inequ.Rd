% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_inequ.R
\name{concstats_all_inequ}
\alias{concstats_all_inequ}
\title{A wrapper for the proposed inequality measures}
\usage{
concstats_all_inequ(x, normalized = TRUE, na.rm = TRUE, digits = NULL)
}
\arguments{
\item{x}{A non-negative numeric vector.}

\item{normalized}{Logical. Argument specifying whether or not a normalized
value is required. Must be either \code{TRUE} or \code{FALSE}. The default
is \code{FALSE}.}

\item{na.rm}{A logical vector that indicates whether \code{NA} values should
be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
if \code{NA} values are present.}

\item{digits}{An optional value for digits. Specifies the minimum number of
significant digits to be printed in values. The default is \code{NULL} and
will use base R print option.}
}
\value{
A \code{data.frame}.
}
\description{
A wrapper for the proposed inequality measures
}
\details{
\code{concstats_all_inequ} returns all proposed group measures in a one step
procedure with default settings if not otherwise specified.
}
\examples{
# a vector of market shares
x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
concstats_all_inequ(x, digits = 2)

}
\seealso{
\code{\link[=concstats_all_mstruct]{concstats_all_mstruct()}}, \code{\link[=concstats_all_comp]{concstats_all_comp()}}

Other Concentration and inequality measures: 
\code{\link{concstats_entropy}()},
\code{\link{concstats_gini}()},
\code{\link{concstats_grs}()},
\code{\link{concstats_inequ}()},
\code{\link{concstats_palma}()},
\code{\link{concstats_simpson}()}
}
\concept{Concentration and inequality measures}

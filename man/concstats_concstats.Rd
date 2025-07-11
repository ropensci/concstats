% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concstats.R
\name{concstats_concstats}
\alias{concstats_concstats}
\title{A set of Market Structure, Concentration, and Inequality Measures}
\usage{
concstats_concstats(x, na.rm = TRUE, digits = NULL)
}
\arguments{
\item{x}{A non-negative numeric vector.}

\item{na.rm}{A logical vector that indicates whether \code{NA} values should
be excluded or not. Must be either \code{TRUE} or \code{FALSE}. Defaults to
\code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
vector contains \code{NA} values.}

\item{digits}{A non-null value for digits specifies the minimum number of
significant digits to be printed in values. The default is \code{NULL} and
will use base R print option.}
}
\value{
A \verb{data frame} of numeric measures with default settings from
the respective individual measure.
}
\description{
A convenience function which calculates a selected set of
different market structure, inequality and concentration measures more or
less commonly used, e.g. k-firm ratios, Entropy, HHI, Palma ratio,
and others in a one step procedure to provide a first overview.
}
\details{
\code{concstats_concstats} computes a set of different and selected
structural, inequality, and concentration measures in a one step procedure.
The resulting \verb{data frame} contains eight measures: number of firms with
market share, numbers equivalent, the cumulative share of the top
(top 3 and top 5) firm(s) in percentage, the hhi index, the entropy index,
and the palma ratio. However, all measures can be computed individually or
in groups.
}
\note{
The vector of market shares should be in a decimal form corresponding
to the total share of individual firms/units. The vector should sum up to 1,
otherwise a numeric vector will be converted into decimal form.
}
\examples{
# a vector of market shares
x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
# a selected set of different structural, concentration, and inequality
# measures
concstats_concstats(x, digits = 2)

}
\seealso{
\code{\link[=concstats_mstruct]{concstats_mstruct()}}, \code{\link[=concstats_comp]{concstats_comp()}}, \code{\link[=concstats_inequ]{concstats_inequ()}}
}

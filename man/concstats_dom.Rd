% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dom.R
\name{concstats_dom}
\alias{concstats_dom}
\title{Dominance Index}
\usage{
concstats_dom(x, na.rm = TRUE, digits = NULL)
}
\arguments{
\item{x}{A non-negative numeric vector.}

\item{na.rm}{A logical vector that indicates whether \code{NA} values should
be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
is \code{TRUE}. If set to \code{FALSE} the computation yields a warning
if the vector contains \code{NA} values. NAs will be removed for further
computations.}

\item{digits}{An optional value for digits. Specifies the minimum number of
significant digits to be printed in values. The default is \code{NULL} and
will use base R print option.}
}
\value{
A single numeric measure in decimal form or, if NAs are present,
with a warning.
}
\description{
An alternative measure which can be used in case of mergers.
}
\details{
\code{concstats_dom} calculates a dominance index, which measures the
concentration within the Herfindahl-Hirschman index, that is, the
concentration within the concentration.
}
\examples{
# a vector of market shares
x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
concstats_dom(x, na.rm = FALSE, digits = 2)

# a vector with NA values
x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
concstats_dom(x, digits = 2)

}
\references{
Garcia Alba Idunate, P. (1994). "Un Indice de dominancia para el
analisis de la estructura de los mercados". \emph{El Trimestre Economico},
61: 499-524.
}
\seealso{
Other Competition/Concentration measures: 
\code{\link{concstats_all_comp}()},
\code{\link{concstats_comp}()},
\code{\link{concstats_hhi}()},
\code{\link{concstats_hhi_d}()},
\code{\link{concstats_hhi_min}()},
\code{\link{concstats_sten}()}
}
\concept{Competition/Concentration measures}

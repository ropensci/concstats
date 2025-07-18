% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_mstruct.R
\name{concstats_all_mstruct}
\alias{concstats_all_mstruct}
\title{A wrapper for the proposed structural measures}
\usage{
concstats_all_mstruct(x, na.rm = TRUE, digits = NULL)
}
\arguments{
\item{x}{A non-negative numeric vector.}

\item{na.rm}{A logical vector that indicates whether \code{NA} values should
be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
is \code{TRUE}.
If set to \code{FALSE} the computation yields \code{NA} if the vector
contains \code{NA} values.}

\item{digits}{An optional value for digits. Specifies the minimum number of
significant digits to be printed in values. The default is \code{NULL} and
will use base R print option.}
}
\value{
A \code{data.frame}.
}
\description{
A wrapper for the proposed structural measures
}
\details{
\code{concstats_all_mstruct} returns all proposed group measures in a
one step procedure with default settings if not otherwise specified.
}
\examples{
# a vector of market shares
x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
concstats_all_mstruct(x, digits = 2)

}
\seealso{
\code{\link[=concstats_all_comp]{concstats_all_comp()}}, \code{\link[=concstats_all_inequ]{concstats_all_inequ()}}

Other Market structure measures: 
\code{\link{concstats_firm}()},
\code{\link{concstats_mstruct}()},
\code{\link{concstats_nrs_eq}()},
\code{\link{concstats_top}()},
\code{\link{concstats_top3}()},
\code{\link{concstats_top3_df}()},
\code{\link{concstats_top5}()},
\code{\link{concstats_top5_df}()},
\code{\link{concstats_top_df}()}
}
\concept{Market structure measures}

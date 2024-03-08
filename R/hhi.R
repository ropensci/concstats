#' @title Herfindahl-Hirschman Index
#'
#' @description
#' A measure of industry concentration and widely used in merger control.
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Ranges from {0, 1} and often used for comparison over
#'  time. Must be either \code{TRUE} or \code{FALSE}. The default is
#'  \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
#'  \code{NA} values are present.
#' @details \code{concstats_hhi} calculates the widely used Herfindahl-Hirschman
#'  Index (Herfindahl, 1950 and Hirschman, 1945). The index is calculated by
#'  squaring the market share of each firm competing in the market and then
#'  summing the resulting numbers.
#' @return A single numeric measure in decimal form.
#' @references Herfindahl, O. C. (1950), "Concentration in the steel industry"
#'  (PhD thesis), Columbia University.
#' @references Hirschman, A. O. (1945), "National power and structure of
#'  foreign trade". Berkeley, CA: University of California Press.
#'
#' @family Competition/Concentration measures
#' @rdname concstats_hhi
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi(x, na.rm = FALSE)
#'
#' @export
concstats_hhi <- function(x, normalized = FALSE, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_hhi must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_hhi` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("x in `concstats_hhi` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  shares <- try(sum(x, na.rm = TRUE))
  if (shares == 100 || shares == 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_hhi` does not sum to 1")
  }


  hhi <- as.numeric(sum(x ^ 2))
  if (normalized == TRUE) hhi <- as.numeric((hhi - (1 / sum(x > 0))) /
                                              (1 - (1 / sum(x > 0))))
  return(hhi)
}

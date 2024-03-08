#' @title Palma ratio
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @details
#'  \code{concstats_palma} measures the ratio of inequality (normally used with
#'   income inequality) of the top 10 percent to the bottom 40 percent.
#' @return A single numeric measure.
#' @references Palma, J. G. (2006). "Globalizing Inequality: 'Centrifugal' and
#'  'Centripetal' Forces at Work", DESA Working Paper No. 35.
#'
#' @family Concentration and inequality measures
#' @rdname concstats_palma
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_palma(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_palma(x, na.rm = FALSE)
#'
#' @export
concstats_palma <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_palma must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_palma` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_palma` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_palma` does not sum to 1")
  }

  # explicit conversion to continuous
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x)
  x_cut <-  cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                include.lowest = TRUE, labels = FALSE)
  x_bottom <- sum(x[x_cut <= 4])
  x_top <- sum(x[x_cut > 9])
  palma <- as.numeric(x_top / x_bottom)
  return(palma)
}

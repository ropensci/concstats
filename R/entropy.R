#' @title Shannon Entropy
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#' @return A single numeric measure.
#' @references Shannon, C. E. (1948). "A Mathematical Theory of Communication",
#'  \emph{The Bell System Technical Journal} (Nokia Bell Labs).
#'
#' @family Concentration and inequality measures
#' @rdname concstats_entropy
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_entropy(x, normalized = TRUE)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_entropy(x, na.rm = FALSE)
#'
#' @export
concstats_entropy <- function(x, normalized = TRUE, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_entropy must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_entropy` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_entropy` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_entropy` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_entropy` does not sum to 1")
  }

  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
                         / log(sum(x > 0), base = 2)))
  if (normalized == FALSE) entropy <- as.numeric(
    sum(-x / sum(x) * log(x / sum(x), base = 2))
  )
  return(entropy)
}

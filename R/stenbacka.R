#' @title Stenbacka Index
#'
#' @description
#' The measure suggests an approach that classifies when an individual firm has
#' a dominant position and therefore assesses market dominance.
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'   If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'   are present.
#' @details
#'  \code{concstats_sten} calculates the Stenbacka index,
#'   which indicates the market share of a dominant position.
#' @return A single numeric measure in decimal form.
#' @references Melnik, A., Shy, Oz, Stenbacka, R., (2008), "Assessing market
#'  dominance", \emph{Journal of Economic Behavior and Organization},
#'  68: pp. 63-72.
#'
#' @family Competition/Concentration measures
#' @rdname concstats_sten
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_sten(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_sten(x, na.rm = FALSE)
#'
#' @export
concstats_sten <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_sten must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_sten` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_sten` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_sten` does not sum to 1")
  }

  # explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- as.numeric(stats::na.omit(x))
  x <- sort(x, decreasing = TRUE)

  sten1 <- x[1]
  sten2 <- x[2]
  sten <- as.numeric(0.5 * (1 - 1 * (sten1 ^ 2 - sten2 ^ 2)) * 100)
  return(sten)
}

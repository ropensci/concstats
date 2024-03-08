#' @title Top 3 market share
#'
#' @inheritParams concstats_top
#'
#' @return A positive numeric value, which indicates the cumulative sum of the
#'  top 3 market shares as a percentage.
#'
#' @family Market structure measures
#' @rdname concstats_top3
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_top3(x)
#'
#' @export
concstats_top3 <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_top3 must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top3` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("`x` in `concstats_top3` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) == 1 || sum(x, na.rm = TRUE) == 100) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_top3` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  top3 <- as.numeric(sum(x[1:3], na.rm = TRUE) * 100)
  return(top3)
}

#' @title A wrapper for the proposed structural measures
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
#' @details
#'  \code{concstats_all_mstruct} returns all proposed group measures in a
#'   one step procedure with default settings if not otherwise specified.
#' @return A `data.frame`.
#'
#' @seealso [concstats_all_comp()], [concstats_all_inequ()]
#'
#' @family Market structure measures
#' @rdname concstats_all_mstruct
#' @export
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_all_mstruct(x, digits = 2)
#'
concstats_all_mstruct <- function(x, na.rm = TRUE, digits = NULL) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_all_mstruct must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_mstruct` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("x in `concstats_all_mstruct` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) == 1 || sum(x, na.rm = TRUE) == 100) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_all_mstruct` does not sum to 1")
  }

  firm <- concstats_firm(x, na.rm = TRUE)
  nrs_eq <- concstats_nrs_eq(x, na.rm = TRUE)
  top <- concstats_top(x, na.rm = TRUE)
  top3 <- concstats_top3(x, na.rm = TRUE)
  top5 <- concstats_top5(x, na.rm = TRUE)

  # screen-based output
  results_mstruct <- data.frame(Measure = c("Firms", "Nrs_equivalent",
                                            "Top (%)", "Top3 (%)", "Top5 (%)"),
                                Value = as.numeric(format(c(firm, nrs_eq, top,
                                                            top3, top5),
                                                          scientific = FALSE,
                                                          digits = digits,
                                                          justify = "right")))

  return(results_mstruct)
}

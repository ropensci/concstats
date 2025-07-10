#' @title A wrapper for the proposed structural measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @usage
#'  concstats_all_mstruct(x, na.rm = TRUE, digits = NULL)
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
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
#' @srrstats {G2.1} Assertions on types of inputs
#' @srrstats {EA1.3} Identify data to accept as input

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0} Implement assertions on lengths of inputs
#' @srrstats {G2.14a} error on missing data
#' @srrstats {G2.14b} ignore missing data with messages issued
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_mstruct` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE && anyNA(x)) {
    message("`x` has NA values. NAs have been removed for computation.")
    x <- x[!is.na(x)]
  }

  if (!na.rm && anyNA(x)) return(NA_real_)

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  # check sum of vector. Must sum to 1.
  if (!isTRUE(all.equal(sum(x, na.rm = TRUE),
                        1, tolerance = .Machine$double.eps^0.25))) {
    stop("Your input vector `x` in `concstats_all_mstruct` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  firm <- concstats_firm(x)
  nrs_eq <- concstats_nrs_eq(x)
  top <- concstats_top(x)
  top3 <- concstats_top3(x)
  top5 <- concstats_top5(x)

#' @srrstats {EA4.1, EA5.2} Screen-based with numeric formatting
  results_mstruct <- data.frame(Measure = c("Firms", "Nrs_equivalent",
                                            "Top (%)", "Top3 (%)",
                                            "Top5 (%)"),
                                Value = as.numeric(format(c(firm, nrs_eq,
                                                            top, top3,
                                                            top5),
                                                          cientific = FALSE,
                                                          digits = digits,
                                                          justify = "right")))

  return(as.data.frame(results_mstruct))
}

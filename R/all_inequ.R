#' @title A wrapper for the proposed inequality measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @srrstats {EA1.3} Identify data to accept as input
#' @usage
#'  concstats_all_inequ(x, normalized = TRUE, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if \code{NA} values are present.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details
#'  \code{concstats_all_inequ} returns all proposed group measures in a one step
#'   procedure with default settings if not otherwise specified.
#' @return A `data.frame`.
#' @seealso [concstats_all_mstruct()], [concstats_all_comp()]
#'
#' @family Concentration and inequality measures
#' @rdname concstats_all_inequ
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_all_inequ(x, digits = 2)
#'
#' @export
concstats_all_inequ <- function(x, normalized = TRUE, na.rm = TRUE,
                                digits = NULL) {
#' @srrstats {G2.1} Assertions on types of inputs
  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0} Implement assertions on lengths of inputs
#' @srrstats {G2.14a} error on missing data
#' @srrstats {G2.14b} ignore missing data with messages issued

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_inequ` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normalized` in `concstats_all_inequ` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE && anyNA(x)) {
    message("`x` has NA values. NAs have been removed for computation.")
    x <- x[!is.na(x)]
  }

  if (!na.rm && anyNA(x)) return(NA_real_)

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (!isTRUE(all.equal(sum(x, na.rm = TRUE),
                        1, tolerance = .Machine$double.eps^0.25))) {
    stop("Your input vector `x` in `concstats_all_inequ` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  entropy <- concstats_entropy(x)
  gini <- concstats_gini(x)
  simpson <- concstats_simpson(x)
  palma <- concstats_palma(x)
  grs <- concstats_grs(x)

#' @srrstats {EA4.1, EA5.2} Screen-based with numeric formatting
  results_inequ <- data.frame(Measure = c("Entropy", "Gini Index",
                                          "Simpson Index", "Palma Ratio",
                                          "GRS"),
                              Value = as.numeric(format(c(entropy, gini,
                                                          simpson, palma, grs),
                                                        scientific = FALSE,
                                                        digits = digits,
                                                        justify = "right")))

  return(as.data.frame(results_inequ))
}

#' @title A wrapper for the proposed concentration measures
#'
#' @srrstats {EA1.3} Identify data to accept as input
#' @usage
#'  concstats_all_comp(x, normalized = FALSE, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector. The computation yields \code{NA} if
#'  \code{NA} values are present.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. Defaults to
#'  \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values.
#' @details
#'  \code{concstats_all_comp} returns all proposed group measures in a one step
#'   procedure with default settings if not otherwise specified.
#' @return A `data.frame`.
#' @seealso [concstats_all_mstruct()], [concstats_all_inequ()]
#'
#' @family Competition/Concentration measures
#' @rdname concstats_all_comp
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_all_comp(x, digits = 2)
#'
#' @export
concstats_all_comp <- function(x, normalized = FALSE, na.rm = TRUE,
                               digits = NULL) {
#' @srrstats {G5.8a} Zero-length data

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
#' @srrstats {G5.2. G5.2a} Appropriate error, warning, message behavior

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_comp` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normlized` in `concstats_all_comp` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE && anyNA(x)) {
    message("`x` has NA values. NAs have been removed for computation.")
    x <- x[!is.na(x)]
  }

  if (!na.rm && anyNA(x)) return(NA_real_)

  x <- as.numeric(x / sum(x, na.rm = TRUE))

#' @srrstats {G3.0, G5.9, G5.9a, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x, na.rm = TRUE), 1,
                        tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_all_comp` does not sum to 1")
  }

  x <- as.numeric(x[!is.na(x)] / sum(x[!is.na(x)]))

  hhi <- concstats_hhi(x)
  hhi_d <- concstats_hhi_d(x)
  hhi_min <- concstats_hhi_min(x)
  dom <- concstats_dom(x)
  sten <- concstats_sten(x)

#' @srrstats {EA4.1, EA5.2} Screen-based with numeric formatting
  results_comp <- data.frame(Measure = c("HHI", "HHI(min)", "HHI(dual)",
                                         "Dominance", "Stenbacka(%)"),
                             Value = as.numeric(format(c(hhi, hhi_min, hhi_d,
                                                         dom, sten),
                                                       scientific = FALSE,
                                                       digits = digits,
                                                       justify = "right")))
#' @srrstats {EA4.0, EA4.2, EA5.3, EA5.4}
  return(as.data.frame(results_comp))
}

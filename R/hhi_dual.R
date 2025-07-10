#' @title Dual of the Herfindahl-Hirschman Index
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description
#' The dual of the HHI reflects the fraction of participants that do have market
#' participation.
#' @srrstats {G1.1} First implementation in R
#' @usage
#'  concstats_hhi_d(x, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details \code{concstats_hhi_d} is the dual of the HHI index, which indicates
#'  the percentage which represents the fraction of the banks that do not have
#'  market participation.
#' @return A single numeric value.
#' @references Chang, E. J., Guerra, S. M., de Souza Penaloza, R. A. & Tabak,
#'  B. M. (2005) Banking concentration: the Brazilian case. \emph{In Financial
#'  Stability Report}. Brasilia: Banco Central do Brasil, 4: 109-129.
#' @srrstats {G1.0} Primary reference
#' @family Competition/Concentration measures
#' @rdname concstats_hhi_d
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi_d(x)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi_d(x, na.rm = TRUE, digits = 2)
#'
#' @export
concstats_hhi_d <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi_d` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_hhi_d` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  hhi <- as.numeric(sum(x ^ 2))
  hhi_d <- as.numeric(1 - 1 / (sum(x > 0) * hhi))
  if (!is.null(digits)) hhi_d <- as.numeric(round(hhi_d, digits = digits))
  return(as.numeric(hhi_d))
}

#' @title Dominance Index
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description
#' An alternative measure which can be used in case of mergers.
#'
#' @usage
#'  concstats_dom(x, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a warning
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @param digits An optional value for digits. Specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details
#'  \code{concstats_dom} calculates a dominance index, which measures the
#'   concentration within the Herfindahl-Hirschman index, that is, the
#'   concentration within the concentration.
#' @return A single numeric measure in decimal form or, if NAs are present,
#'  with a warning.
#' @srrstats {G1.0} Primary reference
#' @srrstats {G1.1} First implementation in R
#' @references Garcia Alba Idunate, P. (1994). "Un Indice de dominancia para el
#'  analisis de la estructura de los mercados". \emph{El Trimestre Economico},
#'  61: 499-524.
#'
#' @family Competition/Concentration measures
#' @rdname concstats_dom
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_dom(x, na.rm = FALSE, digits = 2)
#'
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_dom(x, digits = 2)
#'
#' @export
concstats_dom <- function(x, na.rm = TRUE, digits = NULL) {
  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
  checkmate::assert_numeric(x, any.missing = TRUE, .var.name = "x")

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_dom` must be either TRUE or FALSE")
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
    stop(" Your input vector `x` in `concstats_dom` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  hhi_1 <- x ^ 2
  hhi <- sum(x ^ 2, na.rm = TRUE)
  dom <- (hhi_1 / hhi) ^ 2
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  dom <- as.numeric(sum(dom, na.rm = TRUE))
  if (!is.null(digits)) dom <- as.numeric(round(dom, digits = digits))
  return(as.numeric(dom))

}

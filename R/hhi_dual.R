#' @title Dual of the Herfindahl-Hirschman Index
#'
#' @description
#' The dual of the HHI reflects the fraction of participants that do have market
#' participation.
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @details \code{concstats_hhi_d} is the dual of the HHI index, which indicates
#'  the percentage which represents the fraction of the banks that do not have
#'  market participation.
#' @return A single numeric measure in decimal form.
#' @references Chang, E. J., Guerra, S. M., de Souza Penaloza, R. A. & Tabak,
#'  B. M. (2005) Banking concentration: the Brazilian case. \emph{In Financial
#'  Stability Report}. Brasilia: Banco Central do Brasil, 4: 109-129.
#'
#' @family Competition/Concentration measures
#' @rdname concstats_hhi_d
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi_d(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi_d(x, na.rm = FALSE)
#'
#' @export

concstats_hhi_d <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_hhi_d must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi_d` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi_d` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_hhi_d` does not sum to 1")
  }

  # explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }


  hhi <- as.numeric(sum(x ^ 2))
  hhi_d <- as.numeric(1 - 1 / (sum(x > 0) * hhi))
  return(hhi_d)
}

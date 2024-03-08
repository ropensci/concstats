#' @title Dominance Index
#'
#' @description
#' An alternative measure which can be used in case of mergers.
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @details
#'  \code{concstats_dom} calculates a dominance index, which measures the
#'   concentration within the Herfindahl-Hirschman index, that is, the
#'   concentration within the concentration.
#' @return A single numeric measure in decimal form.
#' @references Garcia Alba Idunate, P. (1994). "Un Indice de dominancia para el
#'  analisis de la estructura de los mercados". \emph{El Trimestre Economico},
#'  61: 499-524.
#'
#' @family Competition/Concentration measures
#' @rdname concstats_dom
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_dom(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_dom(x, na.rm = FALSE)
#'
#' @export
concstats_dom <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_dom must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_dom` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("x in `concstats_dom` must be a positive vector")
  }

  # explicit conversion to continuous via `as.numeric()`
  shares <- try(sum(x, na.rm = TRUE))
  if (shares == 100 || shares == 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_dom` does not sum to 1")
  }


  hhi_1 <- x ^ 2
  hhi <- sum(x ^ 2)
  dom <- (hhi_1 / hhi) ^ 2
  dom <- as.numeric(sum(dom))
  return(dom)
}

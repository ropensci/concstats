#' @title A wrapper for the proposed concentration measures
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. Defaults to
#'  \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
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
                               digits = NULL ) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_all_comp must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_comp` must be either TRUE or FALSE")
  }

  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_all_comp` must be either TRUE or
            FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("x in `concstats_all_comp` must be a positive vector")
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
    stop("vector `x` in `concstats_all_comp` does not sum to 1")
  }


  x <- as.numeric(x, na.rm = TRUE)

  hhi <- concstats_hhi(x, normalized = normalized, na.rm = TRUE)
  hhi_d <- concstats_hhi_d(x, na.rm = TRUE)
  hhi_min <- concstats_hhi_min(x, na.rm = TRUE)
  dom <- concstats_dom(x, na.rm = TRUE)
  sten <- concstats_sten(x, na.rm = TRUE)

  # screen-based output.
  results_comp <- data.frame(Measure = c("HHI", "HHI(min)", "HHI(dual)",
                                         "Dominance", "Stenbacka(%)"),
                             Value = as.numeric(format(c(hhi, hhi_min, hhi_d,
                                                         dom, sten),
                                                       scientific = FALSE,
                                                       digits = digits,
                                                       justify = "right")))

  return(results_comp)
}

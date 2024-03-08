#' @title Gini-Simpson Index
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @details \code{concstats_simpson} is the Gini-Simpson index, also known as
#'  the Gini impurity (Gini's diversity index) in Machine Learning, Gibbs-Martin
#'  index or Blau index in sociology and management studies. This index ranges
#'  from (0, 1).
#' @return A single numeric value in decimal form.
#'
#' @references Simpson, E. H. (1949). "Measurement of Diversity", \emph{Nature},
#'  163, 688.
#' @references Jost, L. (2006). "Entropy and Diversity". \emph{Oikos}, 113(2),
#'  363-375.
#'
#' @family Concentration and inequality measures
#' @rdname concstats_simpson
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_simpson(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_simpson(x, na.rm = FALSE)
#'
#' @export
concstats_simpson <- function(x, na.rm = TRUE) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_simpson must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_simpson` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_simpson` must be a positive vector")
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x < 1) &&
                 !isTRUE(all.equal(sum(x), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `x` in `concstats_simpson` does not sum to 1")
  }

  # explicit conversion to continuous
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  simpson <- as.numeric(1 - sum((x/sum(x))^2))
  return(simpson)
}

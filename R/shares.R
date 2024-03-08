#' @title shares
#'
#' @description
#' The \code{concstats_shares} function is a helper function making it easier
#' to convert numeric variable into individual shares. This might be convenient
#' for larger vectors.
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
#'  \code{NA} values are present.
#' @param digits Numeric. A non-null value for digits specifies the minimum
#'  number of significant digits to be rounded in values. The default is
#'  \code{NULL} and will use base R print option.
#' @details \code{concstats_shares} is a helper function. The user can manually
#'  convert or provide numerical vectors of shares or use
#'  \code{constats_shares}.
#' @return A numeric vector in decimal form.
#'
#' @rdname concstats_shares
#'
#' @examples
#' # a vector of loans (without special characters, e.g. currency symbols)
#' x <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72)
#' concstats_shares(x, digits = 5)
#' # a vector with NA values
#' x2 <- c(538572286.08, 481096.77, 161914143.03, 128796268.59, 69055940.72, NA)
#' concstats_shares(x2, na.rm = FALSE, digits = 5)
#'
#' @export
concstats_shares <- function(x, na.rm = TRUE, digits = NULL) {
  if (!is.numeric(x)) {
    stop("`x` in concstats_shares must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_shares` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(any(x < 0))) {
    stop("`x` in `concstats_shares` must be a positive vector")
  }

  out <-  as.numeric(x / sum(x, na.rm = TRUE))

  if (!is.null(digits)) out <- as.numeric(round(out, digits = digits))
  #out <- as.numeric(gsub("@%$#*Gs,","", out))
  return(out)

}


#' @title Top market share data frame
#'
#' @param x A data frame.
#' @param y A non-negative vector of shares.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @return A data frame, which indicates an id or firm column and the top market
#'  share in decimal form.
#'
#' @family Market structure measures
#' @rdname concstats_top_df
#' @importFrom stats na.omit
#'
#' @export
#' @examples
#' # some data
#' id <- c(1, 2, 3, 4, 5)
#' x <- c(0.2, 0.25, 0.4, 0.1, 0.05)
#' test_df <- data.frame(id, x)
#'
#' concstats_top_df(test_df, "x")
#'
#'

concstats_top_df <- function(x, y,  na.rm = TRUE) {
  if (!is.data.frame(x)) {
    stop("`x` in concstats_top_df must be a data frame\n",
         "You have provided an object of class:", class(x)[1])
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_top_df` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE) {
    x <- stats::na.omit(x)
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  #check if x has negative values
  if (as.logical(any(x < 0))) {
    stop("`y` in `concstats_top_df` has negative values")
  }

  # explicit conversion to continuous via `as.numeric()`
  shares <- try(sum(x[ ,y], na.rm = TRUE))
  if (shares == 100 || shares == 1) {
    x[ ,y] <-  as.numeric(x[ ,y] / sum(x[ ,y], na.rm = TRUE))
  }

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(x[ ,y] < 1) &&
                 !isTRUE(all.equal(sum(x[ ,y]), 1,
                                   tolerance = .Machine$double.eps^0.25)))) {
    stop("vector `y` in `concstats_top_df` does not sum to 1")
  }

  x <- x[order(x[ ,y], decreasing = TRUE),]
  top_df <- as.data.frame(x[1, ])
  return(top_df)

}

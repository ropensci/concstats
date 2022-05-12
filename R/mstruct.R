#' @title Market Structure Measures
#'
#' @description Set of different measures to reflect a given market structure.
#'
#' @usage
#'  mstruct(x, type = c("firm", "nrs_eq", "top", "top3", "top5", "all"),
#'  na.rm = TRUE)
#'
#'
#' @param x A non-negative numeric vector.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "firm".
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details
#'  \code{mstruct} is a wrapper for the proposed structural measures \code{firm},
#'  returns the number of firms with a given market share, \code{nrs_eq}
#'  computes the numbers equivalent, \code{top}, \code{top3}, and \code{top5}
#'  calculate the share of the top (top 3 and top 5) firm(s). \code{all}
#'  computes all measures in a one step procedure. All measures can be computed
#'  individually.
#'
#' @return returns the calculated measure
#' @note the vector of market shares should be in a decimal form corresponding
#'  to total share of individual firms/units.The sum of the vector should sum up
#'  to 1.
#'
#' @seealso {\code{\link{concstats}}, \code{\link{comp}}, \code{\link{inequ}}}
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the number of firms with market share
#' share_firm <- mstruct(share, type = "firm")
#' # Calculate top market share individually
#' share_top <- top(share)
#' # Calculate the market structure group measures
#' share_mstruct <- mstruct(share, type = "all")
#'
#' @export mstruct
mstruct <- function(x,
                    type = c("firm", "nrs_eq", "top", "top3", "top5", "all"),
                    na.rm = TRUE) {

  switch(match.arg(type),
         firm = firm(x, na.rm = na.rm),
         nrs_eq = nrs_eq(x, na.rm = na.rm),
         top = top(x, na.rm = na.rm),
         top3 = top3(x, na.rm = na.rm),
         top5 = top5(x, na.rm = na.rm),
         all = all_mstruct(x, na.rm = na.rm))
}

#' @export
#' @rdname mstruct
#' @param x a non-negative numeric vector.
#' @param na.rm Logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
firm <- function(x, na.rm = TRUE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(round(x) == 0)) {
    x
  } else {
    stop("'x' must be in decimal format")
  }

  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- as.numeric(stats::na.omit(x))
  firm <- sum(x > 0)
  return(firm)
}

#' @export
#' @rdname mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
nrs_eq <- function(x, na.rm = TRUE) {

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(round(x) == 0)) {
    x
  } else {
    stop("'x' must be in decimal format")
  }

  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- as.numeric(stats::na.omit(x))
  nrs_eq <- 1 / sum(x ^ 2)
  return(nrs_eq)
}

#' @export
#' @rdname mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
top <- function(x, na.rm = TRUE) {

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(round(x) == 0)) {
    x
  } else {
    stop("'x' must be in decimal format")
  }

  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- sort(x, decreasing = TRUE)
  top <- x[1] * 100
  return(top)
}

#' @export
#' @rdname mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
top3 <- function(x, na.rm = TRUE) {

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(round(x) == 0)) {
    x
  } else {
    stop("'x' must be in decimal format")
  }

  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- sort(x, decreasing = TRUE)
  top3 <- sum(x[1:3], na.rm = TRUE) * 100
  return(top3)
}

#' @export
#' @rdname mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
top5 <- function(x, na.rm = TRUE) {

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(round(x) == 0)) {
    x
  } else {
    stop("'x' must be in decimal format")
  }

  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- sort(x, decreasing = TRUE)
  top5 <-  sum(x[1:5], na.rm = TRUE) * 100
  return(top5)
}

#' @export
#' @rdname mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
all_mstruct <- function(x, na.rm = TRUE) {

  invisible(utils::capture.output(
    firm <- firm(x, na.rm = TRUE),
    nrs_eq <- nrs_eq(x, na.rm = TRUE),
    top <- top(x, na.rm = TRUE),
    top3 <- top3(x, na.rm = TRUE),
    top5 <- top5(x, na.rm = TRUE)))
  results_struc <- data.frame(Measure = c("Firms", "Nrs_equivalent", "Top (%)",
                                          "Top3 (%)", "Top5 (%)"),
                              Value = format(c(firm, nrs_eq, top, top3, top5),
                              scientific = FALSE,
                              digits = 2,
                              justify = "right"))

  return(results_struc)
}

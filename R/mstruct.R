#' @title Market Structure Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description Set of different market structure measures to reflect a given
#'  market structure.
#'
#' @usage
#'  concstats_mstruct(x, type = c("firm", "nrs_eq", "top", "top3", "top5",
#'  "all"), na.rm = TRUE)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A numeric vector of length 1 with non-negative values.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "firm". Input is not
#'  case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details
#'  \code{concstats_mstruct} is a wrapper for the proposed structural measures
#'  \code{concstats_firm}, returns the number of firms with a given market share
#'  \code{concstats_nrs_eq} computes the numbers equivalent,
#'  \code{concstats_top}, \code{concstats_top3}, and \code{concstats_top5}
#'  calculate the share of the top (top 3 and top 5) firm(s) and returns the
#'  value in percentage. \code{concstats_all} computes all measures in a
#'  one step procedure. All measures can be computed individually.
#'
#' @return Returns the calculated numeric measure.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total share of individual firms/units.The sum of the vector should sum up
#'  to 1.
#'
#' @seealso {\code{\link{concstats_concstats}}, \code{\link{concstats_comp}},
#'  \code{\link{concstats_inequ}}}
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the number of firms with market share
#' share_firm <- concstats_mstruct(share, type = "firm")
#' # Calculate top market share individually
#' share_top <- concstats_top(share)
#' # Calculate the market structure group measures
#' share_mstruct <- concstats_mstruct(share, type = "all")
#'
#' @export concstats_mstruct
concstats_mstruct <- function(x,
                    type = c("firm", "nrs_eq", "top", "top3", "top5", "all"),
                    na.rm = TRUE) {
  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()` on line#50

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
         firm = concstats_firm(x, na.rm = na.rm),
         nrs_eq = concstats_nrs_eq(x, na.rm = na.rm),
         top = concstats_top(x, na.rm = na.rm),
         top3 = concstats_top3(x, na.rm = na.rm),
         top5 = concstats_top5(x, na.rm = na.rm),
         all = concstats_all_mstruct(x, na.rm = na.rm))
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm Logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_firm <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_firm cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_firm` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_firm` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_firm` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_firm` does not sum to 1")
  }

  x <- as.numeric(stats::na.omit(x))
  firm <- as.numeric(sum(x > 0))
  return(firm)
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_nrs_eq <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_nrs_eq cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_nrs_eq` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_nrs_eq` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_nrs_eq` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_nrs_eq` does not sum to 1")
  }

  x <- as.numeric(stats::na.omit(x))
  nrs_eq <- as.numeric(1 / sum(x ^ 2))
  return(nrs_eq)
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_top <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_top cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_top` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("na.rm in `concstats_top` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
    if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_top` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  top <- as.numeric(x[1] * 100)
  return(top)
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_top3 <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_top3 cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_top3` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("na.rm in `concstats_top3` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top3` must be a positive vector")
  }
#' @srrstats {EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_top3` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  top3 <- as.numeric(sum(x[1:3], na.rm = TRUE) * 100)
  return(top3)
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
concstats_top5 <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_top5 cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_top5` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("na.rm in `concstats_top5` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_top5` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_top5` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  top5 <- as.numeric(sum(x[1:5], na.rm = TRUE) * 100)
  return(top5)
}

#' @export
#' @rdname concstats_mstruct
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
#' @return A `data.frame` of market structure measures with default settings.
#' @srrstats {EA2.6}
concstats_all_mstruct <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_all_mstruct cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_all_mstruct` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("na.rm in `concstats_all_mstruct` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_mstruct` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_all_mstruct` does not sum to 1")
  }

  invisible(utils::capture.output(
    firm <- concstats_firm(x, na.rm = TRUE),
    nrs_eq <- concstats_nrs_eq(x, na.rm = TRUE),
    top <- concstats_top(x, na.rm = TRUE),
    top3 <- concstats_top3(x, na.rm = TRUE),
    top5 <- concstats_top5(x, na.rm = TRUE)))
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of screen-based
#'  output.
  results_mstruct <- data.frame(Measure = c("Firms", "Nrs_equivalent",
                                            "Top (%)", "Top3 (%)", "Top5 (%)"),
                              Value = as.numeric(format(c(firm, nrs_eq, top,
                                                          top3, top5),
                                            scientific = FALSE,
                                            digits = 3,
                                            justify = "right")))

  return(results_mstruct)
}

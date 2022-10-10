#' @title Concentration Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description A set of different concentration measures.
#'
#' @usage
#'  concstats_comp(x, unbiased = FALSE, type = c("hhi", "hhi_d", "hhi_min",
#'  "dom", "sten", "all"),na.rm = TRUE)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A numeric vector of length 1 with non-negative values.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'  correction should be applied. Must be either TRUE or FALSE. The default is
#'  FALSE.
#
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "hhi". Input is not
#'  case-sensitive.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'  Must be either TRUE or FALSE. Defaults to TRUE.
#'
#' @details
#'  \code{concstats_comp} is a wrapper for the proposed concentration measures
#'  \code{concstats_hhi}, \code{concstats_hhi_d}, \code{concstats_hhi_min},
#'  \code{concstats_dom}, \code{concstats_sten}, \code{concstats_all}.
#'  If no measure is specified "hhi" will be the default.
#'
#'  \code{concstats_hhi}, \code{concstats_hhi_min}, \code{concstats_hhi_d}
#'  calculate the Herfindahl-Hirschman index, its minimum, and its dual,
#'  respectively. \code{concstats_dom} calculates a dominance index and
#'  \code{concstats_sten} calculates the Stenbacka index. The index indicates
#'  the market share of a dominant position.
#'
#'  All measures can be accessed individually.
#'  \code{concstats_hhi}, \code{concstats_hhi_d}, and \code{concstats_dom} can
#'  be calculated individually as a normalized measure changing the default
#'  setting to \code{TRUE}. \code{concstats_all} computes all measures in a one
#'  step procedure.
#'
#' @return the calculated numeric measure.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total shares of individual firms/units. The sum of the vector should sum
#'  up to 1.
#'
#'
#' @seealso {\code{\link{concstats_concstats}}, \code{\link{concstats_mstruct}},
#'  \code{\link{concstats_inequ}}}
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the Herfindahl-Hirschman index of the vector
#' shares_hhi <- concstats_comp(x, type = "hhi")
#' # individual measure
#' shares_sten <- concstats_sten(x)
#' # complete group measures
#' shares_comp <- concstats_comp(x, type = "all")
#'
#' @export concstats_comp
concstats_comp <- function(x, unbiased = FALSE, type = c("hhi", "hhi_d",
                                                         "hhi_min", "dom",
                                               "sten", "all"), na.rm = TRUE) {

  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()` on line#65

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
  switch(match.arg(type),
         hhi = concstats_hhi(x, unbiased = unbiased, na.rm = na.rm),
         hhi_d = concstats_hhi_d(x, na.rm = na.rm),
         hhi_min = concstats_hhi_min(x, na.rm = na.rm),
         dom = concstats_dom(x, na.rm = na.rm),
         sten = concstats_sten(x, na.rm = na.rm),
         all = concstats_all_comp(x, na.rm = na.rm))
}

#' @export
#' @rdname concstats_comp
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. Must be either TRUE or FALSE. The default is
#'   FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
concstats_hhi <- function(x, unbiased = FALSE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_hhi cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_hhi`must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(unbiased) | !length(unbiased) == 1) {
    warning("`unbiased` in `concstats_hhi` must be either TRUE or FALSE")
  }
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_hhi` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_hhi` does not sum to 1")
  }

  hhi <- as.numeric(sum(x ^ 2))
  if (unbiased == TRUE) hhi <- as.numeric((hhi - (1 / sum(x > 0))) /
    (1 - (1 / sum(x > 0))))
  return(hhi)
}

#' @export
#' @rdname concstats_comp
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
concstats_hhi_min <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_hhi_min cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_hhi_min` must be a numeric vector\n",
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
    warning("`na.rm` in `concstats_hhi_min` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi_min` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_hhi_min` does not sum to 1")
  }


  hhi_min <- as.numeric(1 / sum(x > 0))
  return(hhi_min)
}

#' @export
#' @rdname concstats_comp
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. Must be either TRUE or FALSE. The default is
#'   FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
concstats_hhi_d <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_hhi_d cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_hhi_d` must be a numeric vector\n",
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
    warning("`na.rm` in `concstats_hhi_d` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm  == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi_d` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_hhi_d` does not sum to 1")
  }

  hhi <- as.numeric(sum(x ^ 2))
  hhi_d <- as.numeric(1 - 1 / (sum(x > 0) * hhi))
  return(hhi_d)
}

#' @export
#' @rdname concstats_comp
#' @param x A non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. Must be either TRUE or FALSE. The default is
#'   FALSE.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
concstats_dom <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_dom cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_dom` must be a numeric vector\n",
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
    warning("`na.rm` in `concstats_dom` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm  == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_dom` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_dom` does not sum to 1")
  }

  hhi_1 <- x ^ 2
  hhi <- sum(x ^ 2)
  dom <- (hhi_1 / hhi) ^ 2
  dom <- as.numeric(sum(dom))
  return(dom)
}

#' @export
#' @rdname concstats_comp
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is FALSE.
#'   If set to \code{FALSE} the computation yields \code{NA}.
concstats_sten <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_sten cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_sten` must be a numeric vector\n",
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
    warning("`na.rm` in `concstats_sten` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm  == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_sten` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_sten` does not sum to 1")
  }

  x <- as.numeric(stats::na.omit(x))
  x <- sort(x, decreasing = TRUE)

  sten1 <- x[1]
  sten2 <- x[2]
  sten <- as.numeric(0.5 * (1 - 1 * (sten1 ^ 2 - sten2 ^ 2)))
  sten <- if (sum(sten < 1) || sum(sten == 1)) (sten * 100)
  return(sten)
}

#' @export
#' @rdname concstats_comp
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is FALSE.
#' @return `data.frame` of concentration and competition measures with default
#'   settings.
#' @srrstats {EA2.6}
concstats_all_comp <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_all_comp cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_all_comp` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <- as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_all_comp` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm  == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_comp` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_all_comp` does not sum to 1")
  }

  x <- as.numeric(stats::na.omit(x))

  invisible(utils::capture.output(
    hhi <- concstats_hhi(x, unbiased = FALSE, na.rm = TRUE),
    hhi_d <- concstats_hhi_d(x, na.rm = TRUE),
    hhi_min <- concstats_hhi_min(x, na.rm = TRUE),
    dom <- concstats_dom(x, na.rm = TRUE),
    sten <- concstats_sten(x, na.rm = TRUE)))
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of screen-based
#'  output.
  results_comp <- data.frame(Measure = c("HHI", "HHI(min)", "HHI(dual)",
                                       "Dominance", "Stenbacka(%)"),
                             Value = as.numeric(format(c(hhi, hhi_min, hhi_d,
                                                         dom, sten),
                                            scientific = FALSE,
                                            digits = 2,
                                            justify = "right")))

  return(results_comp)
}

#' @title Group of Concentration Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description A set of different concentration measures.
#'
#' @usage
#'  concstats_comp(x, normalized = FALSE, type = c("hhi", "hhi_d", "hhi_min",
#'  "dom", "sten", "all"), na.rm = TRUE, digits = NULL)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Ranges from (0, 1) and often used for comparison over
#'  time. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "hhi". Input is not
#'  case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#'  Must be either \code{TRUE} or \code{FALSE}. Defaults to \code{TRUE}.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.  Significant digits defaults to 7.
#' @details
#'  \code{concstats_comp} is a wrapper for the proposed concentration measures
#'  [concstats_hhi()], [concstats_hhi_d()], [concstats_dom()],
#'  [concstats_hhi_min()], [concstats_sten()], [concstats_all_comp()].
#'   If no measure is specified "hhi" will be the default.
#'  \code{concstats_hhi}, can be calculated individually as a normalized
#'   measure changing the default setting to \code{TRUE}.
#'  \code{concstats_all_comp} computes all measures in a one step procedure.
#'   For more details or references please see the help page of the respective
#'   function.
#'
#' @return A single numeric measure in decimal form or `data frame`.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total shares of individual firms/units. The vector should sum up to 1.
#'
#' @seealso [concstats_concstats()], [concstats_mstruct()], [concstats_inequ()]
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the Herfindahl-Hirschman index of the vector
#' concstats_comp(x, type = "hhi")
#' # individual measure
#' concstats_sten(x)
#' # complete group measures
#' concstats_comp(x, type = "all", digits = 2)
#'
#' @export concstats_comp
concstats_comp <- function(x, normalized = FALSE,
                           type = c("hhi", "hhi_d", "hhi_min", "dom", "sten",
                                    "all"), na.rm = TRUE, digits = NULL) {

  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()`
#' @srrstats {G2.0, G2.1}
  if (!is.logical(normalized) || !length(normalized) == 1 ||
      is.na(normalized)) {
    warning("`normalized` in `concstats_comp` must be either TRUE or FALSE")
  }

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
  switch(match.arg(type),
         hhi = concstats_hhi(x, normalized = normalized, na.rm = na.rm),
         hhi_d = concstats_hhi_d(x, na.rm = na.rm),
         hhi_min = concstats_hhi_min(x, na.rm = na.rm),
         dom = concstats_dom(x, na.rm = na.rm),
         sten = concstats_sten(x, na.rm = na.rm),
         all = concstats_all_comp(x, normalized = normalized, na.rm = na.rm,
                                  digits = digits))

}

#' @export
#' @title Herfindahl-Hirschman Index
#' @rdname concstats_hhi
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Ranges from {0, 1} and often used for comparison over
#'  time. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
#'  \code{NA} values are present.
#' @details \code{concstats_hhi} calculates the widely used Herfindahl-Hirschman
#'  Index (Herfindahl, 1950 and Hirschman, 1945). The index is calculated by
#'  squaring the market share of each firm competing in the market and then
#'  summing the resulting numbers.
#' @return A single numeric measure in decimal form.
#' @references Herfindahl, O. C. (1950), "Concentration in the steel industry"
#'  (PhD thesis), Columbia University.
#' @references Hirschmann, A. O. (1945), "National power and structure of
#'  foreign trade". Berkeley, CA: University of California Press.
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi(x, na.rm = FALSE)
#'
concstats_hhi <- function(x, normalized = FALSE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_hhi must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
    {
    stop("`normalized` in `concstats_hhi` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi` must be a positive vector")
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_hhi` does not sum to 1")
  }

  hhi <- as.numeric(sum(x ^ 2))
  if (normalized == TRUE) hhi <- as.numeric((hhi - (1 / sum(x > 0))) /
    (1 - (1 / sum(x > 0))))
  return(hhi)
}

#' @export
#' @title Minimum of Herfindahl-Hirschman Index
#' @rdname concstats_hhi_min
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if the vector
#'  contains \code{NA} values.
#' @details Calculates the minimum of the Herfindahl-Hirschman index, that is,
#'  the equivalent of all participants in the market with equal market shares.
#' @return A single numeric measure in decimal form.
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi_min(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi_min(x, na.rm = FALSE)
#'
concstats_hhi_min <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_hhi_min must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi_min` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi_min` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_hhi_min` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  hhi_min <- as.numeric(1 / sum(x > 0))
  return(hhi_min)
}

#' @export
#' @title Dual of the Herfindahl-Hirschman Index
#' @rdname concstats_hhi_d
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
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_hhi_d(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_hhi_d(x, na.rm = FALSE)
#'
concstats_hhi_d <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_hhi_d must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_hhi_d` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_hhi_d` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_hhi_d` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }


  hhi <- as.numeric(sum(x ^ 2))
  hhi_d <- as.numeric(1 - 1 / (sum(x > 0) * hhi))
  return(hhi_d)
}

#' @export
#' @title Dominance Index
#' @rdname concstats_dom
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
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_dom(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_dom(x, na.rm = FALSE)
#'
concstats_dom <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_dom must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_dom` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_dom` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_dom` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }


  hhi_1 <- x ^ 2
  hhi <- sum(x ^ 2)
  dom <- (hhi_1 / hhi) ^ 2
  dom <- as.numeric(sum(dom))
  return(dom)
}

#' @export
#' @title Stenbacka Index
#' @rdname concstats_sten
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'   If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'   are present.
#' @details
#'  \code{concstats_sten} calculates the Stenbacka index,
#'   which indicates the market share of a dominant position.
#' @return A single numeric measure in decimal form.
#' @references Melnik, A., Shy, Oz, Stenbacka, R., (2008), "Assessing market
#'  dominance", \emph{Journal of Economic Behavior and Organization},
#'  68: pp. 63-72.
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_sten(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_sten(x, na.rm = FALSE)
concstats_sten <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_sten must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_sten` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_sten` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_sten` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- as.numeric(stats::na.omit(x))
  x <- sort(x, decreasing = TRUE)

  sten1 <- x[1]
  sten2 <- x[2]
  sten <- as.numeric(0.5 * (1 - 1 * (sten1 ^ 2 - sten2 ^ 2)) * 100)
  return(sten)
}

#' @export
#' @title A wrapper for the proposed concentration measures
#' @rdname concstats_all_comp
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
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_all_comp(x, digits = 2)
#' @srrstats {EA2.6}
concstats_all_comp <- function(x, normalized = FALSE, na.rm = TRUE,
                               digits = NULL ) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_all_comp must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_comp` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_all_comp` must be either TRUE or
            FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_comp` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_all_comp` does not sum to 1")
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- as.numeric(stats::na.omit(x))

  invisible(utils::capture.output(
    hhi <- concstats_hhi(x, normalized = normalized, na.rm = TRUE),
    hhi_d <- concstats_hhi_d(x, na.rm = TRUE),
    hhi_min <- concstats_hhi_min(x, na.rm = TRUE),
    dom <- concstats_dom(x, na.rm = TRUE),
    sten <- concstats_sten(x, na.rm = TRUE)))
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of
#'  screen-based output.
  results_comp <- data.frame(Measure = c("HHI", "HHI(min)", "HHI(dual)",
                                       "Dominance", "Stenbacka(%)"),
                             Value = as.numeric(format(c(hhi, hhi_min, hhi_d,
                                                         dom, sten),
                                            scientific = FALSE,
                                            digits = digits,
                                            justify = "right")))

  return(results_comp)
}

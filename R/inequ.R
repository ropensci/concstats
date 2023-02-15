#' @title Inequality and Diversity Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  concstats_inequ(x, normalized = FALSE, type = c("entropy", "gini",
#'  "simpson", "palma", "grs", "all"), na.rm = TRUE)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x a numeric vector of length 1 with non-negative values.
#' @param normalized Logical. Argument of the functions
#'  \code{concstats_entropy}, \code{concstats_gini} specifying whether or not a
#'   normalized value is required. Ranges from {0, 1} and often used for
#'   comparison over time. Must be either TRUE or FALSE. The default is FALSE.
#' @param type a character string of the measure to be calculated, defaults to
#'  "concstats_entropy". Input is not case-sensitive.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'  Must be either TRUE or FALSE. The default is TRUE.
#'
#' @details
#'  \code{concstats_inequ} is a wrapper for the proposed inequality measures
#'  \code{concstats_entropy}, \code{concstats_gini}, \code{concstats_simpson},
#'  \code{concstats_palma}, \code{concstats_grs}, \code{concstats_all}.
#'   If no measure is specified, "concstats_entropy" is the default.
#'  \code{concstats_entropy} returns the Shannon Entropy (Shannon, 1948),
#'  \code{concstats_gini} is the Gini coefficient. You can normalize the
#'   Entropy and Gini measures by setting \code{normalized = TRUE}
#'  \code{concstats_simpson} is the Gini-Simpson (Simpson, 1949; Jost, 2006),
#'   also known as the Gini impurity (Gini's diversity index) in
#'   Machine Learning, Gibbs-Martin index or Blau index in sociology and
#'   management studies. This index ranges from {0, 1}.
#'  \code{concstats_palma} measures the ratio of inequality (normally used with
#'   income inequality) of the top 10 percent to the bottom 40 percent
#'   (Palma, 2006).
#'  \code{concstats_grs} is an alternative inequality measure (Ginevicius, 2009)
#'   and
#'  \code{concstats_all} returns all measures in a one step procedure.
#'
#' @return the calculated numeric measure
#' @note the non-negative vector of market shares should be in a decimal form
#'  corresponding to the total shares of individual firms/units.The vector
#'  should sum up to 1.
#' @references Shannon, C. E. (1948). A Mathematical Theory of Communication,
#'  The Bell System Technical Journal (Nokia Bell Labs).
#' @references Simpson, E. H. (1949). Measurement of Diversity, Nature, 163,
#'  688.
#' @references Palma, J. G. (2006). Globalizing Inequality: 'Centrifugal' and
#'  'Centripetal' Forces at Work, DESA Working Paper No. 35.
#' @references Ginevicius, R. and S. Cirba (2009). Additive measurement of
#'  market concentration, Journal of Business Economics and Management, 10(3),
#'  191-198.
#' @references Jost, L. (2006). Entropy and Diversity. Oikos, 113(2), 363-375.
#'
#' @seealso \code{\link[=concstats_concstats]{concstats_concstats()}},
#'  \code{\link[=concstats_mstruct]{concstats_mstruct()}},
#'  \code{\link[=concstats_comp]{concstats_comp()}}
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' # Calculate the Palma ratio
#' concstats_inequ(x, type = "palma")
#' # Calculate the entropy measure directly
#' concstats_entropy(x, normalized = TRUE)
#' # Calculate the group measures
#' concstats_inequ(x, type = "all")
#'
#' @export concstats_inequ
concstats_inequ <- function(x, normalized = FALSE, type = c("entropy", "gini",
                                                          "simpson", "palma",
                                                          "grs", "all"),
                            na.rm = TRUE) {
  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()`
#' @srrstats {G2.0, G2.1}
  if (!is.logical(normalized) || !length(normalized) == 1) {
    warning("`normalized` in `concstats_comp` must be either TRUE or FALSE")
  }
#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
           entropy = concstats_entropy(x, normalized = normalized,
                                       na.rm = na.rm),
           gini = concstats_gini(x, normalized = normalized, na.rm = na.rm),
           simpson = concstats_simpson(x, na.rm = na.rm),
           palma = concstats_palma(x, na.rm = na.rm),
           grs = concstats_grs(x, na.rm = na.rm),
           all = concstats_all_inequ(x, na.rm = na.rm))

}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either TRUE or FALSE. The default is TRUE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
concstats_entropy <- function(x, normalized = TRUE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_entropy cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_entropy` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(normalized) || !length(normalized) == 1) {
    warning("`normalized` in `concstats_entropy` must be either TRUE or FALSE")
  }
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_entropy` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_entropy` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_entropy` does not sum to 1")
  }

  entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
               / log(sum(x > 0), base = 2)))
  if (normalized == FALSE) entropy <- as.numeric(
    sum(-x / sum(x) * log(x / sum(x), base = 2))
    )
  return(entropy)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either TRUE or FALSE. The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_gini <- function(x, normalized = FALSE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_gini cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_gini`must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <- as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(normalized) || !length(normalized) == 1) {
    warning("`normalized` in `concstats_gini` must be either TRUE or FALSE")
  }
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_gini` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_gini` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_gini` does not sum to 1")
  }

  x <- sort(x)
  gini <- as.numeric(2 * sum(x * seq_len(length(x))) /
    (length(x) * sum(x)) - 1 - (1 / length(x)))
  if (normalized) gini <- as.numeric(length(x) / (length(x) - 1) * gini)
  return(gini)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_simpson <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_simpson cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_simpson` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("`na.rm` in `concstats_simpson` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_simpson` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_simpson` does not sum to 1")
  }

  simpson <- as.numeric(1 - (sum(x * (x - 1)) / (sum(x) * (sum(x - 1)))))
  return(simpson)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_palma <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_palma cannot be empty.")
  }
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_palma` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("*na.rm* in `concstats_palma` is not a logical value")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_palma` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_palma` does not sum to 1")
  }

  x <- sort(x)
  x_cut <-  cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                include.lowest = TRUE, labels = FALSE)
  x_bottom <- sum(x[x_cut <= 4])
  x_top <- sum(x[x_cut > 9])
  palma <- as.numeric(x_top / x_bottom)
  return(palma)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_grs <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_grs cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_grs` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
  #' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("*na.rm* in `concstats_grs` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_grs` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_grs` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)
  grs <- as.numeric(sum((sum(x > 0) ^ 2 * x[1] + 0.3 * x ^ 2) /
               (sum(x > 0) ^ 2 + sum(x > 0) * 0.3 * x[1] * x) * x))
  return(grs)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
#' @return a `data.frame` of inequality measures with default settings.
#' @srrstats {EA2.6}
concstats_all_inequ <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_all_inequ cannot be empty.")
  }

#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("'x' in `concstats_all_inequ` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1) {
    warning("*na.rm* in `concstats_all_inequ` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (as.logical(na.rm == TRUE)) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)
#' @srrstats {G3.0, EA6.0e} Testing values of single-valued objects.
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_all_inequ` does not sum to 1")
  }

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_inequ` must be a positive vector")
  }
  x <- as.numeric(x)

  invisible(utils::capture.output(
    entropy <- concstats_entropy(x, normalized = TRUE, na.rm = TRUE),
    gini <- concstats_gini(x, normalized = FALSE, na.rm = TRUE),
    simpson <- concstats_simpson(x, na.rm = TRUE),
    palma <- concstats_palma(x, na.rm = TRUE),
    grs <- concstats_grs(x, na.rm = TRUE)))
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of
#'  screen-based output.
  results_inequ <- data.frame(Measure = c("Entropy", "Gini Index",
                                          "Simpson Index", "Palma Ratio",
                                          "GRS"),
                              Value = as.numeric(format(c(entropy, gini,
                                                          simpson, palma, grs),
                                             scientific = FALSE,
                                             digits = 2,
                                             justify = "right")))

  return(results_inequ)

}

#' @title Inequality and Diversity Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  concstats_inequ(x, normalized = FALSE, type = c("entropy", "gini",
#'  "simpson", "palma", "grs", "all"), na.rm = TRUE, digits = NULL)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument of the functions
#'  \code{concstats_entropy}, \code{concstats_gini} specifying whether or not a
#'   normalized value is required. Ranges from (0, 1) and often used for
#'   comparison over time. Must be either \code{TRUE} or \code{FALSE}. The
#'   default is \code{FALSE}.
#' @param type A character string of the measure to be calculated, defaults to
#'  `concstats_entropy`. Input is not case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#'  Must be either \code{TRUE} or \code{FALSE}. The default is \code{TRUE}.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
#' @details
#'  \code{concstats_inequ} is a wrapper for the proposed inequality measures
#'  [concstats_entropy()], [concstats_gini()],[concstats_simpson()],
#'  [concstats_palma()],[concstats_grs()], [concstats_all_inequ()]
#'   If no measure is specified, `concstats_entropy` is the default.
#'  \code{concstats_entropy} returns the Shannon Entropy (Shannon, 1948),
#'  \code{concstats_gini} is the Gini coefficient. You can normalize the
#'   Entropy and Gini measures by setting \code{normalized = TRUE}
#'  \code{concstats_palma} measures the ratio of inequality (normally used in
#'   the context of measuring income inequality) of the top 10 percent to the
#'    bottom 40 percent (Palma, 2006).
#'  \code{concstats_grs} is an alternative inequality measure (Ginevicius, 2009)
#'   and
#'  \code{concstats_all_inequ} returns all measures in a one step procedure.
#'  For more details or references please see the help page of the respective
#'  function.
#'
#' @return The calculated numeric measure or a `data frame`
#' @seealso [concstats_concstats()],[concstats_mstruct()],[concstats_comp()]
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' # Calculate the Palma ratio
#' concstats_inequ(x, type = "palma")
#' # Calculate the entropy measure directly
#' concstats_entropy(x, normalized = TRUE)
#' # Calculate the group measures
#' concstats_inequ(x, type = "all", digits = 2)
#'
#' @export concstats_inequ
concstats_inequ <- function(x, normalized = FALSE, type = c("entropy", "gini",
                                                          "simpson", "palma",
                                                          "grs", "all"),
                            na.rm = TRUE, digits = NULL) {
  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()`
#' @srrstats {G2.0, G2.1}
  if (!is.logical(normalized) || !length(normalized) == 1 ||
      is.na(normalized)) {
    stop("`normalized` in `concstats_comp` must be either TRUE or FALSE")
  }
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_comp` must be either TRUE or FALSE")
  }
#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
           entropy = concstats_entropy(x, normalized = normalized,
                                       na.rm = na.rm),
           gini = concstats_gini(x, normalized = normalized, na.rm = na.rm),
           simpson = concstats_simpson(x, na.rm = na.rm),
           palma = concstats_palma(x, na.rm = na.rm),
           grs = concstats_grs(x, na.rm = na.rm),
           all = concstats_all_inequ(x, normalized = normalized, na.rm = na.rm,
                                     digits = digits))

}

#' @export
#' @title Shannon Entropy
#' @rdname concstats_entropy
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#' @return A single numeric measure.
#' @references Shannon, C. E. (1948). "A Mathematical Theory of Communication",
#'  \emph{The Bell System Technical Journal} (Nokia Bell Labs).
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_entropy(x, normalized = TRUE)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_entropy(x, na.rm = FALSE)
#'
concstats_entropy <- function(x, normalized = TRUE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_entropy must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_entropy` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_entropy` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_entropy` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_entropy` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
               / log(sum(x > 0), base = 2)))
  if (normalized == FALSE) entropy <- as.numeric(
    sum(-x / sum(x) * log(x / sum(x), base = 2))
    )
  return(entropy)
}

#' @export
#' @title Gini Index
#' @rdname concstats_gini
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#' @return A single numeric measure.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_gini(x, normalized = TRUE)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_gini(x, na.rm = FALSE)
#'
concstats_gini <- function(x, normalized = TRUE, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_gini must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_gini` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_gini` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_gini` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_gini` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x)
  gini <- as.numeric(2 * sum(x * seq_len(length(x))) /
    (length(x) * sum(x)) - 1 - (1 / length(x)))
  if (normalized) gini <- as.numeric(length(x) / (length(x) - 1) * gini)
  return(gini)
}

#' @export
#' @title Gini-Simpson Index
#' @rdname concstats_simpson
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
#' @references Simpson, E. H. (1949). "Measurement of Diversity", \emph{Nature},
#'  163, 688.
#' @references Jost, L. (2006). "Entropy and Diversity". \emph{Oikos}, 113(2),
#'  363-375.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_simpson(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_simpson(x, na.rm = FALSE)
#'
concstats_simpson <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_simpson must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_simpson` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_simpson` must be a positive vector")
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
    stop("vector `x` in `concstats_simpson` does not sum to 1")
  }


  simpson <- as.numeric(1 - sum((x/sum(x))^2))
  return(simpson)
}

#' @export
#' @title Palma ratio
#' @rdname concstats_palma
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @details
#'  \code{concstats_palma} measures the ratio of inequality (normally used with
#'   income inequality) of the top 10 percent to the bottom 40 percent.
#' @return A single numeric measure.
#' @references Palma, J. G. (2006). "Globalizing Inequality: 'Centrifugal' and
#'  'Centripetal' Forces at Work", DESA Working Paper No. 35.
#'@examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_palma(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_palma(x, na.rm = FALSE)
#'
concstats_palma <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_palma must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_palma` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_palma` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_palma` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
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
#' @title GRS measure
#' @rdname concstats_grs
#' @param x A non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if vector contains
#'  \code{NA} values.
#' @return A single numeric measure in decimal form.
#' @references Ginevicius, R. and S. Cirba (2009). "Additive measurement of
#'  market concentration", \emph{Journal of Business Economics and Management},
#'  10(3), 191-198.
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' concstats_grs(x)
#' # a vector with NA values
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05, NA)
#' concstats_grs(x, na.rm = FALSE)
#'
concstats_grs <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_grs must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_grs` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_grs` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_grs` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }

  x <- sort(x, decreasing = TRUE)
  grs <- as.numeric(sum((length(x) ^ 2 * x[1] + 0.3 * x ^ 2) /
                      (length(x) ^ 2 + length(x) * 0.3 * x[1] * x) * x))
  return(grs)
}

#' @export
#' @title A wrapper for the proposed inequality measures
#' @rdname concstats_all_inequ
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#'  If set to \code{FALSE} the computation yields \code{NA} if \code{NA} values
#'  are present.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7.
#' @details
#'  \code{concstats_all_inequ} returns all proposed group measures in a one step
#'   procedure with default settings if not otherwise specified.
#' @return A `data.frame`.
#' @seealso [concstats_all_mstruct()], [concstats_all_comp()]
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_all_inequ(x, digits = 2)
#' @srrstats {EA2.6}
concstats_all_inequ <- function(x, normalized = FALSE, na.rm = TRUE,
                                digits = NULL) {
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  if (!is.numeric(x)) {
    stop("`x` in concstats_all_inequ must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_all_inequ` must be either TRUE or FALSE")
  }
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized))
  {
    stop("`normalized` in `concstats_all_inequ` must be either TRUE or
            FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (as.logical(all(x < 0))) {
    stop("x in `concstats_all_inequ` must be a positive vector")
  }

#' @srrstats {G3.0, EA6.0, EA6.0e} Return values, single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(sum(x), 1, tolerance = .Machine$double.eps^0.25))) {
    stop("vector `x` in `concstats_all_inequ` does not sum to 1")
  }

#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
  x <- as.numeric(x)

    entropy <- concstats_entropy(x, normalized = normalized, na.rm = TRUE)
    gini <- concstats_gini(x, normalized = normalized, na.rm = TRUE)
    simpson <- concstats_simpson(x, na.rm = TRUE)
    palma <- concstats_palma(x, na.rm = TRUE)
    grs <- concstats_grs(x, na.rm = TRUE)
#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of
#'  screen-based output.
  results_inequ <- data.frame(Measure = c("Entropy", "Gini Index",
                                          "Simpson Index", "Palma Ratio",
                                          "GRS"),
                              Value = as.numeric(format(c(entropy, gini,
                                                          simpson, palma, grs),
                                             scientific = FALSE,
                                             digits = digits,
                                             justify = "right")))

  return(results_inequ)

}

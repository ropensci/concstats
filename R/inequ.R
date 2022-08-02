#' @title Inequality and Diversity Measures
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  concstats_inequ(x, unbiased = FALSE, type = c("entropy", "gini", "simpson",
#'  "palma", "grs", "all"), na.rm = TRUE)
#'
#' @param x a numeric vector of non-negative values.
#' @param unbiased Logical. Argument of the functions \code{concstats_entropy},
#'  \code{concstats_gini}, and \code{concstats_simpson} specifying whether or
#'  not a finite sample correction should be applied. Must be either TRUE or
#'  FALSE. The default is FALSE.
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
#'  If no measure is specified, "concstats_entropy" is the default.
#'
#' \code{concstats_entropy} returns the (Shannon) Entropy, \code{concstats_gini}
#'  is the Gini coefficient, \code{concstats_simpson} is the complement of the
#'  Herfindahl-Hirschman Index. You can normalize each of these three measures
#'  by setting \code{unbiased = TRUE}
#'
#' \code{concstats_palma} measures the ratio of inequality (normally used with
#'  income inequality) of the top 10 percent to the bottom 40 percent.
#'  \code{concstats_grs} is an alternative inequality measure (Ginevicius, 2009),
#'  \code{concstats_all} returns all measures in a one step procedure.
#'
#' @return prints the calculated measure
#' @note the non-negative vector of market shares should be in a decimal form
#'  corresponding to the total shares of individual firms/units.The vector
#'  should sum up to 1.
#'
#' @seealso {\code{\link{concstats_concstats}}, \code{\link{concstats_mstruct}},
#'  \code{\link{concstats_comp}}}
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' # Calculate the Palma ratio
#' share_p <- concstats_inequ(share, type = "palma")
#' # Calculate the entropy measure directly
#' share_ent <- concstats_entropy(share, unbiased = TRUE)
#' # Calculate the group measures
#' share_inequ <- concstats_inequ(share, type = "all")
#'
#' @export concstats_inequ
concstats_inequ <- function(x, unbiased = FALSE, type = c("entropy", "gini",
                                                          "simpson", "palma",
                                                          "grs", "all"),
                  na.rm = TRUE) {

  type <- tolower(as.character(type))

  if (!isTRUE(length(unbiased) == 1L)) {
    stop("`unbiased` must be of length 1")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (!is.logical(unbiased) | !is.logical(na.rm)) {
    warning(" Either`unbiased` or `na.rm` is not a logical value")
  }

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

    switch(match.arg(type),
           entropy = concstats_entropy(x, unbiased = unbiased, na.rm = na.rm),
           gini = concstats_gini(x, unbiased = unbiased, na.rm = na.rm),
           simpson = concstats_simpson(x, unbiased = unbiased, na.rm = na.rm),
           palma = concstats_palma(x, na.rm = na.rm),
           grs = concstats_grs(x, na.rm = na.rm),
           all = concstats_all_inequ(x, na.rm = na.rm))
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. Must be either TRUE or FALSE. The default is
#'   TRUE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
concstats_entropy <- function(x, unbiased = TRUE, na.rm = TRUE) {

  if (!isTRUE(length(unbiased) == 1L)) {
    stop("`unbiased` must be of length 1")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (!is.logical(unbiased) | !is.logical(na.rm)) {
    warning(" Either`unbiased` or `na.rm` is not a logical value")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

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

  entropy <- (sum(-x / sum(x) * log(x / sum(x), base = 2))
               / log(sum(x > 0), base = 2))
  if (unbiased == FALSE) entropy <- sum(-x / sum(x) * log(x / sum(x), base = 2))
  return(entropy)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'  correction should be applied. Must be either TRUE or FALSE. The default is
#'  FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_gini <- function(x, unbiased = FALSE, na.rm = TRUE) {

  if (!isTRUE(length(unbiased) == 1L)) {
    stop("`unbiased` must be of length 1")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (!is.logical(unbiased) | !is.logical(na.rm)) {
    warning(" Either`unbiased` or `na.rm` is not a logical value")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

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

  x <- sort(x)
  gini <- 2 * sum(x * seq_len(length(x))) /
    (length(x) * sum(x)) - 1 - (1 / length(x))
  if (unbiased) gini <- length(x) / (length(x) - 1) * gini
  return(gini)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. Must be either TRUE or FALSE. The default is
#'   FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_simpson <- function(x, unbiased = FALSE, na.rm = TRUE) {

  if (!isTRUE(length(unbiased) == 1L)) {
    stop("`unbiased` must be of length 1")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (!is.logical(unbiased) | !is.logical(na.rm)) {
    warning(" Either`unbiased` or `na.rm` is not a logical value")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

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

  simpson <- 1 - sum(x ^ 2)
  if (unbiased) simpson <- 1 - sum(x ^ 2) / (sum(x / sum(x))) ^ 2
  return(simpson)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_palma <- function(x, na.rm = TRUE) {

  if (!is.logical(na.rm)) {
    warning("`na.rm` must be a logical value")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

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

  x <- sort(x)
  x_cut <-  cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                include.lowest = TRUE, labels = FALSE)
  x_bottom <- sum(x[x_cut <= 4])
  x_top <- sum(x[x_cut > 9])
  palma <- x_top / x_bottom
  return(palma)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
concstats_grs <- function(x, na.rm = TRUE) {

  if (!is.logical(na.rm)) {
    warning("`na.rm` must be a logical value")
  }

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

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
  grs <- sum((sum(x > 0) ^ 2 * x[1] + 0.3 * x ^ 2) /
               (sum(x > 0) ^ 2 + sum(x > 0) * 0.3 * x[1] * x) * x)
  return(grs)
}

#' @export
#' @rdname concstats_inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. The default is TRUE.
#'  If set to \code{FALSE} the computation yields \code{NA}.
#' @return a data.frame of inequality measures with default settings.
concstats_all_inequ <- function(x, na.rm = TRUE) {

  if (!isTRUE(length(na.rm) == 1L)) {
    stop("`na.rm` must be of length 1")
  }

  if (!is.logical(na.rm)) {
    warning("`na.rm` must be a logical value")
  }

  if (as.logical(na.rm) == TRUE) {
    x <- x[!is.na(x)]
  }

  if (isFALSE(na.rm) && any(is.na(x))) return(NA_real_)

  invisible(utils::capture.output(
    entropy <- concstats_entropy(x, unbiased = TRUE, na.rm = TRUE),
    gini <- concstats_gini(x, unbiased = FALSE, na.rm = TRUE),
    simpson <- concstats_simpson(x, unbiased = FALSE, na.rm = TRUE),
    palma <- concstats_palma(x, na.rm = TRUE),
    grs <- concstats_grs(x, na.rm = TRUE)))

  results_inequ <- data.frame(Measure = c("Entropy", "Gini Index",
                                          "Simpson Index", "Palma Ratio",
                                          "GRS"),
                              Value = format(c(entropy, gini, simpson, palma,
                                               grs),
                                             scientific = FALSE,
                                             digits = 2,
                                             justify = "right"))

  return(results_inequ)

}

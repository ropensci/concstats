#' @title Inequality and Diversity Measures
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  inequ(x, unbiased = FALSE, type = c("entropy", "gini", "simpson", "palma",
#'  "grs", "all"), na.rm = TRUE)
#'
#' @param x a numeric vector of non-negative values.
#' @param unbiased Logical. Argument of the function \code{entropy},
#'  \code{gini}, and \code{simpson} specifying whether or not a finite sample
#'   correction should be applied.
#' @param type a character string of the measure to be calculated, defaults to
#'  "entropy".
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details
#'  \code{inequ} is a wrapper for the proposed inequality measures
#'  \code{entropy}, \code{gini}, \code{simpson}, \code{palma}, \code{grs},
#'  \code{all}.
#'  If no measure is specified, "entropy" is the default.
#'
#' \code{entropy} returns the (Shannon) Entropy, \code{gini} is the
#'  Gini coefficient, \code{simpson} is the complement of the
#'  Herfindahl-Hirschmann Index. You can normalize each of these three measures
#'  by setting \code{unbiased = TRUE}
#'
#' \code{Palma} measures the ratio of inequality (used with income inequality)
#'  of the top 10 percent to the bottom 40 percent. \code{grs} is an alternative
#'  inequality measure (Ginevicius, 2009), \code{all} returns all measures in
#'  a one step procedure.
#'
#' @return prints the calculated measure
#' @note the non- negative vector might be sales figures or market shares of
#'  individual firms/units. In the latter case the vector should sum up to 1.
#'
#' @seealso {\code{\link{concstats}}, \code{\link{mstruct}}, \code{\link{comp}}}
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' # Calculate the Palma ratio
#' share_p <- inequ(share, type = "palma")
#' # Calculate the entropy measure directly
#' share_ent <- entropy(share, unbiased = TRUE)
#' # Calculate the group measures
#' share_inequ <- inequ(share, type = "all")
#'
#' @export inequ
inequ <- function(x, unbiased = FALSE, type = c("entropy", "gini", "simpson",
                                                "palma", "grs", "all"),
                  na.rm = TRUE) {

  switch(match.arg(type),
         entropy = entropy(x, unbiased = unbiased, na.rm = na.rm),
         gini = gini(x, unbiased = unbiased, na.rm = na.rm),
         simpson = simpson(x, unbiased = unbiased, na.rm = na.rm),
         palma = palma(x, na.rm = na.rm),
         grs = grs(x, na.rm = na.rm),
         all = all_inequ(x, na.rm = na.rm))
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finite sample
#'   correction should be applied. The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
entropy <- function(x, unbiased = FALSE, na.rm = TRUE) {

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
  if (!(sum(x) == 1)) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  k <- sum(x > 0)
  entropy <- sum(-x / sum(x) * log(x / sum(x), base = 2))
  if (unbiased == TRUE) entropy <- entropy / log(k, base = 2)
  return(entropy)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied. The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
gini <- function(x, unbiased = FALSE, na.rm = TRUE) {

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
  if (!(sum(x) == 1)) {
    stop("vector does not sum to 1")
  }

  if (!is.numeric(x)) {
    stop("'x' must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }

  x <- sort(x)
  gini <- 2 * sum(x * seq_len(length(x))) / (length(x) * sum(x)) - 1 - (1 / length(x))
  if (unbiased) gini <- length(x) / (length(x) - 1) * gini
  return(gini)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied. The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not.
simpson <- function(x, unbiased = FALSE, na.rm = TRUE) {

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
  if (!(sum(x) == 1)) {
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
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
palma <- function(x, na.rm = TRUE) {
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
  if (!(sum(x) == 1)) {
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
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
grs <- function(x, na.rm = TRUE) {

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
  if (!(sum(x) == 1)) {
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
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
all_inequ <- function(x, na.rm = TRUE) {

  invisible(utils::capture.output(
    entropy <- entropy(x, unbiased = FALSE, na.rm = TRUE),
    gini <- gini(x, unbiased = FALSE, na.rm = TRUE),
    simpson <- simpson(x, unbiased = FALSE, na.rm = TRUE),
    palma <- palma(x, na.rm = TRUE),
    grs <- grs(x, na.rm = TRUE)))

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

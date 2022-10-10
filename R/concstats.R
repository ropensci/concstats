#' @title A set of Market Structure, Concentration, and Inequality Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @srrstats {EA1.2} scope of the package/ kind of questions the software is
#'   intended to solve.
#' @description a convenience function which calculates a selected set of
#'  different market structure, inequality and concentration measures more or
#'  less commonly used, e.g. k-firm ratios, Entropy, HHI, Palma ratio,
#'  and others in a one step procedure to provide a first overview.
#'
#' @usage concstats_concstats(x, na.rm = TRUE)
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a, EA1.1, EA1.3} accepted as input, length
#'  and type
#' @param x a numeric vector of length 1 with non-negative values.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either TRUE or FALSE. Defaults to TRUE. If set
#'  to \code{FALSE} the computation yields \code{NA}.
#' @details \code{concstats_concstats} computes a set of different and selected
#'  structural, inequality, and concentration measures in a one step procedure,
#'  however, all measures can be computed individually or in groups.
#'
#' @return returns a data frame of numeric measures with default settings.
#' .
#' @note the vector of market shares should be in a decimal form corresponding
#'  to the total share of individual firms/units. The vector should sum up to 1.
#'
#' @seealso \code{\link{concstats_mstruct}}, \code{\link{concstats_comp}},
#'  \code{\link{concstats_inequ}}
#'
#' @examples
#' # a vector of market shares
#' # share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # a selected set of different structural, concentration, and inequality
#' # measures
#' # share_conc <- concstats_concstats(share)
#'
#' @export
#' @srrstats {G2.6, EA2.6} process vector data
concstats_concstats <- function(x, na.rm = TRUE) {
#' @srrstats {G5.8a} Zero-length data
  if (length(x) == 0) {
    stop("x in concstats_concstats cannot be empty.")
  }
#' @srrstats {G2.2, G2.6, G2.16} Checking class and type, NaN handling
  # convert x in a positive decimal vector
  if (!is.numeric(x)) {
    stop("x in `concstats_concstats` must be a numeric vector\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.4, G2.4b} explicit conversion to continuous via `as.numeric()`
  else if (sum(x, na.rm = TRUE) > 1) {
    x <-  as.numeric(x / sum(x, na.rm = TRUE))
  } else {
    x
  }
#' @srrstats {G2.0, G2.1}
  if (!is.logical(na.rm) | !length(na.rm ) == 1) {
    warning("`na.rm` in `concstats_concstats` must be either TRUE or FALSE")
  }
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (na.rm == TRUE) {
    x <- as.numeric(x[!is.na(x)])
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  # check if x is a positive decimal vector
  if (all(x[!is.na(x)] < 0)) {
    stop("x in `concstats_concstats` must be a positive vector")
  }
#' @srrstats {G3.0, EA6.0, EA6.0e} Testing values of single-valued objects.
  # check sum of vector. Must sum to 1
  if (!isTRUE(all.equal(1, sum(x), tolerance = .Machine$double.eps^0.25))) {
    stop("vector x in `concstats_concstats` does not sum to 1")
  }

  x <- sort(x, decreasing = TRUE)

  concstats_firm <- as.numeric(sum(x > 0))

  concstats_nrs_eq <- as.numeric(1 / sum(x ^ 2))

  concstats_top <- as.numeric(x[1] * 100)

  concstats_top3 <- as.numeric(sum(x[1:3], na.rm = TRUE) * 100)

  concstats_top5 <- as.numeric(sum(x[1:5], na.rm = TRUE) * 100)

  concstats_hhi <- as.numeric(sum(x ^ 2))

  concstats_entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
    / log(sum(x > 0), base = 2)))

  palma <- as.numeric(stats::na.omit(x))
  palma <- sort(x)
  palma_cut <- cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                   include.lowest = TRUE, labels = FALSE)
  palma_bottom <- sum(x[palma_cut <= 4])
  palma_top <- sum(x[palma_cut > 9])
  concstats_palma <- as.numeric(palma_top / palma_bottom)

#' @srrstats {EA4.0, EA4.1, EA4.2, EA5.2, EA5.4} Numeric control of screen-based
#'  output.
  results_all <- data.frame(Measure = c("Firms", "Nrs_equivalent", "Top (%)",
                                       "Top3 (%)", "Top5 (%)", "HHI",
                                       "Entropy(RE)", "Palma ratio"),
                        Value = as.numeric(format(c(concstats_firm,
                                                    concstats_nrs_eq,
                                                    concstats_top,
                                                    concstats_top3,
                                                    concstats_top5,
                                                    concstats_hhi,
                                                    concstats_entropy,
                                                    concstats_palma),
                                   scientific = FALSE,
                                   digits = 2,
                                   justify = "right")))

  return(results_all)

}

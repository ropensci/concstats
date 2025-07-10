#' @title A set of Market Structure, Concentration, and Inequality Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description A convenience function which calculates a selected set of
#'  different market structure, inequality and concentration measures more or
#'  less commonly used, e.g. k-firm ratios, Entropy, HHI, Palma ratio,
#'  and others in a one step procedure to provide a first overview.
#'
#' @usage
#'  concstats_concstats(x, na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. Defaults to
#'  \code{TRUE}. If set to \code{FALSE} the computation yields \code{NA} if
#'   vector contains \code{NA} values.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.
#' @details \code{concstats_concstats} computes a set of different and selected
#'  structural, inequality, and concentration measures in a one step procedure.
#'  The resulting `data frame` contains eight measures: number of firms with
#'  market share, numbers equivalent, the cumulative share of the top
#'  (top 3 and top 5) firm(s) in percentage, the hhi index, the entropy index,
#'  and the palma ratio. However, all measures can be computed individually or
#'  in groups.
#'
#' @return A `data frame` of numeric measures with default settings from
#'  the respective individual measure.
#'
#' @note The vector of market shares should be in a decimal form corresponding
#'  to the total share of individual firms/units. The vector should sum up to 1,
#'  otherwise a numeric vector will be converted into decimal form.
#'
#' @seealso [concstats_mstruct()], [concstats_comp()], [concstats_inequ()]
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # a selected set of different structural, concentration, and inequality
#' # measures
#' concstats_concstats(x, digits = 2)
#'
#' @export
concstats_concstats <- function(x, na.rm = TRUE, digits = NULL) {
#' @srrstats {G2.1} Assertions on types of inputs
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
#' @srrstats {G2.13, G2.14, G2.14a, G2.14b, G2.15} Handling of missing values
  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_dom` must be either TRUE or FALSE")
  }

  if (na.rm == TRUE && anyNA(x)) {
    message("`x` has NA values. NAs have been removed for computation.")
    x <- x[!is.na(x)]
  }

  if (!na.rm && anyNA(x)) return(NA_real_)

  x <- as.numeric(x / sum(x, na.rm = TRUE))

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (!isTRUE(all.equal(sum(x, na.rm = TRUE),
                        1, tolerance = .Machine$double.eps^0.25))) {
    stop(" Your input vector `x` in `concstats_dom` does not sum to 1")
  }

  x <- as.numeric(x / sum(x, na.rm = TRUE))
  x <- sort(x, decreasing = TRUE)

  concstats_firm <- as.numeric(sum(x > 0), na.rm = TRUE)

  concstats_nrs_eq <- as.numeric(1 / sum(x ^ 2), na.rm = TRUE)

  concstats_top <- as.numeric(x[1] * 100)

  concstats_top3 <- as.numeric(sum(x[1:3], na.rm = TRUE) * 100)

  concstats_top5 <- as.numeric(sum(x[1:5], na.rm = TRUE) * 100)

  concstats_hhi <- as.numeric(sum(x ^ 2), normalized = FALSE, na.rm = TRUE)

  concstats_entropy <- as.numeric((sum(-x / sum(x) * log(x / sum(x), base = 2))
    / log(sum(x > 0), base = 2)))

  palma <- as.numeric(stats::na.omit(sort(x)))
  palma_cut <- cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                   include.lowest = TRUE, labels = FALSE)
  palma_bottom <- sum(x[palma_cut <= 4])
  palma_top <- sum(x[palma_cut > 9])
  concstats_palma <- as.numeric(palma_top / palma_bottom)

  # screen-based output
  results_all <- data.frame(Measure = c("Firms", "Nrs_equivalent", "Top (%)",
                                        "Top3 (%)", "Top5 (%)", "HHI",
                                        "Entropy", "Palma ratio"),
                            Value = as.numeric(format(c(concstats_firm,
                                                        concstats_nrs_eq,
                                                        concstats_top,
                                                        concstats_top3,
                                                        concstats_top5,
                                                        concstats_hhi,
                                                        concstats_entropy,
                                                        concstats_palma),
                                                      scientific = FALSE,
                                                      digits = digits,
                                                      justify = "right")))

  return(as.data.frame(results_all))
}

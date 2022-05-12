#' @title A set of Market Structure, Concentration, and Inequality Measures
#'
#' @description a convenience function which calculates a selected set of
#'  different market structure, inequality and concentration measures more or
#'  less commonly used, e.g. k-firm ratios, Entropy, HHI, Palma ratio,
#'  and others in a one step procedure to provide a first overview.
#'
#' @usage concstats(x, na.rm = TRUE)
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not.
#'  If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details \code{concstats} computes a set of different and selected
#'  structural, inequality, and concentration measures in a one step procedure,
#'  however, all measures can be computed individually or in groups.
#'
#' @return returns a data frame of calculated measures
#' .
#' @note the vector of market shares should be in a decimal form corresponding
#'  to the total share of individual firms/units. The vector should sum up to 1.
#'
#' @seealso {\code{\link{mstruct}}, \code{\link{comp}}, \code{\link{inequ}}}
#'
#' @examples
#' # a vector of market shares
#' # share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # a selected set of different structural, concentration, and inequality
#' # measures
#' # share_conc <- concstats(share)
#'
#' @export
concstats <- function(x, na.rm = TRUE) {

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

  firm <- sum(x > 0)

  nrs_eq <- 1 / sum(x ^ 2)

  top <- x[1] * 100

  top3 <- sum(x[1:3] * 100)

  top5 <- sum(x[1:5]) * 100

  hhi <- sum(x ^ 2)

  entropy <- (sum(-x / sum(x) * log(x / sum(x), base = 2))
    / log(sum(x > 0), base = 2))

  palma <- as.numeric(stats::na.omit(x))
  palma <- sort(x)
  palma_cut <- cut(x, stats::quantile(x, probs = seq(0, 1, 0.1)),
                   include.lowest = TRUE, labels = FALSE)
  palma_bottom <- sum(x[palma_cut <= 4])
  palma_top <- sum(x[palma_cut > 9])
  palma <- palma_top / palma_bottom


  results_all <- data.frame(Measure = c("Firms", "Nrs_equivalent", "Top (%)",
                                       "Top3 (%)", "Top5 (%)", "HHI",
                                       "Entropy(RE)", "Palma ratio"),
                        Value = format(c(firm, nrs_eq, top, top3, top5, hhi,
                                          entropy, palma),
                                   scientific = FALSE,
                                   digits = 2,
                                   justify = "right"))

  return(results_all)

}

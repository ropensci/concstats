#' @title A set of Market Structure, Concentration, and Inequality Measures
#'
#' @description a convenience function which calculates a selected set of
#'  different market structure, inequality and concentration measures more or
#'  less commonly used, e.g. k-firm ratios, Entropy, HHI, Palma ratio, and others
#'  in a one step procedure to provide a first overview.
#'
#' @usage concstats(x, na.rm = TRUE)
#' @param x a numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not.
#'  If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details \code{concstats} computes a set of different and selected structural,
#'  inequality, and concentration measures in a one step procedure, however, all
#'  measures can be computed individually or in groups.
#'
#' @return returns a data frame of calculated measures
#' .
#' @note the vector of market shares should be in a decimal form corresponding
#'  total share of individual firms/units.The vector should sum up to 1.
#'  You can also use sales figures to compute the respective measure.
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

  x <- replace(x, x == 0, NA)

  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
  }

  if (!na.rm && any(is.na(x))) return(NA_real_)

  if (!is.numeric(x)) {
    stop('"x" must be a numeric vector\n',
         'You have provided an object of class: ', class(x)[1])
  }

  if (sum(is.na(x))) {
    stop('"x" is an empty vector\n',
         'All values will be 0')
  }

    if (sum(x == 1)) {
    stop('"x" is a monopolistic market')
  }

  x <- sort(x, decreasing = TRUE)
  Firm <- sum(x > 0)
  Firm

  Nrs_equ <- x/sum(x)
  Nrs_equ <- Nrs_equ^2
  Nrs_equ <- sum(Nrs_equ)
  Nrs_equ <- 1/Nrs_equ

  Top <- sort(x, decreasing = TRUE)
  Top <- x/sum(x)
  Top <- Top[1]
  Top <- sum(Top * 100, na.rm = TRUE)
  Top

  Top3 <- x/sum(x)
  Top3 <- Top3[1:3]
  Top3 <- sum(Top3 * 100, na.rm = TRUE)
  Top3

  Top5 <- x/sum(x)
  Top5 <- Top5[1:5]
  Top5 <- sum(Top5 * 100, na.rm = TRUE)
  Top5

  hhi_1 <- x/sum(x)
  hhi_2 <- hhi_1^2
  HHI <- sum(hhi_2)
  HHI

  Entropy <- sum(-x/sum(x)*log(x/sum(x), base = 2))
  Entropy <- Entropy/log(Firm, base = 2)
  Entropy

  Palma <- as.numeric(stats::na.omit(x))
  Palma <- sort(x, decreasing = FALSE)
  Palma_cut <- cut(x, stats::quantile(x, probs = seq(0,1, 0.1)),
                   include.lowest = TRUE, labels = FALSE)
  Palma_bottom <- sum(x[Palma_cut <= 4])
  Palma_top <- sum(x[Palma_cut > 9])
  Palma <- Palma_top/Palma_bottom
  Palma

  results_all <- data.frame(Measures = c("Firms", "Nrs_equivalent", "Top (%)",
                                       "Top3 (%)", "Top5 (%)", "HHI", "Entropy(RE)",
                                       "Palma ratio"),
                        Values = c(Firm, Nrs_equ, Top, Top3, Top5, HHI, Entropy,
                                   Palma))

  return(format(results_all, scientific = F, digits = 2, justify = "right"))

}

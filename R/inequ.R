#' @title Inequality and Diversity Measures
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  concstats_inequ(x, normalized = TRUE, type = c("entropy", "gini",
#'  "simpson", "palma", "grs", "all"), na.rm = TRUE, digits = NULL)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument of the functions
#'  \code{concstats_entropy}, \code{concstats_gini} specifying whether or not a
#'   normalized value is required. Must be either \code{TRUE} or \code{FALSE}.
#'   The default is \code{TRUE}.
#' @param type A character string of the measure to be calculated, defaults to
#'  `concstats_entropy`. Input is not case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{FALSE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7. Values are
#'  restricted between 1 and default value.
#' @details
#' * \code{concstats_inequ} is a wrapper for the proposed inequality measures.
#' All measures can be accessed individually.
#' * [concstats_entropy()] returns the Shannon entropy. \code{concstats_entropy}
#' You can normalize the entropy measures by setting \code{normalized = TRUE}.
#' * [concstats_gini()] calculates the gini coefficient. \code{concstats_gini}
#' You can normalize the gini measures by setting \code{normalized = TRUE}.
#' * [concstats_simpson()] calculates the gini-simpson index.
#' * [concstats_palma()] calculates the palma ratio of inequality.
#' * [concstats_grs()] calculates an alternative concentration measure.
#' * [concstats_all_inequ()] returns all measures in a one step procedure.
#'  For more details or references please see the help page of the respective
#'  function.
#'
#' @family Concentration and inequality measures
#' @return The calculated numeric measure or a `data frame`
#' @seealso [concstats_concstats()],[concstats_mstruct()],[concstats_comp()]
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#'
#' # Calculate the Palma ratio
#' concstats_inequ(x, type = "palma")
#'
#' # Calculate the entropy measure directly
#' concstats_inequ(x, normalized = TRUE, type = "entropy")
#'
#' # Calculate the group measures
#' concstats_inequ(x, type = "all", digits = 2)
#'
#' @export concstats_inequ
concstats_inequ <- function(x, normalized = TRUE, type = c("entropy", "gini",
                                                          "simpson", "palma",
                                                          "grs", "all"),
                            na.rm = TRUE, digits = NULL) {
  type <- tolower(as.character(type))
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()`
#' @srrstats {G2.0, G2.1} Assertions on types of inputs
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")
  if (!is.logical(normalized) || !length(normalized) == 1 || is.na(normalized)) {
    stop("`normalized` in `concstats_inequ` must be either TRUE or FALSE")
  }

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
           entropy = concstats_entropy(x, normalized = normalized,
                                       na.rm = na.rm, digits = digits),
           gini = concstats_gini(x),
           simpson = concstats_simpson(x, na.rm = na.rm, digits = digits),
           palma = concstats_palma(x, na.rm = na.rm, digits = digits),
           grs = concstats_grs(x, na.rm = na.rm, digits = digits),
           all = concstats_all_inequ(x, normalized = normalized, na.rm = na.rm,
                                     digits = digits))

}

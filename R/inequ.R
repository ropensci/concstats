#' @title Inequality and Diversity Measures
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  concstats_inequ(x, normalized = FALSE, type = c("entropy", "gini",
#'  "simpson", "palma", "grs", "all"), na.rm = TRUE, digits = NULL)
#'
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

  if (!is.logical(normalized) || !length(normalized) == 1 ||
      is.na(normalized)) {
    stop("`normalized` in `concstats_comp` must be either TRUE or FALSE")
  }

  if (!is.logical(na.rm) || !length(na.rm) == 1 || is.na(na.rm)) {
    stop("`na.rm` in `concstats_comp` must be either TRUE or FALSE")
  }
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


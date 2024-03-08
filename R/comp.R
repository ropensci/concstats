#' @title Group of Concentration Measures
#'
#' @description A set of different concentration and competition measures.
#'
#' @usage
#'  concstats_comp(x, normalized = FALSE, type = c("hhi", "hhi_d", "hhi_min",
#'  "dom", "sten", "all"), na.rm = TRUE, digits = NULL)
#'
#' @param x A non-negative numeric vector.
#' @param normalized Logical. Argument specifying whether or not a normalized
#'  value is required. Ranges from (0, 1) and often used for comparison over
#'  time. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{FALSE}.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "hhi". Input is not
#'  case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}
#'  if vector contains \code{NA} values.
#'  Must be either \code{TRUE} or \code{FALSE}. Defaults to \code{TRUE}.
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option.  Significant digits defaults to 7.
#' @details
#' * \code{concstats_comp} is a wrapper for the proposed concentration measures.
#'  All measures can be accessed individually.
#' * [concstats_hhi()] returns the Herfindahl-Hirschman index (HHI).
#'  \code{concstats_hhi}, can be calculated individually as a normalized
#'   measure changing the default setting to \code{TRUE}.
#' * [concstats_hhi_d()] returns the dual of the HHI.
#' * [concstats_hhi_min()] calculates the minimum of the HHI index.
#' * [concstats_dom()] calculates the dominance index.
#' * [concstats_sten()] calculates the stenbacka index.
#' * [concstats_all_comp()] is a wrapper that computes all measures in a one
#'   step procedure. For more details or references please see the help page of
#'   the respective function.
#'
#' @return A single numeric measure in decimal form or `data frame`.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total shares of individual firms/units. The vector should sum up to 1.
#'  Alternatively, the user might use [concstats_shares()] to converting raw
#'  variables, e.g. loans or sales into shares.
#'
#' @seealso [concstats_concstats()], [concstats_mstruct()], [concstats_inequ()]
#'
#' @family Competition/Concentration measures
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the Herfindahl-Hirschman index of the vector
#' concstats_comp(x, type = "hhi")
#' # individual measure
#' concstats_sten(x)
#' # complete group measures
#' concstats_comp(x, type = "all", digits = 2)
#'
#' @export concstats_comp
concstats_comp <- function(x, normalized = FALSE,
                           type = c("hhi", "hhi_d", "hhi_min", "dom", "sten",
                                    "all"), na.rm = TRUE, digits = NULL) {

  type <- tolower(as.character(type))

  if (!is.logical(normalized) || !length(normalized) == 1 ||
      is.na(normalized)) {
    warning("`normalized` in `concstats_comp` must be either TRUE or FALSE")
  }


  switch(match.arg(type),
         hhi = concstats_hhi(x, normalized = normalized, na.rm = na.rm),
         hhi_d = concstats_hhi_d(x, na.rm = na.rm),
         hhi_min = concstats_hhi_min(x, na.rm = na.rm),
         dom = concstats_dom(x, na.rm = na.rm),
         sten = concstats_sten(x, na.rm = na.rm),
         all = concstats_all_comp(x, normalized = normalized, na.rm = na.rm,
                                  digits = digits))

}


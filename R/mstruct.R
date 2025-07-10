#' @title Market Structure Measures
#' @srrstats {G1.4} roxygen2 used to document functions
#'
#' @description Set of different market structure measures to reflect a given
#'  market structure.
#'
#' @usage
#'  concstats_mstruct(x, type = c("firm", "nrs_eq", "top", "top3", "top5",
#'  "all"), na.rm = TRUE, digits = NULL)
#' @srrstats {G2.0a, G2.1a, EA1.1, EA1.3} accepted as input, length and type
#' @param x A non-negative numeric vector.
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "firm". Input is not
#'  case-sensitive.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'  be excluded or not. Must be either \code{TRUE} or \code{FALSE}. The default
#'  is \code{TRUE}. If set to \code{TRUE} the computation yields a message
#'  if the vector contains \code{NA} values. NAs will be removed for further
#'  computations. If set to \code{FALSE} with NAs present the computation
#'  yield \code{NA}.
#' @srrstats {EA4.1}  control of numeric precision
#' @param digits A non-null value for digits specifies the minimum number of
#'  significant digits to be printed in values. The default is \code{NULL} and
#'  will use base R print option. Significant digits defaults to 7. Values are
#'  restricted between 1 and default value.
#' @details
#' * \code{concstats_mstruct} is a wrapper for the proposed structural measures.
#' * [concstats_firm()], returns the number of firms with a given market share.
#' * [concstats_nrs_eq()] computes the reciprocal of the HHI, which indicates
#'  the equivalent number of firms of the same size.
#' * [concstats_top()], [concstats_top3()], and [concstats_top5()]
#'  calculate the cumulative share of the top (top 3 and top 5) firm(s) and
#'  returns the value in percentage.
#' * [concstats_all_mstruct()] computes all measures in a one step procedure.
#'  All measures can be computed individually.
#' * [concstats_top_df()], [concstats_top3_df()], and [concstats_top5_df()] are
#'  slight variations. Firm id or ranking might be of interest. In this case an
#'  additional id or firm variable is needed. The functions will return a data
#'  frame. These functions are just individually accessible.
#'
#' @return A single calculated numeric measure or `data frame`.
#' @note The vector of market shares should be in a decimal form corresponding
#'  to total shares of individual firms/units.The sum of the vector should sum up
#'  to 1. Alternatively, the user might use [concstats_shares()] to converting
#'  raw variables, e.g. loans or sales into shares.
#'
#' @seealso [concstats_concstats()],[concstats_comp()],[concstats_inequ()]
#'
#' @family Market structure measures
#'
#' @examples
#' # a vector of market shares
#' x <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' concstats_mstruct(x, type = "firm", digits = 1)
#'
#' # Calculate the market structure group measures
#' concstats_mstruct(x, type = "all", digits = 2)
#'
#' @export
concstats_mstruct <- function(x,
                              type = c("firm", "nrs_eq", "top", "top3",
                                       "top5", "all"),
                              na.rm = TRUE, digits = NULL) {
  type <- tolower(as.character(type))
#' @srrstats {G2.4, G2.4c} explicit conversion to character via as.character()
#' @srrstats {G2.3, G2.3b, G2.4c} used `tolower()`
#' @srrstats {G2.0, G2.1} Assertions on types of inputs

  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x, "n[0,)")

#' @srrstats {G2.3, G2.3a} Used `match.arg()`
    switch(match.arg(type),
           firm = concstats_firm(x, na.rm = na.rm),
           nrs_eq = concstats_nrs_eq(x, na.rm = na.rm),
           top = concstats_top(x, na.rm = na.rm),
           top3 = concstats_top3(x, na.rm = na.rm),
           top5 = concstats_top5(x, na.rm = na.rm),
           all = concstats_all_mstruct(x, na.rm = na.rm))
}

#' @title concstats:  Market Structure, Concentration, and Inequality Measures
#'
#' @description
#' Based on individual market shares of all participants in a
#' market or space, the package offers a set of different structural
#' and concentration measures frequently - and not so frequently - used
#' in research and in practice. Measures can be calculated in groups or
#' individually.
#' The calculated measure or the resulting vector of measures in table
#' format should help practitioners make more informed decisions.
#' To learn more about how to use concstats, see the vignettes at
#' \url{https://docs.ropensci.org/concstats/} or using the following
#' code in your console:
#'
#' `browseVignettes(package = "concstats")`
#'
#' This package provides a set of functions for calculating measures to
#' get insights of a given market. Measures are, e.g. the
#' Herfindahl-Hirschmann Index (HHI) or its dual, Entropy, Gini or
#' Palmer measure, as well as the Top market performer (by shares).
#' Monitoring markets is particularly useful for government agencies,
#' but also used in research or even on firm level.
#' While all functions rely on individual shares provided by the user,
#' the user might use a helper function to convert variables to shares.
#'
#' @name concstats-package
#' @aliases concstats
#' @docType package
#' @author Andreas Schneider \email{schneiderconsultingpy@@gmail.com}
#' @keywords internal
#'
#' @srrstats {G1.3} All statistical terminology is defined and explained.
#' @srrstats {G1.4, G1.4a} Roxygen is used for all documentation.
#' @srrstats {G1.2} implemented in CONTRIBUTING file
#' @srrstats {G2.0, G2.0a, G2.1, G2.1a, G2.2, G2.3, G2.3a, G2.3b}
#'   Input types are asserted and appropriately restricted and tested
#' @srrstats {G2.7. G2.8} `data.frame` and its extensions are supported.
#'   Input types are well defined and asserted.
#' @srrstats {G2.15} Functions never assume non-missingness.
#' @srrstats {G5.4c} Values used from published paper are used as an
#'  example in concstats-intro vignete.
#'
#' @srrstats {EA1.0} Target audience in README line#56 - 61
#' @srrstats {EA1.1} Kind of questions: in README line#55/56
#' @srrstats {EA1.2} Kind of questions: in README line#55/56
#' @srrstats {EA5.4} Rounding of value outputs are determined by the use
#'  with an additional attribute or default setting.
#'
"_PACKAGE"


## usethis namespace: start
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble
#' @importFrom utils browseVignettes
## usethis namespace: end
NULL

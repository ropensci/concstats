## usethis namespace: start
#' @importFrom stats na.omit
#' @importFrom utils browseVignettes
## usethis namespace: end
#'
#'
#' @title concstats:  Market Structure, Concentration, and Inequality Measures
#'
#' @description
#' Based on individual market shares of all participants in a
#' market or space, the package offers a set of different structural and
#' concentration measures frequently - and not so frequently - used in research
#' and in practice. Measures can be calculated in groups or individually.
#' The calculated measure or the resulting vector of measures in table format
#' should help practitioners make more informed decisions. To learn more about
#' how to use concstats, see the vignettes at
#' \url{https://docs.ropensci.org/concstats/}
#' or using the following code in your console:
#'
#' `browseVignettes(package = "concstats")`
#'
#' This package provides a set of functions for calculating measures to get
#' insights of a given market. Measures are, e.g. the Herfindahl-Hirschmann
#' Index (HHI) or its dual, Entropy, Gini or Palmer measure, as well as the Top
#' market performer (by shares). Monitoring markets is particularly useful for
#' government agencies, but also used in research or even on firm level.
#' While all functions rely on individual shares provided by the user, the user
#' might use a helper function to convert variables to shares.
#'
#' @name concstats-package
#' @aliases concstats
#' @docType package
#' @author Andreas Schneider \email{schneiderconsultingpy@@gmail.com}
#' @keywords internal
#'
#'
"_PACKAGE"




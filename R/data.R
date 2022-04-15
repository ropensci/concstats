#' @title Creditcoops
#' @description data set with 22 paired Paraguayan credit cooperatives
#'  (2016, 2018)
#' @format A data frame with 44 rows and 5 variables:
#' \describe{
#'   \item{\code{coop_id}}{double, ID of the credit cooperative}
#'   \item{\code{year}}{integer, sample year}
#'   \item{\code{total_loans}}{double, total loans granted (USD) per year and
#'    cooperative}
#'   \item{\code{paired}}{integer, paires of cooperatives}
#'   \item{\code{total_loans_log}}{double, the natural log of total loans}
#'}
#' @importFrom dplyr "%>%" mutate filter
#' @importFrom utils data head
#' @importFrom readr read_csv
#' @source \url{http://www.incoop.gov.py/v2/}
#' @author Andreas Schneider
#'
#' @note real names of the cooperatives have been purposely omitted, but are
#' available on request.
#'
#' @examples
#' data("creditcoops")
#' head(creditcoops)
"creditcoops"

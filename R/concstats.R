#' Market Structure and Concentration Measures.
#'
#' @description A convenience function which calculates a set of different
#' market structure and concentration measures more or less commonly used, e.g. k-firm ratios,
#' Entropy, HHI, and others.
#'
#' @usage concstats(x, na.rm = TRUE)
#' @param x A numeric vector
#' @param na.rm logical vector that indicates whether NA values should be excluded
#'
#' @return returns a vector of calculated measures in table format.
#' @note the vector of market shares should be in a decimal form corresponding to total
#'     share of individual firms/units (e.g. share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04 )).
#'     The sum of the vector/share should sum up to 1. The results are presented in a table
#'
#' @examples
#' share <- c(0.4, 0.266, 0.334, 0, 0) # a vector of market shares
#' con_share <- concstats(share)  # concentration measures of the vector
#'
#' @export
concstats <- function(x, na.rm = TRUE) {

  x <- replace(x, x==0, NA)

  if(na.rm==TRUE) {
    x <- x[!is.na(x)]
  }

  if(!is.numeric(x)) {
    stop('"x" must be a numeric vector\n',
         'You have provided an object of class: ', class(x)[1])
  }

  if(sum(is.na(x))) {
    stop('"x" is an empty vector\n',
         'All values will be 0')
  }

  if(sum(x>1)) {
    stop('"x" is not in a decimal form\n',
         'Please convert your vector into a decimal form')
  }

  if(sum(x==1)) {
    stop('"x" is a monopolistic market')
  }

  x <- sort(x, decreasing = TRUE)

  Top <- x[1]
  Top <- sum(Top * 100, na.rm = TRUE)
  Top

  Top3 <- x[1:3]
  Top3 <- sum(Top3 * 100, na.rm = TRUE)
  Top3

  Top5 <- x[1:5]
  Top5 <- sum(Top5 * 100, na.rm = TRUE)
  Top5

  x <- sort(x, decreasing = TRUE)
  Herf <- x/sum(x)
  Herf <- Herf^2
  Herf <- sum(Herf)
  Herf

  Firm <- sum(x >0)
  Firm

  Nrs_equ <- 1/Herf
  Nrs_equ

  HHI_min <- 1/Firm

  HHI_dual <- 1-1/(Firm*Herf)

  Dom_index <- x/sum(x)
  Dom_index <- (Dom_index^2/Herf)^2
  Dom_index <- sum(Dom_index)
  Dom_index

  Sten_index <- sort(x, decreasing = TRUE)
  Sten1 <- x[1]
  Sten2 <- x[2]
  Sten_index <- 0.5*(1-1*(Sten1^2-Sten2^2))
  Sten_index <- if(sum(Sten_index<1) || sum(Sten_index==1)) (Sten_index*100)
  Sten_index


  RE <- sum(-x/sum(x)*log(x/sum(x), base = 2))
  RE <- RE/log(Firm, base = 2)
  RE

  Berry <- 1-Herf

  results <- data.frame(measures=c("Firms", "Numbers equivalent", "Top (%)", "Top3 (%)", "Top5 (%)", "HHI", "HHI(min)", "HHI(dual)", "Dominance Index",
                                   "Stenbacka Index (%)", "RE", "Berry Index"),
                        values=c(Firm, Nrs_equ, Top, Top3, Top5, Herf, HHI_min, HHI_dual, Dom_index, Sten_index, RE, Berry))
  r_table <- knitr::kable(results, format = "simple", digits = 2)
}


#' @title Concentration Measures
#'
#' @description A set of different concentration measures.
#'
#' @usage
#'  comp(x, unbiased = FALSE, type = c("hhi", "hhi_d", "hhi_min", "dom",
#'  "sten", "all"),na.rm = TRUE)
#'
#' @param x A numeric vector of non-negative values.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'  correction should be applied.The default is FALSE.
#
#' @param type A character string of the measure to be calculated,
#'  can be abbreviated with the first letter. Defaults to "hhi".
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details
#'  \code{comp} is a wrapper for the proposed concentration measures \code{hhi},
#'  \code{hhi_d}, \code{hhi_min}, \code{dom}, \code{sten}, \code{all}.
#'  If no measure is specified "hhi" will be the default.
#'
#'  \code{hhi}, \code{hhi_min}, \code{hhi_d} calculate the Herfindahl-Hirschman
#'  index, its minimum, and its dual, respectivly. \code{dom} calculates a
#'  dominance index and \code{sten} calculates the Stenbacka index. The index
#'  indicates the market share of a dominante position.
#'
#' All measures can be accessed individually.
#'  \code{hhi}, \code{hhi_d}, and \code{dom} can be calculated individually as a
#'  normalized measure changing the default setting to \code{TRUE}. \code{all}
#'  computes all measures in a one step procedure.
#'
#'
#' @return prints the calculated measure
#' @note the vector of market shares should be in a decimal form corresponding
#'  to total share of individual firms/units.The sum of the vector should sum up
#'  to 1. You can also use sales figures to compute the respective measure.
#'
#' @references
#'  Chang, E. J., Guerra, S. M., de Souza Peñaloza, R. A. & Tabak, B. M. (2005)
#'  Measuring Banking concentration: the Brazilian case. In Financial Stability
#'  Report. Brasilia: Banco Central do Brasil, 4: 109-129
#' @references
#'  García Alba Iduñate, P. (1994). Un índice de dominación para el análisis de
#'  la estructura de los mercados. El Trimestre Económico, 61: 499-524
#' @references
#'  Melnik, A., Shy, O. & Stenbacka, R. (2008). Assessing market dominance.
#'  Journal of Economic Behavior & Organization 68: 63-72
#'
#' @seealso {\code{\link{concstats}}, \code{\link{mstruct}}, \code{\link{inequ}}}
#'
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.35, 0.4, 0.05, 0.1, 0.06, 0.04)
#' # the Herfindahl-Hirschman index of the vector
#' share_hhi <- comp(share, type = "hhi")
#' # individual measure
#' share_sten <- sten(share)
#' # complete group measures
#' share_comp <- comp(share, type = "all")
#'
#' @export comp
comp <- function(x, unbiased = FALSE, type = c("hhi", "hhi_d", "hhi_min", "dom",
                                               "sten", "all"), na.rm = TRUE)
{
  switch(match.arg(type),
         hhi = hhi(x, unbiased = unbiased, na.rm = na.rm),
         hhi_d = hhi_d(x, unbiased = unbiased, na.rm = na.rm),
         hhi_min = hhi_min(x, na.rm = na.rm),
         dom = dom(x, unbiased = unbiased, na.rm = na.rm),
         sten = sten(x, na.rm = na.rm),
         all = all_comp(x, na.rm = na.rm))
}

#' @export
#' @rdname comp
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
hhi <- function(x, unbiased = FALSE, na.rm = TRUE)
{
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
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  hhi_1 <- x/sum(x)
  hhi_2 <- hhi_1^2
  hhi <- sum(hhi_2)
  if (unbiased == TRUE) hhi <- (hhi - (1/sum(x > 0)))/(1 - (1/sum(x > 0)))
  return(hhi)
}

#' @export
#' @rdname comp
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
hhi_min <- function(x, na.rm = TRUE)
{
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
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  n <- sum(x > 0)
  hhi_min <- 1/n
  retrun(hhi_min)
}

#' @export
#' @rdname comp
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
hhi_d <- function(x, unbiased = FALSE, na.rm = TRUE)
{
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
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  hhi_1 <- x/sum(x)
  hhi_2 <- hhi_1^2
  hhi <- sum(hhi_2)
  hhi_d <- 1 - 1/(sum(x > 0)*hhi)
  if (unbiased) hhi_d <- 1 - 1/(sum(x > 0)*(hhi - (1/(sum(x > 0))))/(1 - (1/(sum(x > 0)))))
  retrun(hhi_d)
}

#' @export
#' @rdname comp
#' @param x A non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
dom <- function(x, unbiased = FALSE, na.rm = TRUE)
{
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
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  hhi_1 <- x/sum(x)
  hhi_2 <- hhi_1^2
  hhi <- sum(hhi_2)
  dom <- (hhi_2/hhi)^2
  dom <- sum(dom)
  if (unbiased) dom <- sum((hhi_2/((hhi - (1/(sum(x > 0))))/(1 - (1/(sum(
    x > 0))))))^2)
  return(dom)
}

#' @export
#' @rdname comp
#' @param x A non-negative numeric vector.
#' @param na.rm A logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
sten <- function(x, na.rm = TRUE)
{
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
  if (!na.rm && any(is.na(x))) return(NA_real_)
  x <- as.numeric(stats::na.omit(x))
  x <- sort(x, decreasing = TRUE)
  x <- x/sum(x)
  sten1 <- x[1]
  sten2 <- x[2]
  sten <- 0.5*(1 - 1*(sten1^2 - sten2^2))
  sten <- if (sum(sten < 1) || sum(sten == 1)) (sten * 100)
  return(sten)
}

all_comp <- function(x, na.rm = TRUE)
{
  invisible(utils::capture.output(
    hhi <- hhi(x, unbiased = FALSE, na.rm = TRUE),
    hhi_d <- hhi_d(x, unbiased = FALSE, na.rm = TRUE),
    hhi_min <- hhi_min(x, na.rm = TRUE),
    dom <- dom(x, unbiased = FALSE, na.rm = TRUE),
    sten <- sten(x, na.rm = TRUE)))
  results_comp <- data.frame(Measure = c("HHI", "HHI(min)", "HHI(dual)",
                                       "Dominance", "Stenbacka(%)"),
                             Value = c(hhi, hhi_min, hhi_d, dom, sten))

  return(format(results_comp, scientific = F, digits = 2, justify = "right"))
}

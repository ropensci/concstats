#' @title Inequality and Diversity Measures
#'
#' @description A set of different inequality and diversity measures.
#'
#' @usage
#'  inequ(x, unbiased = FALSE, type = c("entropy", "gini", "berry", "palma",
#'  "grs", "all"), na.rm = TRUE)
#'
#' @param x a numeric vector of non-negative values.
#' @param unbiased Logical. Argument of the function \code{entropy}, \code{gini},
#'  and \code{berry} specifying whether or not a finite sample correction should
#'  be applied.
#' @param type a character string of the measure to be calculated, defaults to
#'  "entropy".
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not. If set to \code{FALSE} the computation yields \code{NA}.
#'
#' @details
#'  \code{inequ} is a wrapper for the proposed inequality measures \code{entropy},
#'  \code{gini}, \code{berry}, \code{palma}, \code{grs}, \code{all}.
#'  If no measure is specified, "entropy" is the default.
#'
#' \code{entropy} returns the (Shannon) Entropy, \code{gini} is the
#'  Gini coefficient, \code{berry} is the inverse of the Herfindahl-Hirschman
#'  Index. You can normalize each of these three measures by setting
#'  \code{unbiased = TRUE}
#'
#' \code{Palma} measures the ratio of inequality (used with income inequality)
#'  of the top 10 percent to the bottom 40 percent. \code{grs} is an alternative
#'  inequality measure (Ginevicius, 2009), \code{all} returns all measures in
#'  a one step procedure.
#'
#' @return prints the calculated measure
#' @note the non- negative vector might be sales figures or market shares of
#'  individual firms/units. In the latter case the vector should sum up to 1.
#'
#' @references C.E. Shannon (1948). A Mathematical Theory of Communication,
#'  The Bell System Technical Journal (Nokia Bell Labs)
#' @references J.G. Palma (2006). Globalizing Inequality:
#'  'Centrifugal' and 'Centripetal' Forces at Work, DESA Working Paper No. 35.
#' @references A. Cobham / A. Summer (2013). Is It All About the Tails?
#'  The Palma Measure of Income Inequality, Center for Global Development,
#'  Washington, DC.
#' @references R. Ginevicius and S. Cirba (2009). Additive measurement of
#'  market concentration, Journal of Business Economics and Management,
#'   10(3), 191-198. \url{https://doi.org/10.3846/1611-1699.2009.10.191-198}.
#'
#' @seealso {\code{\link{concstats}}, \code{\link{mstruct}}, \code{\link{comp}}}
#'
#' @examples
#' # a vector of market shares
#' share <- c(0.4, 0.2, 0.25, 0.1, 0.05)
#' # Calculate the Palma ratio
#' share_p <- inequ(share, type = "palma")
#' # Calculate the entropy measure directly
#' share_ent <- entropy(share, unbiased = TRUE)
#' # Calculate the group measures
#' share_inequ <- inequ(share, type = "all")
#'
#' @export inequ
inequ <- function(x, unbiased = FALSE, type = c("entropy", "gini", "berry",
                                                "palma", "grs", "all"),
                  na.rm = TRUE)
{
  switch(match.arg(type),
         entropy = entropy(x, unbiased = unbiased, na.rm = na.rm),
         gini = gini(x, unbiased = unbiased, na.rm = na.rm),
         berry = berry(x, unbiased = unbiased, na.rm = na.rm),
         palma = palma(x, na.rm = na.rm),
         grs = grs(x, na.rm = na.rm),
         all = all_inequ(x, na.rm = na.rm))
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
entropy <- function(x, unbiased = FALSE, na.rm = TRUE)
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
  k <- sum(x > 0)
  entropy <- sum(-x/sum(x)*log(x/sum(x), base = 2))
  if (unbiased == TRUE) entropy <- entropy/log(k, base = 2)
  entropy
  print(entropy)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
gini <- function(x, unbiased = FALSE, na.rm = TRUE)
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
  x <- sort(x)
  n <- length(x)
  gini <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
  if (unbiased) gini <- n / (n - 1) * gini
  gini
  print(gini)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param unbiased Logical. Argument specifying whether or not a finit sample
#'   correction should be applied.The default is FALSE.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'  be excluded or not.
berry <- function(x, unbiased = FALSE, na.rm = TRUE)
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
  h <- x/sum(x)
  h <- h^2
  h <- sum(h)
  berry <- 1 - h
  if (unbiased) berry <- 1 - h/(sum(x/sum(x)))^2
  berry
  print(berry)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
palma <- function(x, na.rm = TRUE)
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
  x <- sort(x, decreasing = FALSE)
  x_cut <-  cut(x, stats::quantile(x, probs = seq(0,1, 0.1)),
                include.lowest = TRUE, labels = FALSE)
  x_bottom <- sum(x[x_cut <= 4])
  x_top <- sum(x[x_cut > 9])
  palma <- x_top/x_bottom
  palma
  print(palma)
}

#' @export
#' @rdname inequ
#' @param x a non-negative numeric vector.
#' @param na.rm a logical vector that indicates whether \code{NA} values should
#'   be excluded or not.
grs <- function(x, na.rm = TRUE)
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
  top <- x[1]
  firm <- sum(x > 0)
  firm2 <- firm^2
  grs <- sum((firm2*top + 0.3*x^2)/(firm2 + firm*0.3*top*x)*x)
  grs
  print(grs)
}

all_inequ <- function(x, na.rm = TRUE)
{
  invisible(utils::capture.output(
    entropy <- entropy(x, unbiased = FALSE, na.rm = TRUE),
    gini <- gini(x, unbiased = FALSE, na.rm = TRUE),
    berry <- berry(x, unbiased = FALSE, na.rm = TRUE),
    palma <- palma(x, na.rm = TRUE),
    grs <- grs(x, na.rm = TRUE)))
  results_inequ <- data.frame(Measure = c("Entropy", "Gini", "Berry Index",
                                             "Palma Ratio", "GRS"),
                              Value = c(entropy, gini, berry, palma, grs))

  print(format(results_inequ, scientific = F, digits = 2, justify = "right"))

}

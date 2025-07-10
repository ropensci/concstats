#' @title Top market share data frame
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @inheritParams concstats_top_df
#'
#' @examples
#' x <- data.frame(
#' firm = c("A", "B", "C", "D", "E"),
#' share = c(0.2,0.25,0.1,0.05,0.4)
#' )
#' concstats_top3_df(x, "share", digits = 2)
#'
#' @export
concstats_top3_df <- function(x, y, digits = NULL) {
#' @srrstats {G2.1} Assertions on types of inputs
#' @srrstats {G5.8a} Zero-length data
#' @srrstats {G2.2, G2.6, G2.16} Checking class, type, NaN handling
  # Checking if an argument is a data frame with specific column names
  checkmate::assert_data_frame(x, types = c("numeric", "character"),
                               col.names = "unique",
                               .var.name = "x")
  # Check if y is numeric
  checkmate::assert_numeric(x[ ,y], any.missing = TRUE, .var.name = "y")
  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x[ ,y], "n[0,)")
  if (!is.data.frame(x)) {
    stop("`x` in concstats_top3_df must be a data frame\n",
         "You have provided an object of class:", class(x)[1])
  }
#' @srrstats {G2.0, G2.14a, G2.14b} Implement assertions on lengths of inputs
  if (anyNA(x)) {
    message(paste("NA values have been removed before the calculation for the following variable: ", y))
  }

  x <- x |> dplyr::filter(complete.cases(x))

#' @srrstats {G2.4b} explicit conversion to continuous via `as.numeric()`
#' @srrstats {G2.15} Functions should never assume non-missingness for base routine sum()
#' @srrstats {G3.0}  using tolerances for approximate equality.

  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(na.omit(x[,y] < 1))) &&
                 !isTRUE(all.equal(sum(na.omit(x[,y])), 1,
                                   tolerance = .Machine$double.eps^0.25))) {
    stop(paste("The following vector in `concstats_top3_df` does not sum to 1: ", y))
  }

  x <- x |> dplyr::filter(complete.cases(x))
  x[ ,y] <- x[ ,y] / sum(x[ ,y])
  x <- x[order(x[ ,y], decreasing = TRUE),]
  index  <- x[, -1] %% 1 != 0
  x[, -1][index] <- x[, -1][index] *100


  top3_df <- x[1:3,]
  if (!is.null(digits)) top3_df[,-1] <- as.numeric(round(top3_df[ ,y],
                                                        digits = digits))
#' @srrstats {EA4.0, EA4.2, EA5.3, EA5.4} output type same as input type
  return(as.data.frame(top3_df))

}

#' @title Top 5 market shares data frame
#'
#' @srrstats {G1.4} roxygen2 used to document functions
#' @inheritParams concstats_top_df
#' @return A `data frame`.
#' @family Market structure measures
#' @rdname concstats_top5_df
#'
#' @examples
#' x <- data.frame(
#' firm = c("A", "B", "C", "D", "E"),
#' share = c(0.2,0.25,0.1,0.05,0.4)
#' )
#' concstats_top5_df(x, "share", digits = 2)
#'
#' @export
concstats_top5_df <- function(x, y, digits = NULL) {
  #' @srrstats {G2.1} Assertions on types of inputs
  #' @srrstats {G5.8a} Zero-length data
  #' @srrstats {G2.2, G2.6, G2.7, G2.16} Checking class, type, NaN handling
  #' @srrstats {EA2.6}
  # Checking if an argument is a data frame with specific column names
  checkmate::assert_data_frame(x, types = c("numeric", "character"),
                               col.names = "unique",
                               .var.name = "x")
  checkmate::assert_int(x = digits, lower = 1, null.ok = TRUE)
  checkmate::qassert(x[ ,y], "n[0,)")

  x <- tibble::as_tibble(x)

  #' @srrstats {G2.10, G2.11, G2.12} data frame pre-processing
  if (anyNA(x)) {
    message(paste("NA values have been removed before the calculation for the following variable: ", y))
  }
  # check sum of vector. Must sum to 1 if all x(market share) < 1
  if (as.logical(all(na.omit(x[[2]] < 1))) &&
      !isTRUE(all.equal(sum(na.omit(x[[2]])), 1,
                        tolerance = .Machine$double.eps^0.25))) {
    stop(paste("The following vector in `concstats_top5_df` does not sum to 1: ", y))
  }
  x[[2]] <- x[[2]] / sum(x[[2]], na.rm = TRUE)
  x <- x[order(x[[2]], decreasing = TRUE),]
  #index  <- x[, -1] %% 1 != 0
  x[, -1] <- x[, -1] *100

  #' @srrstats {G2.4b} explicit conversion to continuous via `as.numeric()`
  top_df5 <- x[1:5,]
  if (!is.null(digits)) top_df5[,-1] <- as.numeric(round(top_df5[[2]],
                                                         digits = digits))
  #' @srrstats {EA4.0, EA4.2, EA5.3, EA5.4} output type same as input type
  return(top_df5)
}

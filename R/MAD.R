#' Median Absolute Deviation
#'
#' Compute the median absolute deviation.
#'
#' @export MAD
#' @param x 1-d signal.
#' @examples
#' x <- c(1, 1, 2, 2, 4, 6, 9)
#' MAD(x)

MAD <- function(x) {
  return(median(abs(x - median(x)))/0.6745)
}

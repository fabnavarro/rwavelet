#' Median Absolute Deviation
#'
#' Compute the median absolute deviation.
#'
#' @export MAD
#' @param x 1-d signal.
#' @examples
#' \dontrun{
#' MAD(x)
#' }

MAD <- function(x) {
  return(median(abs(x - median(x)))/0.6745)
}

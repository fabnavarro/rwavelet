#' Apply Soft Threshold.
#'
#' @export SoftThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return \code{x} filtered result (y 1_{|y|>t}).
#' @examples
#' \dontrun{
#' SoftThresh(y,t)
#' }

SoftThresh <- function(y, t) {
  res <- (abs(y) - t)
  res <- (res + abs(res))/2
  x <- sign(y) * res
  return(x)
}

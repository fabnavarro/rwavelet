#' Apply Soft Threshold.
#'
#' @export SoftThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return \code{x} filtered result (y 1_{|y|>t}).
#' @examples
#' f <- MakeSignal('HeaviSine', 2^3)
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' wc <- FWT_PO(f, L, qmf)
#' thr <- 2
#' wct <- SoftThresh(wc, thr)
#' fsoft <- IWT_PO(wct, L, qmf)

SoftThresh <- function(y, t) {
  res <- (abs(y) - t)
  res <- (res + abs(res))/2
  x <- sign(y) * res
  return(x)
}

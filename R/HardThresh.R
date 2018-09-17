#' Apply Hard Threshold.
#'
#' @export HardThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return \code{x} filtered result (y 1_{|y|>t}).
#' @examples
#' f <- MakeSignal('HeaviSine',2^3)
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' wc <- FWT_PO(f, L, qmf)
#' thr <- 2
#' wct <- HardThresh(wc, thr)
#' fhard <- IWT_PO(wct, L, qmf)

HardThresh <- function(y, t) {
  x <- y * (abs(y) > t)
  return(x)
}

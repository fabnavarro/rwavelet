#' Apply James-Stein Threshold
#'
#' (also called the nonnegative garrote)
#'
#' @export JSThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return \code{x} filtered result.
#' @examples
#' f <- MakeSignal('HeaviSine', 2^3)
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' wc <- FWT_PO(f, L, qmf)
#' thr <- 2
#' wct <- JSThresh(wc, thr)
#' fsoft <- IWT_PO(wct, L, qmf)
#' @seealso \code{\link{HardThresh}}, \code{\link{SoftThresh}}

JSThresh <- function(y, t) {
  x <- (y - t^2/y) * (abs(y) > t)
  return(x)
}

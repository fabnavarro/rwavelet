#' Signal/Noise ratio
#'
#' @export SNR
#' @param x Original reference signal.
#' @param y Restored or noisy signal
#' @return Signal/Noise ratio.
#' @examples
#' n <- 2^4
#' x <- MakeSignal('HeaviSine', n)
#' y <- x + rnorm(n, mean=0, sd=1)
#' SNR(x, y)

SNR <- function(x, y) {
  v <- 20 * log10(normvec(as.vector(x))/normvec(as.vector(x) - as.vector(y)))
  return(v)
}

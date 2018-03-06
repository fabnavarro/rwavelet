#' Signal/Noise ratio
#'
#' @export SNR
#' @param x Original reference signal.
#' @param y Restored or noisy signal
#' @return Signal/Noise ratio.
#' @examples
#' \dontrun{
#' value=SNR(x,y)
#' }

SNR <- function(x, y) {
  v <- 20 * log10(normvec(as.vector(x))/normvec(as.vector(x) - as.vector(y)))
  return(v)
}

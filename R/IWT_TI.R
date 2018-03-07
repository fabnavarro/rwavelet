#' Translation Invariant Iorward Wavelet Transform.
#'
#'
#' @export IWT_TI
#' @param pkt translation-invariant wavelet transform table (TIWT).
#' @param qmf orthonormal quadrature mirror filter.
#' @return \code{x} 1-d signal reconstructed from translation-invariant transform TIWT.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' L <- 0
#' qmf <- MakeONFilter('Haar')
#' TIWT <- FWT_TI(x, L, qmf)
#' xr <- IWT_TI(TIWT,qmf)
#' @seealso \code{\link{FWT_TI}}, \code{\link{MakeONFilter}}.

IWT_TI <- function(pkt, qmf) {
  n <- dim(pkt)[1]
  D1 <- dim(pkt)[2]
  D <- D1 - 1
  J <- log2(n)
  wp <- pkt
  sig <- t(wp[, 1])
  for (d in seq(D - 1, 0, -1)) {
    for (b in 0:(2^(d) - 1)) {
      hsr <- t(wp[packet(d + 1, 2 * b, n), d + 2])
      hsl <- t(wp[packet(d + 1, 2 * b + 1, n), d + 2])
      lsr <- sig[packet(d + 1, 2 * b, n)]
      lsl <- sig[packet(d + 1, 2 * b + 1, n)]
      loterm <- (UpDyadLo(lsr, qmf) + lshift(UpDyadLo(lsl, qmf)))/2
      hiterm <- (UpDyadHi(hsr, qmf) + lshift(UpDyadHi(hsl, qmf)))/2
      sig[packet(d, b, n)] <- loterm + hiterm
    }
  }
  x <- as.vector(t(sig))
  return(x)
}

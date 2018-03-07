#' Translation Invariant Forward Wavelet Transform.
#'
#' 1. \code{qmf} filter may be obtained from \code{\link{MakeONFilter}}.
#' 2. usually, \code{length(qmf) < 2^(L+1)}.
#' 3. To reconstruct use \code{\link{IWT_TI}}.
#'
#' @export FWT_TI
#' @param x array of dyadic length n=2^J.
#' @param L degree of coarsest scale.
#' @param qmf orthonormal quadrature mirror filter.
#' @return \code{TIWT} stationary wavelet transform table.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' L <- 0
#' qmf <- MakeONFilter('Haar')
#' TIWT <- FWT_TI(x, L, qmf)
#' @seealso \code{\link{IWT_TI}}, \code{\link{MakeONFilter}}.

FWT_TI <- function(x, L, qmf) {
  n <- dyadlength(x)$x
  J <- dyadlength(x)$y
  D <- J - L
  wp <- matrix(0, n, D + 1)
  x <- ShapeAsRow(x)
  wp[, 1] <- t(x)
  for (d in 0:(D - 1)) {
    for (b in 0:(2^d - 1)) {
      s <- t(wp[packet(d, b, n), 1])
      hsr <- DownDyadHi(s, qmf)
      hsl <- DownDyadHi(rshift(s), qmf)
      lsr <- DownDyadLo(s, qmf)
      lsl <- DownDyadLo(rshift(s), qmf)
      wp[packet(d + 1, 2 * b, n), d + 2] <- t(hsr)
      wp[packet(d + 1, 2 * b + 1, n), d + 2] <- t(hsl)
      wp[packet(d + 1, 2 * b, n), 1] <- t(lsr)
      wp[packet(d + 1, 2 * b + 1, n), 1] <- t(lsl)
    }
  }
  return(wp)
}

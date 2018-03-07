#' Forward Wavelet Transform (periodized, orthogonal).
#'
#' 1. \code{qmf} filter may be obtained from \code{\link{MakeONFilter}}.
#' 2. usually, \code{length(qmf) < 2^(L+1)}.
#' 3. To reconstruct use \code{\link{IWT_PO}}.
#'
#' @export FWT_PO
#' @param x 1-d signal; length(x) = 2^J.
#' @param L Coarsest Level of V_0;  L << J.
#' @param qmf quadrature mirror filter (orthonormal).
#' @return \code{wc} 1-d wavelet transform of x.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' L <- 0
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT_PO(x, L, qmf)
#' @seealso \code{\link{IWT_PO}}, \code{\link{MakeONFilter}}.

FWT_PO <- function(x, L, qmf) {
  n <- dyadlength(x)$x
  J <- dyadlength(x)$y
  wcoef <- rep(0, n)
  beta <- ShapeAsRow(x)
  for (j in seq(J - 1, L, -1)) {
    alfa <- DownDyadHi(beta, qmf)
    wcoef[dyad(j)] <- alfa
    beta <- DownDyadLo(beta, qmf)
  }
  wcoef[1:(2^L)] <- beta
  return(wcoef)
}
# a modifier apres la creation de ShapeLike

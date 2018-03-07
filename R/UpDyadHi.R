#' Hi-Pass Upsampling operator; periodized
#'
#'
#' @export UpDyadHi
#' @param x 1-d signal at coarser scale.
#' @param qmf filter.
#' @return \code{u} 1-d signal at finer scale.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' UpDyadHi(x,qmf)
#' @seealso \code{\link{DownDyadLo}}, \code{\link{DownDyadHi}},
#' \code{\link{UpDyadLo}}, \code{\link{IWT_PO}}, \code{\link{aconv}}.

UpDyadHi <- function(x, qmf) {
  y <- aconv(MirrorFilt(qmf), rshift(UpSampleN(x)))
  return(y)
}

#'  Spike-plot display of wavelet coefficients
#'
#' @export PlotWaveCoeff
#' @param wc 1-d wavelet transform.
#' @param L level of coarsest scale.
#' @param scal scale factor (0 ==> autoscale).
#' @return A display of wavelet coefficients (coarsest level
#'         NOT included) by level and position.
#' @examples
#' x <- MakeSignal('Ramp', 128)
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 3
#' scal <- 1
#' wc <- FWT_PO(x, L, qmf)
#' PlotWaveCoeff(wc,L,scal)
#' @seealso \code{\link{FWT_PO}}, \code{\link{IWT_PO}}, \code{\link{PlotSpikes}}.
#' @import graphics

PlotWaveCoeff <- function(wc, L, scal) {
  wavecoef <- ShapeAsRow(wc)
  n <- dyadlength(wavecoef)$x
  J <- dyadlength(wavecoef)$y
  if (scal == 0) {
    scal <- 1/max(abs(wavecoef[(2^L + 1):n]))
  }
  for (j in seq(J - 1, L, -1)) {
    tj <- (0.5:(2^(j) - 0.5))/2^(j)
    PlotSpikes(-j, tj, wavecoef[dyad(j)] * scal, L, J)
    par(new = TRUE)
  }
  axis(2, at = seq(-J + 1, -L, 1), labels = seq(-J + 1, -L, 1))
  axis(1, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2))
  box()
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

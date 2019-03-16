#'  Plot 1-d signal as baseline with series of spikes
#'
#' @export PlotSpikes
#' @param base number, baseline level.
#' @param t ordinate values.
#' @param x 1-d signal, specifies spike deflections from baseline.
#' @param L level of coarsest scale.
#' @param J least power of two greater than n.
#' @return A plot of spikes on a baseline.
#' @examples
#' \dontrun{
#' PlotSpikes(base, t, x, L, J)
#' }
#' @seealso \code{\link{PlotWaveCoeff}}.
#' @import stats graphics

PlotSpikes <- function(base, t, x, L, J) {
  tt <- rbind(t, t, t)
  b <- rep(0, length(x)) + base
  xx <- rbind(b, x + base, b)
  u <- cbind(0, as.vector(tt), 1)
  v <- cbind(base, as.vector(xx), base)
  return(plot(u, v, type = "l", xlim = c(0, 1), ylim = c(-J, -L + 1), axes = FALSE,
    xlab = "", ylab = ""))
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

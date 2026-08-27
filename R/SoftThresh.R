#' Apply Soft Threshold
#'
#' @export SoftThresh
#' @param y Noisy Data.
#' @param t Threshold.
#' @return A numeric vector with entries
#'   \eqn{x_i = \mathrm{sign}(y_i)\max(|y_i| - t, 0)}{x_i = sign(y_i) * max(abs(y_i) - t, 0)}.
#' @examples
#' f <- MakeSignal('HeaviSine', 2^3)
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' wc <- FWT_PO(f, L, qmf)
#' thr <- 2
#' wct <- SoftThresh(wc, thr)
#' fsoft <- IWT_PO(wct, L, qmf)
#' @seealso \code{\link{HardThresh}}

SoftThresh <- function(y, t) {
  res <- (abs(y) - t)
  res <- (res + abs(res))/2
  x <- sign(y) * res
  return(x)
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

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
#' @seealso \code{\link{HardThresh}}

JSThresh <- function(y, t) {
  x <- (y - t^2/y) * (abs(y) > t)
  return(x)
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

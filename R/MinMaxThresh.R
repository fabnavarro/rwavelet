#' Minimax Thresholding
#'
#' @export MinMaxThresh
#' @param y signal upon which to perform thresholding.
#' @return \code{x} result.
#' @references
#' D.L. Donoho and I.M. Johnstone (1994). Ideal spatial adaptation by wavelet shrinkage. \emph{Biometrika}, 81(3), 425--455.

MinMaxThresh <- function(y) {
  lamlist <- c(0, 0, 0, 0, 0, 1.27, 1.47, 1.67, 1.86, 2.05, 2.23, 2.41, 2.6,
    2.77, 2.95, 3.13)
  d <- dyadlength(y)
  j <- d$y
  n <- d$x
  lam <- lamlist[j]
  x <- HardThresh(y, lam)
  return(x)
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

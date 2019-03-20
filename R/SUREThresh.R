#' Adaptive Threshold Selection Using Principle of SURE
#'
#' SURE referes to Stein's Unbiased Risk Estimate.
#'
#' @export SUREThresh
#' @param y  Noisy Data with Std. Deviation = 1.
#' @return \code{x} Estimate of mean vector
#' @return \code{thresh} Threshold used.

SUREThresh <- function(y) {
  thresh <- ValSUREThresh(y)
  x <- HardThresh(y, thresh)
  return(list(x=x,thresh=thresh))
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

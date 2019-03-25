#' Adaptive Threshold Selection Using Principle of SURE
#'
#' SURE referes to Stein's Unbiased Risk Estimate.
#'
#' @export ValSUREThresh
#' @param x  Noisy Data with Std. Deviation = 1.
#' @return \code{thresh} Value of Threshold.

ValSUREThresh <- function(x) {
  a <- sort(abs(x))^2
  b <- cumsum(a)
  n <- length(x)
  c <- seq(n - 1, 0)
  s <- b + c * a
  risk <- (n - (2 * (1:n)) + s)/n
  ibest <- which.min(risk)
  thresh <- sqrt(a[ibest])
  return(thresh)
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

#' Visually calibrated Adaptive Smoothing
#'
#' @export VisuThresh
#' @param y Signal upon which to perform visually calibrated Adaptive Smoothing.
#' @param thresh 'hard' or 'soft'.
#' @return \code{x} result of applying VisuThresh.
#' @references
#' D.L. Donoho and I.M. Johnstone (1994). Ideal spatial adaptation by wavelet shrinkage. \emph{Biometrika}, 81(3), 425--455.

VisuThresh <- function(y, thresh = "soft") {
  thr <- sqrt(2 * log(length(y)))
  if (thresh == "hard") {
    x <- HardThresh(y, thr)
  }
  if (thresh == "soft") {
    x <- SoftThresh(y, thr)
  }
  # else { }
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

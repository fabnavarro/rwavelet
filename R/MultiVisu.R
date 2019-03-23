#' Apply Universal Thresholding to Wavelet Coefficients
#'
#' @export MultiVisu
#' @param wc Wavelet Transform of noisy sequence with N(0,1) noise.
#' @param L low-frequency cutoff for Wavelet Transform
#' @return \code{x} result of applying VisuThresh to each High Frequency Dyadic Block.

MultiVisu <- function(wc, L) {
  d <- dyadlength(wc)
  J <- d$y
  n <- d$x
  ws <- wc
  ws[(2^(L) + 1):n] <- VisuThresh(wc[(2^(L) + 1):n])
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

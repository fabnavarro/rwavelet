#' Apply Shrinkage with level-dependent Noise level estimation
#'
#' @export MultiMAD
#' @param wc Wavelet Transform of noisy sequence.
#' @param L low-resolution cutoff for Wavelet Transform.
#' @return \code{ws} result of applying VisuThresh to each wavelet level,
#'                   after scaling so MAD of coefficienst at each level = .6745

MultiMAD <- function(wc, L) {
  d <- dyadlength(wc)
  J <- d$y
  n <- d$x
  for (j in (J - 1):L) {
    scale <- median(abs(wc[dyad(j)]))/0.6745
    wc[dyad(j)] <- VisuThresh(wc[dyad(j)]/scale)
  }
  ws <- wc
  return(ws)
}

# Copyright (c) 1993-5.  Jonathan Buckheit, David Donoho and Iain Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

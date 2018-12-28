#' Circular left shift of 1-d signal
#'
#' @export lshift
#' @param a 1-d signal.
#' @return \code{l} 1-d signal l(i) = x(i+1) except l(n) = x(1).
#' @examples
#' x <- MakeSignal('HeaviSine',2^3)
#' lshift(x)

lshift <- function(a) {
  return(c(a[2:length(a)], a[1]))
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of WaveLab Version 802 Built Sunday, October 3, 1999 8:52:27 AM This
# is Copyrighted Material For Copying permissions see COPYING.m Comments?
# e-mail wavelab@stat.stanford.edu

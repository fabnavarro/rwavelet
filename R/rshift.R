#' Circular right shift of 1-d signal
#'
#' @export rshift
#' @param a 1-d signal.
#' @return \code{r} 1-d signal r(i) = x(i-1) except r(1) = x(n).
#' @examples
#' x <- MakeSignal('HeaviSine', 2^3)
#' rshift(x)

rshift <- function(a) {
  n <- length(a)
  return(c(a[n], a[1:(n - 1)]))
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of WaveLab Version 802 Built Sunday, October 3, 1999 8:52:27 AM This
# is Copyrighted Material For Copying permissions see COPYING.m

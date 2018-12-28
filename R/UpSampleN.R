#' Upsampling operator
#'
#' @export UpSampleN
#' @param x 1-d signal, of length n.
#' @param s upsampling scale, default = 2.
#' @return \code{y} 1-d signal, of length s*n with zeros
#'                  interpolating alternate samples
#'                  y(s*i-1) = x(i), i=1,...,n

UpSampleN <- function(x, s) {
  if (missing(s)) {
    s <- 2
  }
  n <- length(x) * s
  y <- rep(0, n)
  y[seq(1, n - s + 1, s)] <- x
  return(y)
}

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:39 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

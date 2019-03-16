#' Convolution tool for two-scale transform
#'
#' Filtering by periodic convolution of x with f.
#'
#' @export iconvv
#' @param f filter.
#' @param x 1-d signal.
#' @return \code{y} filtered result.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' iconvv(qmf,x)
#' @seealso \code{\link{aconv}}, \code{\link{UpDyadHi}},
#' \code{\link{UpDyadLo}}, \code{\link{DownDyadHi}}, \code{\link{DownDyadLo}}.

iconvv <- function(f, x) {
  n <- length(x)
  p <- length(f)
  if (p <= n) {
    xpadded <- c(x[(n + 1 - p):n], x)
  } else {
    z <- rep(0, p)
    for (i in 1:p) {
      imod <- 1 + ((p * n - p + i - 1)%%n)
      z[i] <- x[imod]
    }
    xpadded <- c(z, x)
  }
  ypadded <- signal::filter(f, 1, xpadded)
  return(ypadded[(p + 1):(n + p)])
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

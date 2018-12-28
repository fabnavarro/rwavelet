#' Inverse 2-d MRA wavelet transform (periodized, orthogonal).
#'
#' If \code{wc} is the result of a forward 2d wavelet transform, with \code{wc <- FWT2_PO(x,L,qmf)}.
#' then \code{x <- IWT2_PO(wc,L,qmf)} reconstructs \code{x} exactly
#' \code{qmf} is a nice qmf, e.g. one made by \code{\link{MakeONFilter}}.
#'
#' @export IWT2_PO
#' @param wc 2-d wavelet transform (n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{x} 2-d signal reconstructed from wc.
#' @examples
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 3
#' x <- matrix(rnorm(128^2),ncol=128)
#' wc <- FWT2_PO(x, L, qmf)
#' xr <- IWT2_PO(wc,L,qmf)
#' @seealso \code{\link{FWT2_PO}}, \code{\link{MakeONFilter}}.

IWT2_PO <- function(wc, L, qmf) {
  q <- quadlength(wc)
  J <- q$y
  x <- wc
  nc <- 2^(L + 1)
  for (jscal in L:(J - 1)) {
    top <- (nc/2 + 1):nc
    bot <- 1:(nc/2)
    all <- 1:nc
    for (iy in 1:nc) {
      x[all, iy] <- t(UpDyadLo(t(x[bot, iy]), qmf)) + t(UpDyadHi(t(x[top, 
        iy]), qmf))
    }
    for (ix in 1:nc) {
      x[ix, all] <- UpDyadLo(x[ix, bot], qmf) + UpDyadHi(x[ix, top], qmf)
    }
    nc <- 2 * nc
  }
  return(x)
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

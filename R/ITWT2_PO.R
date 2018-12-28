#' Inverse 2-d tensor wavelet transform (periodized, orthogonal).
#'
#' If \code{wc} is the result of a forward 2d wavelet transform,
#' with \code{wc <- FTWT2_PO(x,L,qmf)}, then \code{x <- ITWT2_PO(wc,L,qmf)}
#' reconstructs \code{x} exactly.
#' \code{qmf} is a nice qmf, e.g. one made by \code{\link{MakeONFilter}}.
#'
#' @export ITWT2_PO
#' @param wc 2-d wavelet transform (n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{x} 2-d signal reconstructed from wc.
#' @examples
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 0
#' x <- matrix(rnorm(2^2), ncol=2)
#' wc <- FTWT2_PO(x, L, qmf)
#' xr <- ITWT2_PO(wc,L,qmf)
#' @seealso \code{\link{FTWT2_PO}}, \code{\link{MakeONFilter}}.

ITWT2_PO <- function(wc, L, qmf) {
  q <- quadlength(wc)
  n <- q$x
  J <- q$y
  # for (c in 1:n) { col <- wc[, c] wcol <- IWT_PO(col, L, qmf) wc[, c] <- wcol
  # } for (r in 1:n) { row <- wc[r, ] wrow <- IWT_PO(row, L, qmf) wc[r, ] <-
  # wrow } x <- wc
  wc <- apply(wc, 2, FUN = IWT_PO, L = L, qmf = qmf)
  x <- t(apply(wc, 1, FUN = IWT_PO, L = L, qmf = qmf))
  return(x)
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

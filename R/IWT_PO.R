#' Inverse Wavelet Transform (periodized, orthogonal).
#'
#' Suppose \code{wc <- FWT_PO(x,L,qmf)} where \code{qmf} is an orthonormal quad.
#' mirror filter, e.g. one made by\code{\link{MakeONFilter}}.
#' Then x can be reconstructed by \code{x <- IWT_PO(wc,L,qmf)}.
#'
#' @export IWT_PO
#' @param wc 1-d wavelet transform: length(wc) = 2^J.
#' @param L Coarsest scale (2^(-L) = scale of V_0); L << J.
#' @param qmf quadrature mirror filter (orthonormal).
#' @return \code{x} 1-d signal reconstructed from wc.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' L <- 0
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT_PO(x, L, qmf)
#' xr <- IWT_PO(wc,L,qmf)
#' @seealso \code{\link{FWT_PO}}, \code{\link{MakeONFilter}}.

IWT_PO <- function(wc, L, qmf) {
  wcoef <- ShapeAsRow(wc)
  x <- wcoef[1:(2^L)]
  d <- dyadlength(wcoef)
  J <- d$y
  for (j in L:(J - 1)) {
    x <- UpDyadLo(x, qmf) + UpDyadHi(wcoef[dyad(j)], qmf)
  }
  return(x)
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of WaveLab Version 802 Built Sunday, October 3, 1999 8:52:27 AM This
# is Copyrighted Material For Copying permissions see COPYING.m Comments?
# e-mail wavelab@stat.stanford.edu

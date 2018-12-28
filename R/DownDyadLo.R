#' Lo-Pass Downsampling operator (periodized)
#'
#' @export DownDyadLo
#' @param x 1-d signal at fine scale.
#' @param qmf filter.
#' @return \code{d} 1-d signal at coarse scale.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' DownDyadLo(x,qmf)
#' @seealso \code{\link{DownDyadHi}}, \code{\link{UpDyadHi}},
#' \code{\link{UpDyadLo}}, \code{\link{FWT_PO}}, \code{\link{aconv}}.

DownDyadLo <- function(x, qmf) {
  d <- aconv(qmf, x)
  n <- length(d)
  return(d[seq(1, n - 1, 2)])
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

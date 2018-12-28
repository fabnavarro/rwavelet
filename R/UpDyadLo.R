#' Lo-Pass Upsampling operator; periodized
#'
#' @export UpDyadLo
#' @param x 1-d signal at coarser scale.
#' @param qmf filter.
#' @return \code{y} 1-d signal at finer scale.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' UpDyadLo(x,qmf)
#' @seealso \code{\link{DownDyadLo}}, \code{\link{DownDyadHi}},
#' \code{\link{UpDyadHi}}, \code{\link{IWT_PO}}, \code{\link{iconvv}}.

UpDyadLo <- function(x, qmf) {
  y <- iconvv(qmf, UpSampleN(x))
  return(y)
}

# Copyright (c) 1993. Iain M. Johnstone last modified on October 2005

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

#' Hi-Pass Downsampling operator (periodized)
#'
#' @export DownDyadHi
#' @param x 1-d signal at fine scale.
#' @param qmf filter.
#' @return \code{y} 1-d signal at coarse scale.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' DownDyadHi(x, qmf)
#' @seealso \code{\link{DownDyadLo}}, \code{\link{UpDyadHi}},
#' \code{\link{UpDyadLo}}, \code{\link{FWT_PO}}, \code{\link{iconvv}}.

DownDyadHi <- function(x, qmf) {
  d <- iconvv(MirrorFilt(qmf), lshift(x))
  n <- length(d)
  return(d[seq(1, n - 1, 2)])
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

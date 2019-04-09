#' Apply (-1)^t modulation
#'
#' h(t) = (-1)^(t-1)  * x(t),  1 <= t <= length(x)
#'
#' @export MirrorFilt
#' @param x  1-d signal.
#' @return \code{h} 1-d signal with DC frequency content shifted
#'                  to Nyquist frequency
#' @examples
#' x <- MakeSignal('HeaviSine',2^3)
#' h <- MirrorFilt(x)
#' @seealso \code{\link{DownDyadHi}}.

MirrorFilt <- function(x) {
  return(-((-1)^(1:length(x)) * x))
  # TODO check Brani Vidakovic correction
  #return(-rev((-1)^(1:length(x)) * x))
}

# Copyright (c) 1993. Iain M. Johnstone

# Part of WaveLab Version 802 Built Sunday, October 3, 1999 8:52:27 AM This
# is Copyrighted Material For Copying permissions see COPYING.m Comments?
# e-mail wavelab@stat.stanford.edu

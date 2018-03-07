#' Apply (-1)^t modulation
#'
#' h(t) = (-1)^(t-1)  * x(t),  1 <= t <= length(x)
#'
#' @export MirrorFilt
#' @param x  1-d signal.
#' @return \code{h} 1-d signal with DC frequency content shifted.
#'                  to Nyquist frequency
#' @examples
#' x <- MakeSignal('HeaviSine',2^3)
#' h <- MirrorFilt(x)
#' @seealso \code{\link{DownDyadHi}}.

MirrorFilt <- function(x) {
  return(-((-1)^(1:length(x)) * x))
}

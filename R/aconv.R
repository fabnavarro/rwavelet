#' Convolution Tool for Two-Scale Transform.
#'
#' Filtering by periodic convolution of x with the
#' time-reverse of f.
#'
#' @export aconv
#' @param f filter.
#' @param x 1-d signal.
#' @return \code{y} filtered result.
#' @examples
#' qmf <- MakeONFilter('Haar')
#' x <- MakeSignal('HeaviSine',2^3)
#' aconv(qmf,x)
#' @seealso \code{\link{iconvv}}, \code{\link{UpDyadHi}},
#' \code{\link{UpDyadLo}}, \code{\link{DownDyadHi}}, \code{\link{DownDyadLo}}.

aconv <- function(f, x) {
  n <- length(x)
  p <- length(f)
  if (p < n) {
    xpadded <- c(x, x[1:p])
  } else {
    z <- rep(0, p)
    for (i in 1:p) {
      imod <- 1 + ((i - 1)%%n)
      z[i] <- x[imod]
    }
    xpadded <- c(z, x)
  }
  fflip <- rev(f)
  ypadded <- signal::filter(fflip, 1, xpadded)
  return(ypadded[p:(n + p - 1)])
}

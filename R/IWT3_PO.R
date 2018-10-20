#' Inverse 3-d MRA wavelet transform (periodized, orthogonal).
#'
#' If \code{wc} is the result of a forward 3d wavelet transform, with \code{wc <- FWT3_PO(x, L, qmf)}.
#' then \code{x <- IWT3_PO(wc, L, qmf)} reconstructs \code{x} exactly
#' \code{qmf} is a nice qmf, e.g. one made by \code{\link{MakeONFilter}}.
#'
#' 3-D counterpart of Donoho's IWT2_PO, original matlab code by
#' Vicki Yang and Brani Vidakovic.
#'
#' @export IWT3_PO
#' @param wc 3-d wavelet transform (n by n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{x} 3-d signal reconstructed from wc.
#' @examples
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 3
#' x <- array(rnorm(32^3), c(32, 32, 32))
#' wc <- FWT3_PO(x, L, qmf)
#' xr <- IWT3_PO(wc, L, qmf)
#' @seealso \code{\link{FWT3_PO}}, \code{\link{MakeONFilter}}.

IWT3_PO <- function(wc, L, qmf) {
  c <- cubelength(wc)
  n <- c$x
  J <- c$y
  x <- wc
  nc <- 2^(L + 1)
  for (jscal in seq(L, J - 1, 1)) {
    top <- (nc/2 + 1):nc
    bot <- 1:(nc/2)
    all <- 1:nc
    for (iy in 1:nc) {
      for (iz in 1:nc) {
        x[all, iy, iz] <- UpDyadLo(x[bot, iy, iz], qmf) + UpDyadHi(x[top, 
          iy, iz], qmf)
      }
    }
    for (ix in 1:nc) {
      for (iy in 1:nc) {
        x[ix, iy, all] <- UpDyadLo(x[ix, iy, bot], qmf) + UpDyadHi(x[ix, 
          iy, top], qmf)
      }
    }
    for (ix in 1:nc) {
      for (iz in 1:nc) {
        x[ix, all, iz] <- UpDyadLo(x[ix, bot, iz], qmf) + UpDyadHi(x[ix, 
          top, iz], qmf)
      }
    }
    nc <- 2 * nc
  }
  return(x)
}

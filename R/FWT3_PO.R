#' 3-d MRA Forward Wavelet Transform (periodized, orthogonal)
#'
#' A three-dimensional wavelet transform is computed for the array x.
#' \code{qmf} filter may be obtained from \code{\link{MakeONFilter}}.
#' To reconstruct, use \code{\link{IWT3_PO}}.
#'
#' 3-D counterpart of Donoho's FWT2_PO, original matlab code
#' Vicki Yang and Brani Vidakovic.
#'
#' @export FWT3_PO
#' @param x 3-d array (n by n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{wc} 3-d wavelet transform.
#' @examples
#' qmf <- MakeONFilter('Daubechies', 10)
#' L <- 3
#' x <- array(rnorm(32^3), c(32,32,32))
#' wc <- FWT3_PO(x, L, qmf)
#' @seealso \code{\link{IWT3_PO}}, \code{\link{MakeONFilter}}.

FWT3_PO <- function(x, L, qmf) {
  c <- cubelength(x)
  n <- c$x
  J <- c$y
  wc <- x
  nc <- n
  for (jscal in seq(J - 1, L, -1)) {
    top <- (nc/2 + 1):nc
    bot <- 1:(nc/2)
    for (ix in 1:nc) {
      for (iy in 1:nc) {
        row <- wc[ix, iy, 1:nc]
        wc[ix, iy, bot] <- DownDyadLo(row, qmf)
        wc[ix, iy, top] <- DownDyadHi(row, qmf)
      }
    }
    for (ix in 1:nc) {
      for (iz in 1:nc) {
        row <- wc[ix, 1:nc, iz]
        wc[ix, top, iz] <- DownDyadHi(row, qmf)
        wc[ix, bot, iz] <- DownDyadLo(row, qmf)
      }
    }
    for (iy in 1:nc) {
      for (iz in 1:nc) {
        row <- wc[1:nc, iy, iz]
        wc[top, iy, iz] <- DownDyadHi(row, qmf)
        wc[bot, iy, iz] <- DownDyadLo(row, qmf)
      }
    }
    nc <- nc/2
  }
  return(wc)
}

#' 2-d tensor wavelet transform (periodized, orthogonal).
#'
#' A two-dimensional Wavelet Transform is computed for the array x.
#' \code{qmf} filter may be obtained from \code{\link{MakeONFilter}}.
#' To reconstruct, use  \code{\link{IWT2_PO}}.
#'
#' @export FWT2_PO
#' @param x 2-d image (n by n array, n dyadic).
#' @param L coarse level.
#' @param qmf quadrature mirror filter.
#' @return \code{wc} 2-d wavelet transform.
#' @examples
#' \dontrun{
#' wc <- FWT2_PO(x,L,qmf)
#' }
#' @seealso \code{\link{IWT2_PO}}, \code{\link{MakeONFilter}}.

FWT2_PO <- function(x, L, qmf) {
  q <- quadlength(x)
  n <- q$x
  J <- q$y
  wc <- x
  nc <- n
  for (jscal in seq(J - 1, L, by = -1)) {
    top <- (nc/2 + 1):nc
    bot <- 1:(nc/2)
    for (ix in 1:nc) {
      row <- wc[ix, 1:nc]
      wc[ix, bot] <- DownDyadLo(row, qmf)
      wc[ix, top] <- DownDyadHi(row, qmf)
    }
    for (iy in 1:nc) {
      row <- t(wc[1:nc, iy])
      wc[top, iy] <- t(DownDyadHi(row, qmf))
      wc[bot, iy] <- t(DownDyadLo(row, qmf))
    }
    nc <- nc/2
  }
  return(wc)
}

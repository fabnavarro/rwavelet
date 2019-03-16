#' Inversion of the 2d block partition
#'
#' @export invblock_partition2d
#' @param x partition of coefficients by block.
#' @param n scale.
#' @param L block size.
#' @return \code{out} coefficients.
#' @examples
#' n <- 2
#' x <- matrix(rnorm(n^2), ncol=2)
#' j0 <- 0
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT2_PO(x, j0, qmf)
#' L <- 2
#' wcb <- block_partition2d(wc, L)
#' wcib <- invblock_partition2d(wcb, n, L)
#' @seealso \code{\link{block_partition2d}}

invblock_partition2d <- function(x, n, L) {
  nblocks <- floor(n/L)
  buf <- matrix(x, c(L, L * nblocks^2))
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- rbind(out, buf[, (i * L * nblocks + 1):((i + 1) * L * nblocks)])
  }
  return(out)
}

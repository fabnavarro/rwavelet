#' Inversion of the 1d block partition
#'
#' @export invblock_partition
#' @param x partition of coefficients by block.
#' @param n scale.
#' @param L block size.
#' @examples
#' n <- 8
#' x <- MakeSignal('Ramp', n)
#' j0 <- 1
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT_PO(x, j0, qmf)
#' L <- 2
#' wcb <- block_partition(wc, L)
#' wcib <- invblock_partition(wcb, n, L)
#' @seealso \code{\link{block_partition}}, \code{\link{BlockThresh}}.

invblock_partition <- function(x, n, L) {
  nblocks <- floor(n/L)
  buf <- matrix(x, c(L, nblocks))
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- c(out, buf[, (i + 1):(i + 1)])
  }
  return(out)
}

#' Construct 1d block partition
#'
#' This function is used to group the coefficients into blocks (or groups) of size L.
#'
#' @export block_partition
#' @param x (noisy) wc at a given scale.
#' @param L block size.
#' @return \code{out} partition of coefficients by block.
#' @examples
#' x <- MakeSignal('Ramp', 8)
#' j0 <- 0
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT_PO(x, j0, qmf)
#' L <- 2
#' wcb <- block_partition(wc, L)
#' @seealso \code{\link{invblock_partition}}, \code{\link{BlockThresh}}.

block_partition <- function(x, L) {
  n <- length(x)
  nblocks <- floor(n/L)
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- c(out, x[(i * L + 1):((i + 1) * L)])
  }
  out <- matrix(as.vector(out), c(L, nblocks))
  return(out)
}

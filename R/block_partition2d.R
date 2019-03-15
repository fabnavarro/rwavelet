#' Construct 2d block partition
#'
#' Group the coefficients into blocks (or groups) of size L.
#'
#' @export block_partition2d
#' @param x (noisy) wc at a given scale.
#' @param L block size.
#' @return \code{out} partition of coefficients by block.
#' @examples
#' x <- matrix(rnorm(2^2), ncol=2)
#' j0 <- 0
#' qmf <- MakeONFilter('Haar')
#' wc <- FWT2_PO(x, j0, qmf)
#' L <- 2
#' wcb <- block_partition2d(wc, L)
#' @seealso \code{\link{invblock_partition2d}}

block_partition2d <- function(x, L) {
  n <- dim(x)[1]
  nblocks <- floor(n/L)
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- c(out, x[(i * L + 1):((i + 1) * L), ])
  }
  out <- matrix(as.vector(out), c(L^2, nblocks^2))
  return(out)
}

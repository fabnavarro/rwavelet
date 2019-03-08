#' Construct 2d block partition
#' @export block_partition2d
#' @param x noisy wc at a given scale.
#' @param L block size.
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

#' Inversion of the 2d block partition
#' @export invblock_partition2d
#' @param x noisy wc at a given scale.
#' @param n scale.
#' @param L block size.
invblock_partition2d <- function(x, n, L) {
  nblocks <- floor(n/L)
  buf <- matrix(x, c(L, L * nblocks^2))
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- rbind(out, buf[, (i * L * nblocks + 1):((i + 1) * L * nblocks)])
  }
  return(out)
}

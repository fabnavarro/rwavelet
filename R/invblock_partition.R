#' Inversion of the 1d block partition
#' @export invblock_partition
#' @param x noisy wc at a given scale.
#' @param n scale.
#' @param L block size.
invblock_partition <- function(x, n, L) {
  nblocks <- floor(n/L)
  buf <- matrix(x, c(L, nblocks))
  out <- NULL
  for (i in 0:(nblocks - 1)) {
    out <- c(out, buf[, (i + 1):(i + 1)])
  }
  return(out)
}

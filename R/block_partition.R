#' Construct 1d block partition
#' @export block_partition
#' @param x noisy wc at a given scale.
#' @param L block size.
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

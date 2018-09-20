#' Packet table indexing.
#'
#' @export packet
#' @param d depth of splitting in packet decomposition.
#' @param b block index among 2^d possibilities at depth d.
#' @param n length of signal
#' @return \code{p} linear indices of all coeff's in that block.
#' @examples
#' packet(1, 1, 8)

packet <- function(d, b, n) {
  npack <- 2^d
  p <- ((b * (n/npack) + 1):((b + 1) * n/npack))
  return(p)
}

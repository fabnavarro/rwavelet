#' Index entire j-th dyad of 1-d wavelet xform
#'
#' @export dyad
#' @param j integer.
#' @return \code{ix} list of all indices of wavelet coeffts at j-th level.
#' @examples
#' dyad(0)

dyad <- function(j) {
  i <- (2^(j) + 1):(2^(j + 1))
  return(i)
}

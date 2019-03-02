#' Replicate and tile an array.
#'
#' @export repmat
#' @param a input array (scalar, vector, matrix)
#' @param n number of time to repeat input array in row and column dimensions
#' @param m repetition factor
repmat <- function(a, n, m) {
  a <- matrix(a, nrow = 1, ncol = length(a))
  matrix(1, n, m) %x% a
}

#' Replicate and tile an array
#'
#' Repeat copies of array (equivalent of the repmat matlab function).
#'
#' @export repmat
#' @param a input array (scalar, vector, matrix).
#' @param n number of time to repeat input array in row and column dimensions.
#' @param m repetition factor.
#' @examples
#' repmat(10,3,2)
repmat <- function(a, n, m) {
  a <- matrix(a, nrow = 1, ncol = length(a))
  matrix(1, n, m) %x% a
}

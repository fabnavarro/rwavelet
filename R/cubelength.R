#' Find length and dyadic length of square array.
#'
#' 3-D counterpart of Donoho's quadlength utilized by the 2D pair.
#' Original matlab code Vicki Yang and Brani Vidakovic.
#'
#' @export cubelength
#' @param x  3-d array; dim(n,n,n), n = 2^J (hopefully).
#' @return \code{n} length(x).
#' @return \code{J} least power of two greater than n.
#' @examples
#' cubelength(array(1:3, c(2,2,2)))

cubelength <- function(x) {
  s <- dim(x)
  n <- s[1]
  if (s[2] != s[1] | s[2] != s[3]) {
    warning("nr!=nc or nr!=np")
  }
  k <- 1
  J <- 0
  while (k < n) {
    k <- 2 * k
    J <- J + 1
  }
  if (k != n) {
    warning("n!=2^J: n should be a dyadic number")
  }
  return(list(x = n, y = J))
}

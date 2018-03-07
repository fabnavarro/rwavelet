#' Find length and dyadic length of square matrix.
#'
#' h(t) = (-1)^(t-1)  * x(t),  1 <= t <= length(x)
#'
#' @export quadlength
#' @param x  2-d image; size(n,n), n = 2^J (hopefully).
#' @return \code{n} length(x).
#' @return \code{J} least power of two greater than n.
#' @examples
#' quadlength(matrix(1:16,ncol=4))

quadlength <- function(x) {
  s <- dim(x)
  n <- s[1]
  if (s[2] != s[1]) {
    print("Warning in quadlength : nr != nc")
  }
  k <- 1
  J <- 0
  while (k < n) {
    k <- 2 * k
    J <- J + 1
  }
  if (k != n) {
    print("Warning in quadlength : n != 2^J")
  }
  return(list(x = n, y = J))
}

#' Find length and dyadic length of square matrix.
#'
#' h(t) = (-1)^(t-1)  * x(t),  1 <= t <= length(x)
#'
#' @export quadlength
#' @param x  2-d image; dim(n,n), n = 2^J (hopefully).
#' @return \code{n} length(x).
#' @return \code{J} least power of two greater than n.
#' @examples
#' quadlength(matrix(1:16,ncol=4))

quadlength <- function(x) {
  s <- dim(x)
  n <- s[1]
  if (s[2] != s[1]) {
    warning("nr != nc")
  }
  k <- 1
  J <- 0
  while (k < n) {
    k <- 2 * k
    J <- J + 1
  }
  if (k != n) {
    warning("n != 2^J: n should be a dyadic number")
  }
  return(list(x = n, y = J))
}

# Copyright (c) 1993. David L. Donoho

# Part of Wavelab Version 850 Built Tue Jan 3 13:20:40 EST 2006 This is
# Copyrighted Material For Copying permissions see COPYING.m Comments? e-mail
# wavelab@stat.stanford.edu

repmat <- function(a, n, m){
  a <- matrix(a,
              nrow = 1,
              ncol = length(a))
  matrix(1, n, m) %x% a
}

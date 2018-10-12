cubelength <- function(x) {
  s <- dim(x)
  n <- s[1]
  if (s[2] != s[1] | s[2] != s[3]) {
    print("Warning nr!=nc or nr!=np")
  }
  k <- 1
  J <- 0
  while (k < n) {
    k <- 2 * k
    J <- J + 1
  }
  if (k != n) {
    print("Warning n!=2^J")
  }
  return(list(x = n, y = J))
}

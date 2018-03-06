# to be modified
normvec <- function(x) {
  svd1 <- svd(x)
  return(max(svd1$d))
}

#' Circular Shifting of a matrix/image
#'
#' Pixels that get shifted off one side of the image
#' are put back on the other side.
#'
#' @export CircularShift
#' @param matrix 2-d signal (matrix).
#' @param colshift column shift index (integer).
#' @param rowshift row shift index (integer).
#' @return \code{result} 2-d shifted signal.
#' @examples
#' A <- matrix(1:4, ncol=2, byrow=TRUE)
#' CircularShift(A, 0, -1)
#' @seealso \code{\link{FWT2_TI}}, \code{\link{IWT2_TI}}.

CircularShift <- function(matrix, colshift = 0, rowshift = 0) {
  lastrow <- nrow(matrix)
  lastcol <- ncol(matrix)
  result <- matrix
  # Shift the cols
  if (colshift > 0) {
    result <- cbind(result[, (lastcol - colshift + 1):lastcol], result[,
      seq_len(lastcol - colshift)])
  } else {
    colshift <- -colshift
    result <- cbind(result[, seq(colshift + 1, lastcol, length = max(0, lastcol -
      colshift - 1 + 1))], result[, seq(1, colshift, length = max(0, colshift -
      1 + 1))])
  }
  # Shift the rows
  if (rowshift > 0) {
    result <- rbind(result[(lastrow - rowshift + 1):lastrow, ], result[seq_len(lastrow -
      rowshift), ])
  } else {
    rowshift <- -rowshift
    result <- rbind(result[seq(rowshift + 1, lastrow, length = max(0, lastrow -
      rowshift - 1 + 1)), ], result[seq(1, rowshift, length = max(0, rowshift -
      1 + 1)), ])
  }
  return(result)
}

# Contributor: Matt Considine

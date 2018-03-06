#' Make signal a row vector
#'
#' @export ShapeAsRow
#' @param sig a row or column vector.
#' @return \code{row} a row vector.
#' @examples
#' \dontrun{
#' row <- ShapeAsRow(sig)
#' }

ShapeAsRow <- function(sig) {
  row <- as.vector(t(sig))
  return(row)
}

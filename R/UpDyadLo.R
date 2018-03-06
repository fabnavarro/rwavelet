#' Lo-Pass Upsampling operator; periodized
#'
#' @export UpDyadLo
#' @param x 1-d signal at coarser scale.
#' @param qmf filter.
#' @return \code{y} 1-d signal at finer scale.
#' @examples
#' \dontrun{
#' UpDyadLo(x,qmf)
#' }
#' @seealso \code{\link{DownDyadLo}}, \code{\link{DownDyadHi}},
#' \code{\link{UpDyadHi}}, \code{\link{IWT_PO}}, \code{\link{iconvv}}.

UpDyadLo <- function(x, qmf) {
  y <- iconvv(qmf, UpSampleN(x))
  return(y)
}

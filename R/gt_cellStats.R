#' @include gt_RasterLayer.R
NULL

#' Statistics across cells
#'
#' Compute statistics using cell data in a \code{\link{gt_RasterLayer}} object.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param stat \code{character} name of the statistic to calculate. The following
#' statistics can be used: 'sum', 'mean', 'min', 'max', and 'sd'.
#' @return \code{numeric} object.
#' @details This function is similar to the \code{\link[raster]{cellStats}} function.
#' @export
gt_cellStats <- function(x, stat=c('sum', 'mean', 'min', 'max', 'sd')) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(stat, 'character'))
  stat <- match.arg(stat)
  x$cellStats(stat)
}

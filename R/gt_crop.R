#' @include gt_RasterLayer.R
NULL

#' Crop geotrellis raster data
#'
#' 
#' Crop returns a geographic subset of an \code{\link{gt_RasterLayer}} object as specified 
#' by another \code{\link{gt_RasterLayer}} object.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param y \code{\link{gt_RasterLayer}} object.
#' @return \code{\link{gt_RasterLayer}} object.
#' @details This function is similar to \code{\link[raster]{crop}}, except that \code{y}
#' must be a \code{\link{gt_RasterLayer}} object and not just any \code{\link[sp]{Spatial}} object.
#' @export
gt_zonal <- function(x, y) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(y, 'gt_RasterLayer'),
    gt_compareRaster(x, y, stopiffalse=FALSE))
  x$crop(x, y)
}


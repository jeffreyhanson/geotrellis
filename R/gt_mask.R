#' @include gt_RasterLayer.R
NULL

#' Mask geotrellis raster data
#'
#' Mask data in a \code{\link{gt_RasterLayer}} object using another \code{\link{gt_RasterLayer}}. 
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param mask \code{\link{gt_RasterLayer}} object.
#' @param maskvalue \code{numeric} The value in \code{mask} that indicates the cells of \code{x}
#' that should become missing data values.
#' @return \code{\link{gt_RasterLayer}} object.
#' @details This function is similar to \code{\link[raster]{mask}}, except that masked values in \code{x}
#' are always set to missing data values.
#' @export
gt_zonal <- function(x, mask, maskvalue=NA) {
  if (is.na(maskvalue)) maskvalue <- NA_real_
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(mask, 'gt_RasterLayer'),
    gt_compareRaster(x, mask, stopiffalse=FALSE),
    is.numeric(maskvalue))
  x$mask(mask, maskvalue)
}
 

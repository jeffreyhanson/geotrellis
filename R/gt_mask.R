#' @include gt_RasterLayer.R
NULL

#' Mask geotrellis raster data
#'
#' Mask data in a \code{\link{gt_RasterLayer}} object using another \code{\link{gt_RasterLayer}}. 
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param mask \code{\link{gt_RasterLayer}} object.
#' @param maskvalue \code{numeric} The value in \code{mask} that indicates the cells of \code{x}
#' that should become missing data values. Defaults to \code{NA}.
#' @param updatevalue \code{numeric} The value that masked out values in \code{x} should becomes. 
#' Defaults to \code{NA}.
#' @param ... not used.
#' @return \code{\link{gt_RasterLayer}} object.
#' @details This function is similar to \code{\link[raster]{mask}}, except that masked values in \code{x}
#' are always set to missing data values.
#' @export
setGeneric('gt_mask', function(x, y, ...) {standardGeneric('gt_mask')})

#' @export
setMethod('gt_mask', signature(x='gt_RasterLayer', y='gt_RasterLayer'),
  function(x, y, maskvalue=NA, updatevalue=NA, ...) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(y, 'gt_RasterLayer'),
      gt_compareRaster(x, y),
      assertthat::is.number(maskvalue) || is.na(maskvalue),
      assertthat::is.number(updatevalue) || is.na(updatevalue))
    x$mask(y, maskvalue, updatevalue)
})

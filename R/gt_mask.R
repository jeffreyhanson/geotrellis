#' @include gt_RasterLayer.R
NULL

#' Mask geotrellis raster data
#'
#' Mask data in a \code{\link{gt_RasterLayer}} object using another \code{\link{gt_RasterLayer}}. 
#' @usage gt_mask(x, mask, maskvalue, updatevalue)
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
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(c(1:6), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' mask <- gt_raster(raster::raster(matrix(c(NA, 1:4, NA), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_mask(g, mask)
#' }
#' @export
setGeneric('gt_mask', function(x, mask, ...) {standardGeneric('gt_mask')})

#' @export
setMethod('gt_mask', signature(x='gt_RasterLayer', mask='gt_RasterLayer'),
  function(x, mask, maskvalue=NA, updatevalue=NA) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(mask, 'gt_RasterLayer'),
      gt_compareRaster(x, mask),
      assertthat::is.number(maskvalue) || is.na(maskvalue),
      assertthat::is.number(updatevalue) || is.na(updatevalue))
    x$mask(mask, maskvalue, updatevalue)
})

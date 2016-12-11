#' @include gt_RasterLayer.R
NULL

#' Crop geotrellis raster data
#'
#' 
#' Crop returns a geographic subset of an \code{\link{gt_RasterLayer}} object as specified 
#' by another \code{\link{gt_RasterLayer}} object.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param y \code{\link[raster]{Extent}} or \code{\link{gt_RasterLayer}} object.
#' @return \code{\link{gt_RasterLayer}} object.
#' @details This function is similar to \code{\link[raster]{crop}}, except that \code{y}
#' must be a \code{\link{gt_RasterLayer}} object and not just any \code{\link[sp]{Spatial}} object.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(c(1:6), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' ext <- extent(c(0, 3, 2, 6))
#' result <- gt_crop(g, ext)
#' }
#' @export
setGeneric('gt_crop', function(x, y) {standardGeneric('gt_crop')})

#' @rdname gt_crop
#' @export
setMethod('gt_crop', signature(x='gt_RasterLayer', y='Extent'),
  function(x, y) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(y, 'Extent'),
      inherits(raster::intersect(raster::extent(x), y), 'Extent'))
  x$crop(y)
})

#' @rdname gt_crop
#' @export
setMethod('gt_crop', signature(x='gt_RasterLayer', y='gt_RasterLayer'),
  function(x, y) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(y, 'gt_RasterLayer'),
      raster::compareCRS(crs(x), crs(y)))
  gt_crop(x, extent(y))
})

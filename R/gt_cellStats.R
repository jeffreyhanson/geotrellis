#' @include gt_RasterLayer.R
NULL

#' Statistics across cells
#'
#' Compute statistics using cell data in a \code{\link{gt_RasterLayer}} object.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @return \code{numeric} object.
#' 
#' @details This function calculates the mean, median, mode, 
#' standard deviation, minimum, and maximum of the values in a raster. This function
#' is similar to \code{\link[raster]{cellStats}}.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(c(1:6), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' gt_cellStats(g)
#' }
#' @export
setGeneric('gt_cellStats', function(x) {standardGeneric('gt_cellStats')})

#' @export
setMethod('gt_cellStats', signature(x='gt_RasterLayer'),
  function(x) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'))
    x$cellStats()
})

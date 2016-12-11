#' Zonal statistics
#'
#' Calculate statistics from a \code{\link{gt_RasterLayer}} object using
#' zones specified in a second \code{\link{gt_RasterLayer}}. 
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param z \code{\link{gt_RasterLayer}} object with codes representing zones.
#' @return \code{\link[base]{matrix}} object with a value for each zone (unique values in)
#' \code{z}.
#' #' @details This function calculates the mean, median, mode, and
#' standard deviation in each zone. It is similar to \code{\link[raster]{zonal}}.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' z <- gt_raster(raster::raster(matrix(sample(1:3, 9, replace=TRUE), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_zonal(g, z)
#' }
#' @export
setGeneric('gt_zonal', function(x, z) {standardGeneric('gt_zonal')})

#' @rdname gt_zonal
#' @export
setMethod('gt_zonal', signature(x='gt_RasterLayer', z='gt_RasterLayer'),
  function(x, z) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(z, 'gt_RasterLayer'),
      gt_compareRaster(x, z))
    x$zonal(z)
})

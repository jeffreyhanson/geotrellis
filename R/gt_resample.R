#' @include gt_RasterLayer.R
NULL

#' Resample geotrellis raster data
#'
#' This function resamples  a geotrellis raster data \code{\link{gt_RasterLayer}}
#' object to match the extent, resolution, and dimensionality of another
#' \code{\link[raster]{projectRaster}}.
#' @usage gt_resample(x, y, method)
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param y \code{\link{gt_RasterLayer}} object with parameters that \code{x}
#' should be resampled to.
#' @param method \code{character} method used to compute values for the new \code{\link{gt_RasterLayer}}.
#' Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' 
#' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @param ... not used.
#' @return \code{\link{gt_RasterLayer}} object.
#' @details This function is similar to \code{\link[raster]{resample}}.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' template <- gt_raster(raster::raster(matrix(runif(4), ncol=2),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_resample(g, template, method='bilinear')
#' }
#' @export
setGeneric('gt_resample', function(x, y, ...) {standardGeneric('gt_resample')})

#' @export
setMethod('gt_resample', signature(x='gt_RasterLayer', y='gt_RasterLayer'),
  function(x, y, method=c('bilinear', 'ngb')) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      inherits(y, 'gt_RasterLayer'),
      raster::compareCRS(crs(x), crs(y)),
      inherits(raster::intersect(raster::extent(x), raster::extent(y)), 'Extent'))
    method <- match.arg(method)
    x$resample(y, method)
})

#' Resample geotrellis raster data
#'
#' This function resamples  a geotrellis raster data \code{\link{gt_RasterLayer}}
#' object to match the extent, resolution, and dimensionality of another
#' \code{\link[raster]{projectRaster}}.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param y \code{\link{gt_RasterLayer}} object with parameters that \code{x}
#' should be resampled to.
#' @param method \code{character} method used to compute values for the new \code{\link{gt_RasterLayer}}.
#' Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' 
#' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @return \code{\code{\link{gt_RasterLayer}} object}.
#' @details This function is similar to \code{\link[raster]{resample}}.
#' @export
gt_resample <- function(x, y, method=c('bilinear', 'ngb')) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(y, 'gt_RasterLayer'),
    raster::compareCRS(crs(x), crs(y)))
  method <- match.arg(method)
  x$resample(y, method)
}

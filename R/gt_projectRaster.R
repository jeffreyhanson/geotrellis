#' @include gt_RasterLayer.R
NULL

#' Reproject geotrellis raster data
#'
#' This function reprojects a geotrellis raster data \code{\link{gt_RasterLayer}}
#' object to a new coordinate system. This function is similar to \code{\link[raster]{projectRaster}}.
#' @param from \code{\link{gt_RasterLayer}} object.
#' @param crs \code{\link[sp]{crs}} new coordinate system to project data to.
#' @param method \code{character} method used to compute values for the new \code{\link{gt_RasterLayer}}.
#' Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' 
#' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @param ... additional arguments.
#' @details Note that--unlike \code{\link[raster]{projectRaster}}--this function does not have
#' a \code{to} argument. Therefore users cannot specify a template projected raster to reproject
#' a project \code{\link{gt_RasterLayer}} and ensure that the returned raster has a specific 
#' extent and dimensionality.
#' @return \code{\code{\link{gt_RasterLayer}} object}.
#' @export
setGeneric('gt_projectRaster', function(from, to, ...) {standardGeneric('gt_projectRaster')})

#' @export
setMethod('gt_projectRaster', signature(from='gt_RasterLayer', to='CRS'),
  function(from, to, res, method=c('bilinear', 'ngb')) {
    assertthat::assert_that(
      inherits(from, 'gt_RasterLayer'),
      inherits(to, 'CRS'),
      (length(res) == 1 || length(res) == 2),
      is.numeric(res),
      all(res > 0),
      !is.na(to@projargs))
    method <- match.arg(method)
    if (length(res)==1) res <- c(res, res)
    from$project.to.crs(to, res, method)
  })

#' @export
setMethod('gt_projectRaster', signature(from='gt_RasterLayer', to='gt_RasterLayer'),
  function(from, to, method=c('bilinear', 'ngb')) {
    assertthat::assert_that(
      inherits(from, 'gt_RasterLayer'),
      inherits(to, 'gt_RasterLayer'),
      !is.na(to$crs@projargs))
    method <- match.arg(method)
    from$project.to.raster(to, method)
  })

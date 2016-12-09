#' @include gt_RasterLayer.R
NULL

#' Project geotrellis raster to a new coordinate system
#'
#' This function reprojects a geotrellis raster data \code{\link{gt_RasterLayer}}
#' object to a new coordinate system. This function is similar to \code{\link[raster]{projectRaster}}.
#' @usage gt_projectRaster(from, to, res, method)
#' @usage gt_projectRaster(from, to, method)
#' @param from \code{\link{gt_RasterLayer}} object.
#' @param to A \code{\link{gt_RasterLayer}} or \code{\link[sp]{CRS}} object with to reproject 
#' \code{from} to.
#' @param res \code{numeric} resolution to reproject the data to. This is required when \code{to}
#' is a \code{\link[sp]{CRS}} object.
#' @param method \code{character} method used to compute values for the new \code{\link{gt_RasterLayer}}.
#' Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' 
#' (bilinear interpolation; the default value), which is appropriate for continuous variables.
#' @param ... additional arguments.
#' @details Note that--unlike \code{\link[raster]{projectRaster}}--this function does not have
#' a \code{to} argument. Therefore users cannot specify a template projected raster to reproject
#' a project \code{\link{gt_RasterLayer}} and ensure that the returned raster has a specific 
#' extent and dimensionality.
#' @return \code{\link{gt_RasterLayer}} object.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_projectRaster(g, sp::CRS('+init=epsg:3395'), res=50000, method='ngb')
#' }
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
    from$project_to_crs(to, res, method)
  })

#' @export
setMethod('gt_projectRaster', signature(from='gt_RasterLayer', to='gt_RasterLayer'),
  function(from, to, method=c('bilinear', 'ngb')) {
    assertthat::assert_that(
      inherits(from, 'gt_RasterLayer'),
      inherits(to, 'gt_RasterLayer'),
      !is.na(to$crs@projargs))
    method <- match.arg(method)
    from$project_to_raster(to, method)
  })

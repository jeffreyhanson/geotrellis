#' @include gt_RasterLayer.R
NULL

#' gt_RasterLayer methods
#'
#' These are convience wrappers for \code{\link{gt_RasterLayer}} objects. 
#' 
#' @param x \code{gt_RasterLayer} object.
#' @name gt_RasterLayer-methods
#' @seealso \code{\link{gt_RasterLayer}}
NULL

#' @importFrom methods setOldClass
#' @importFrom methods setMethod
#' @export
setOldClass('gt_RasterLayer')

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='crs', signature='gt_RasterLayer', definition=function(x) {x$crs()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='ncell', signature='gt_RasterLayer', definition=function(x) {x$ncell()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='extent', signature='gt_RasterLayer', definition=function(x) {x$extent()})

#' @rdname gt_RasterLayer-methods
#' @export
as.raster.gt_RasterLayer <- function(x) {
  # load raster directly into memory
  if (x$ncell() <= raster::rasterOptions()$maxmemory) {
    r <- raster(x$values(), extent=x$extent(), crs=x$crs()) 
  } else { 
    # load raster by saving it to disk
    path <- tempfile(fileext='.tif')
    gt_writeRaster(x, path)
    r <- raster::raster(path)
  }
  r
}
#' @include gt_RasterLayer.R
NULL

#' gt_RasterLayer methods
#'
#' These are convience wrappers for \code{\link{gt_RasterLayer}} objects. 
#' 
#' @param x \code{gt_RasterLayer} object.
#' @name gt_RasterLayer-methods
#' @seealso \code{\link{gt_RasterLayer}}
NULL

#' @importFrom methods setOldClass
#' @importFrom methods setMethod
#' @export
setOldClass('gt_RasterLayer')

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='crs', signature='gt_RasterLayer', definition=function(x) {x$crs()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='ncell', signature='gt_RasterLayer', definition=function(x) {x$ncell()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='values', signature='gt_RasterLayer', definition=function(x) {x$values()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='as.raster', signature='gt_RasterLayer', definition=function(x) {
  # load raster by saving it to disk
  path <- tempfile(fileext='.tif')
  gt_writeRaster(x, path)
  r <- raster::raster(path)
})

#' @include gt_RasterLayer.R
NULL

#' gt_RasterLayer methods
#'
#' These are convience wrappers for \code{\link{gt_RasterLayer}} objects. 
#' 
#' @param x \code{gt_RasterLayer} object.
#' @name gt_RasterLayer-methods
#' @seealso \code{\link{gt_RasterLayer}}
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' ncell(g) # number of cells
#' ncol(g) # number of columns
#' nrow(g) # number of rows
#' nrow(g) # number of rows
#' crs(g) # coordinate reference system
#' res(g) # cell resolution
#' extent(g) # spatial extent
#' as.matrix(g) # coerce data to matrix object
#' as.raster(g) # coerce data to matrix object
#' }
NULL

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='crs', signature(x='gt_RasterLayer'), function(x) {x$crs})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='ncell', signature(x='gt_RasterLayer'), function(x) {x$ncell})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='values', signature(x='gt_RasterLayer'), function(x) {x$values()})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='res', signature(x='gt_RasterLayer'), function(x) {x$res})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='extent', signature(x='gt_RasterLayer'), function(x) {x$extent})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='ncol', signature(x='gt_RasterLayer'), function(x) {x$ncol})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='nrow', signature(x='gt_RasterLayer'), function(x) {x$nrow})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='as.matrix', signature(x='gt_RasterLayer'), function(x) {
  matrix(values(x), ncol=ncol(x), nrow=nrow(x), byrow=TRUE)
})

#' @rdname gt_RasterLayer-methods
#' @export
setMethod(f='as.raster', signature(x='gt_RasterLayer'), function(x) {
  # load raster directly into memory
  if (ncell(x) <= .rasterOptions()$maxmemory) {
    r <- raster::raster(as.matrix(x), crs=crs(x), xmn=extent(x)@xmin ,
                        xmx=extent(x)@xmax, ymn=extent(x)@ymin,
                        ymx=extent(x)@ymax)
  } else { 
    # load raster by saving it to disk
    path <- tempfile(fileext='.tif')
    gt_writeRaster(x, path)
    r <- raster::raster(path)
  }
  r
})

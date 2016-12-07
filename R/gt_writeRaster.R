#' Write geotrellis raster to file
#'
#' Save a \code{\link{gt_RasterLayer}} object to disk in a GeoTIFF format.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param x \code{character} file path.
#' @return \code{invisible()}. This function is used for its side-effect
#' of producing files on disk. 
#' @examples
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' gt_writeRaster(g, tempfile(fileext='.tif'))
#' @export
gt_writeRaster <- function(x, path) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(path, 'character'),
    file.exists(dirname(path)),
    !file.exists(path),
    assertthat::has_extension(path, 'tif'))
  x$write_data(path)
  invisible()
}
 

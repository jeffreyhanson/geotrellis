#' Write geotrellis raster to file
#'
#' Save a \code{\link{\gt_RasterLayer}} object to disk in a GeoTIFF format.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param x \code{character} file path.
#' @return \code{invisible()}. This function is used for its side-effect
#' of producing files on disk. 
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
 

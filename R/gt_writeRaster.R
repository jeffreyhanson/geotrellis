#' Write geotrellis raster to file
#'
#' Save a \code{\link{gt_RasterLayer}} object to disk in a GeoTIFF format.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param path \code{character} file path.
#' @param overwrite \code{character} if the file path specifed in \code{path}
#' already exists should it be overwritten? Defaults to \code{FALSE}.
#' @return \code{invisible()}. This function is used for its side-effect
#' of producing files on disk. 
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' gt_writeRaster(g, tempfile(fileext='.tif'))
#' }
#' @export
gt_writeRaster <- function(x, path, overwrite=FALSE) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(path, 'character'),
    file.exists(dirname(path)),
    (!file.exists(path) || overwrite),
    assertthat::has_extension(path, 'tif'))
  x$write_data(path)
  invisible()
}
 

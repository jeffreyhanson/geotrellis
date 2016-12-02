#' Create geotrellis raster object
#'
#' This function creates a geotrellis raster object (\code{\link{gt_RasterLayer}}).
#' @param x input object.
#' @param ... additional arguments passed to \code{\link[raster]{raster}}.
#' @details This function essentially loads \code{x} into geotrellis as
#' as raster data using the following methods:
#' \itemize{
#'
#' \item{\code{\link[base]{character}} object}{\code{x} is treated as a file path
#' for raster data loaded directly into the Scala interpreter.}
#'
#' \item{\code{\link[raster][Raster]} object}{\code{x} is saved to disk
#' and loaded into the Scala interpreter.}
#'
#' \item{otherwise}{an attempt is made to coerce \code{x} to a \code{\link[raster][Raster]}
#' object and then it is saved to disk and loaded into the Scala interpreter.} 
#'
#' }
#' @return \code{\link{gt_RasterLayer}} object.
#' @seealso \code{\link[raster]{raster}}.
#' @export
#' @examples
#' # create gt_raster object using a matrix
#' x <- gt_raster(matrix(1:4, ncol=2))
#' # create gt_raster object using a raster
#' x <- gt_raster(raster(matrix(1:4, ncol=2)))
#' # create gt_raster object using a file path
#' x <- gt_raster(system.file("external/test.grd", package="raster"))
gt_raster <- function(x, ...) UseMethod('gt_raster')

#' @rdname gt_raster
#' @export
gt_raster.character <- function(x, ...) {
  assertthat::assert_that(
    inherits(x, 'character'),
    assertthat::is_readable(x),
    assertthat::has_extension(x, 'tif'))
  r <- gt_RasterLayer$new()
  r$read_data(x)
}

#' @rdname gt_raster
#' @export
gt_raster.RasterLayer <- function(x, ...) {
  path <- tempfile(fileext='.tif', tempdir=file.path(tempdir(), 'geotrellis'))
  raster::writeRaster(x, path)
  gt_raster.character(path)
}

#' @rdname gt_raster
#' @export
gt_raster.default <- function(x, ...) {
  x <- raster::raster(x, ...)
  assertthat::assert_that(inherits(x, 'RasterLayer'))
  gt_raster.RasterLayer(x)
}

#' @export
as.RasterLayer.gt_RasterLayer <- function(x) {
  path <- tempfile(fileext='.tif')
  gt_writeRaster(x, path)
  raster::raster(path)
}

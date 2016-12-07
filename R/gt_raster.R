#' @include gt_RasterLayer.R
NULL

#' Create geotrellis raster object
#'
#' This function creates a geotrellis raster object (\code{\link{gt_RasterLayer}}).
#' @param x input object.
#' @param ... additional arguments passed to \code{\link[raster]{raster}} if \code{x}
#' is not \code{character} or \code{raster}.
#' @details This function essentially loads \code{x} into geotrellis as
#' as raster data using the following methods:
#' \itemize{
#'
#' \item{\code{\link[base]{character}} object}{\code{x} is treated as a file path
#' for raster data loaded directly into the Scala interpreter.}
#'
#' \item{\code{\link[raster]{RasterLayer-class}} object}{\code{x} is saved to disk
#' and loaded into the Scala interpreter.}
#'
#' \item{otherwise}{an attempt is made to coerce \code{x} to a \code{\link[raster]{RasterLayer-class}}
#' object and then it is saved to disk and loaded into the Scala interpreter.} 
#'
#' }
#' @return \code{\link{gt_RasterLayer}} object.
#' @seealso \code{\link[raster]{raster}}.
#' @examples
#' \dontrun{
#' # create gt_raster object using a raster
#' g1 <- gt_raster(raster(matrix(1:4, ncol=2)))
#' # create gt_raster object using a geotiff path
#' g2 <- gt_raster(system.file('extdata', 'test.tif', package='geotrellis'))
#' # create gt_raster object using default method
#' g3 <- gt_raster(matrix(1:4, ncol=2))
#' }
#' @export
setGeneric('gt_raster', function(x, ...) {standardGeneric('gt_raster')})

#' @export
setMethod('gt_raster', signature(x='character'),
  function(x) {
    assertthat::assert_that(
      inherits(x, 'character'),
      assertthat::is.readable(x),
      assertthat::has_extension(x, 'tif'))
    r <- gt_RasterLayer$new()
    r$read_data(x)
    r$read_metadata()
    r
  })

#' @export
setMethod('gt_raster', signature(x='RasterLayer'),
  function(x) {
    path <- tempfile(fileext='.tif')
    raster::writeRaster(x, path)
    gt_raster(path)
  })

#' @export
setMethod('gt_raster', signature(x='ANY'),
  function(x, ...) {
    x <- raster::raster(x, ...)
    assertthat::assert_that(inherits(x, 'RasterLayer'))
    gt_raster(x)
  })


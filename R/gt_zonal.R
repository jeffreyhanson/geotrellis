#' Zonal statistics
#'
#' Calculate statistics from a \code{\link{gt_RasterLayer}} object using
#' zones specified in a second \code{\link{gt_RasterLayer}}. 
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param z \code{\link{gt_RasterLayer}} object with codes representing zones.
#' @param fun \code{function} to be applied to summarize the values by zone.
#  Current options are 'mean', 'sd', 'min', 'max', 'sum'.
#' @return \code{\link[base]{matrix}} object with a value for each zone (unique values in)
#' \code{z}.
#' @details This function is similar to \code{\link[raster]{zonal}}. Note that unlike
#' \code{\link[raster]{zonal}}, the argument to \code{fun} cannot be a code{function}
#' definition.
#' @export
gt_zonal <- function(x, z, fun=c('mean', 'sd', 'min', 'max', 'sum')) {
  assertthat::assert_that(
    inherits(x, 'gt_RasterLayer'),
    inherits(z, 'gt_RasterLayer'),
    gt_compareRaster(x, z, stopiffalse=FALSE))
  fun <- match.arg(fun)
  x$zonal(z, fun)
}

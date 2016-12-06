#' @include gt_RasterLayer.R
NULL

#' Compare geotrellis raster objects
#'
#' Evaluate whether two \code{\link{gt_RasterLayer}} objects have the same
#' extent, number of rows and columns, projection, and resolution
#' (or a subset of these comparisons).
#' @param x \code{\link{gt_RasterLayer}}.
#' @param y \code{\link{gt_RasterLayer}}.
#' @param extent \code{logical} should the objects' extent be compared?
#' Defaults to \code{TRUE}.
#' @param rowcol \code{logical} should the objects' dimensionality be compared?
#' Defaults to \code{TRUE}.
#' @param res \code{logical} should the objects' resolution be compared? 
#' Defaults to \code{TRUE}.
#' @param tolerance \code{numeric} This sets the difference between values
#' that is permissible for them to be considered equal. Defaults to 0.1.
#' @param stopiffalse \code{logical} should an error be thrown if one or
#' more conditions are not met?
#' @param showwarning \code{logical} should a warning be thrown if one or
#' more conditions are not met?
#' @details This function is similar to \code{\link[raster]{compareRaster}}
#' except that is is less rigorous.
#' @return \code{logical} are all tested parameters the same?
#' @export
gt_compareRaster <- function(x, y, extent=TRUE, rowcol=TRUE, res=TRUE,
                             tolerance=0.1, stopiffalse=TRUE, showwarning=FALSE) {
  assertthat::assert_that(
    assertthat::is.flag(extent),
    assertthat::is.flag(rowcol),
    assertthat::is.flag(res),
    assertthat::is.flag(stopiffalse),
    assertthat::is.flag(showwarning),
    assertthat::is.number(tolerance),
    tolerance > 0
  )
  x$compare(y, extent, rowcol, res, tolerance, stopiffalse, showwarning)
}                             

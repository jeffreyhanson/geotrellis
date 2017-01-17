#' Disaggregate geotrellis raster data
#'
#' This function disaggregates geotrellis raster layer data
#' \code{\link{gt_RasterLayer}}.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param fact \code{integer} Disaggregation factor expressed
#' as number of cells in each direction.
#' @param method \code{character} name of method used to disaggregate
#' values. Available methods are 'ngb' for nearest neighbor assignment,
#' and 'bilinear' for bilinear interpolation. Defaults to 'ngb'.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_disaggregate(g, fact=2, method='ngb')
#' }
#' @export
setGeneric('gt_disaggregate', function(x, ...) {standardGeneric('gt_disaggregate')})

#' @rdname gt_projectRaster
#' @export
setMethod('gt_disaggregate', signature(x='gt_RasterLayer'),
  function(x, fact, method=c('ngb', 'bilinear')) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      all(floor(fact)==fact),
      (length(fact) == 1 || length(fact) == 2),
      all(is.finite(fact)),
      all(fact > 1))
    method <- match.arg(method)
    if (length(fact)==1) fact <- c(fact, fact)
    x$disaggregate(fact, method)
})
 

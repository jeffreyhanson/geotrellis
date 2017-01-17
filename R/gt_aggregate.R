#' Aggregate geotrellis raster data
#'
#' This function aggregates geotrellis raster layer data
#' \code{\link{gt_RasterLayer}}.
#' @param x \code{\link{gt_RasterLayer}} object.
#' @param fact \code{integer} Aggregation factor expressed
#' as number of cells in each direction.
#' @param fun \code{character} name of function used to aggregate
#' values. Available functions are 'mean', 'mode', 'median',
#' 'max', and 'min'. Defaults to 'mean'.
#' @examples
#' \dontrun{
#' g <- gt_raster(raster::raster(matrix(runif(9), ncol=3),
#'                               crs=sp::CRS('+init=epsg:4326'),
#'                               xmn=0, xmx=3, ymn=2, ymx=10))
#' result <- gt_aggregate(g, fact=2, fun='mean')
#' }
#' @export
setGeneric('gt_aggregate', function(x, ...) {standardGeneric('gt_aggregate')})

#' @rdname gt_projectRaster
#' @export
setMethod('gt_aggregate', signature(x='gt_RasterLayer'),
  function(x, fact, fun=c('mean', 'mode', 'median', 'max', 'min')) {
    assertthat::assert_that(
      inherits(x, 'gt_RasterLayer'),
      all(floor(fact)==fact),
      (length(fact) == 1 || length(fact) == 2),
      all(is.finite(fact)),
      all(fact > 1))
    fun <- match.arg(fun)
    if (length(fact)==1) fact <- c(fact, fact)
    x$aggregate(fact, fun)
})

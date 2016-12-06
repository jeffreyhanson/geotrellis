#' @include gt_RasterLayer.R
NULL

#' Benchmark geotrellis  functions
#'
#' This benchmarks functions in this package that leverage geotrellis
#' \url{http://geotrellis.io} and their counterparts in the raster package.
#' @param ncell \code{integer} number of cells in data to use for bencmarking.
#' @param times \code{integer} number of replicate runs.
#' @details This function returns a \code{gt_Benchmark} object. This is basically
#' a list of \code{\link[microbenchmark]{microbenchmark}} objects.
#' @seealso \code{\link{print.gt_Benchmark}}, \code{\link{plot.gt_Benchmark}}.
#' @return \code{gt_Benchmark} object.
#' @export
benchmark <- function(ncell=c(100, 1000, 10000), times = 100L) {
  assertthat::assert_that(
    all(sapply(ncell, assertthat::is.number)),
    all(sapply(sqrt(ncell), assertthat::is.number)),
    assertthat::is.number(times))
  print('TODO')
  structure(1, class = 'gt_Benchmark')
}


#' @export
plot.gt_Benchmark <- function(x) {
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  print('TODO')
  plot(1)
}

#' @export
print.gt_Benchmark <- function(x) {
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  stop('TODO')
}

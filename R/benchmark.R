#' @include gt_RasterLayer.R
NULL

#' Benchmark geotrellis  functions
#'
#' This benchmarks functions in this package that leverage geotrellis
#' \url{http://geotrellis.io} and their counterparts in the raster package.
#' @param ncell \code{integer} number of cells in data to use for benchmarking.
#' @param times \code{integer} number of replicate runs.
#' @param io \code{logical} should input/output functions be benchmarked?
#' @param gp \code{logical} should geoprocessing functions be benchmarked?
#' @param stats \code{logical} should statistical functions be benchmarked?
#' @details This function returns a \code{gt_Benchmark} object. This is basically
#' a list of \code{\link[microbenchmark]{microbenchmark}} objects.
#' @return \code{gt_Benchmark} object.
#' @examples
#' \dontrun{
#' # run short benchmark
#' # note that real benchmarks should use more replications
#' # and that geotrellis is better for bigger data sets
#' b <- benchmark(c(100, 2500), times = 10L)
#' # print results
#' b
#' # plot results
#' plot(b)
#' }
#' @export
benchmark <- function(ncell=c(100, 2500, 1e+6), times = 100L, io=TRUE, gp=TRUE, stats=TRUE) {
  ## init
  assertthat::assert_that(
    all(ncell == ceiling(ncell)),
    all(ncell > 1L),
    assertthat::is.count(times),
    times > 0L,
    assertthat::is.flag(io),
    assertthat::is.flag(gp),
    assertthat::is.flag(stats),
    io || gp || stats)
  # check if microbenchmark installed
  if(!'microbenchmark' %in% utils::installed.packages()[,1])
    stop('the microbenchmark package is not installed')
  # simulate data
  raster_data <- lapply(ncell, function(x) {
    .random.raster(x, crs=sp::CRS('+init=epsg:4326'), xmn=-170, xmx=170, ymn=-80, ymx=80)
  })
  gt_data <- lapply(raster_data, function(x) {
    gt_raster(x)
  })
  ## run benchmarks
  b <- list()
  # input/output benchmarks
  if (io) {
    b[['input/output']] <- lapply(
      seq_along(ncell),
      function(i) {
        # init
        g <- gt_data[[i]]
        r <- raster_data[[i]]
        input.path <- tempfile(fileext='.tif')
        output.path <- tempfile(fileext='.tif')
        writeRaster(r, input.path)
        # benchmark
        microbenchmark::microbenchmark(
          raster_read={raster(input.path)},
          raster_write={writeRaster(r, output.path, overwrite=TRUE)},
          geotrellis_read={gt_raster(input.path)},
          geotrellis_write={gt_writeRaster(g, output.path, overwrite=TRUE)},
          times=times)})
    names(b[['input/output']]) <- as.character(ncell)
  }
  # geoprocessing benchmarks
  if (gp) {
    b[['geoprocessing']] <- lapply( 
      seq_along(ncell),
      function(i) {
        # init
        g <- gt_data[[i]]
        r <- raster_data[[i]]
        r_y <- raster::disaggregate(r, fact=2)
        g_y <- gt_raster(r_y)
        res <- 500000
        crs <- sp::CRS('+init=epsg:3395')
        ext <- raster::extent(c(-170, 0, -80, 0))
        r_m <- .random.raster(r, fun=function(n) {sample(c(NA, 1), size=n, replace=TRUE)})
        g_m <- gt_raster(r_m)
        # benchmark
        microbenchmark::microbenchmark(
          raster_resample={raster::resample(r, r_y)},
          raster_reproject={suppressWarnings(raster::projectRaster(r, crs=crs, res=res))},
          raster_crop={raster::crop(r, ext)},
          raster_mask={raster::mask(r, r_m)},
          raster_aggregate={suppressWarnings(raster::aggregate(r, 2, 'mean'))},
          raster_disaggregate={suppressWarnings(raster::disaggregate(r, 2, ''))},
          geotrellis_resample={gt_resample(g, g_y)},
          geotrellis_reproject={gt_projectRaster(g, to=crs, res=res)},
          geotrellis_crop={gt_crop(g, ext)},
          geotrellis_mask={gt_mask(g, g_m)},
          geotrellis_aggregate={gt_aggregate(g, 2, 'mean')},
          geotrellis_disaggregate={gt_disaggregate(g, 2, 'ngb')},
          times=times)})
    names(b[['geoprocessing']]) <- as.character(ncell)
  }
  # statistics benchmarks
  if (stats) {
    b[['statistics']] <- lapply(
      seq_along(ncell),
      function(i) {
        # init
        g <- gt_data[[i]]
        r <- raster_data[[i]]
        r_z <- .random.raster(r, fun=function(n) {rep(seq_len(ncol(r)),
                                                    each=n/ncol(r))})
        g_z <- gt_raster(r_z)
        # benchmark
        ## not that this is unfair for geotrellis since it also calculates other statistics
        microbenchmark::microbenchmark(
          raster_cellStats={cellStats(r, stat='mean')}, 
          raster_zonal={zonal(r, r_z, fun='mean')},
          geotrellis_cellStats={gt_cellStats(g)},
          geotrellis_zonal={gt_zonal(g, g_z)},
          times=times)})
    names(b[['statistics']]) <- as.character(ncell)
  }
  ## export
  class(b) <- c('gt_Benchmark')
  b
}

#' Plot
#' 
#' This function plots an object.
#' @param x input object.
#' @param ... not used.
#' @return \code{\link{ggplot2}{ggplot2}} object.
#' @name plot
#' @docType methods
NULL

#' @method plot gt_Benchmark
#' @export
plot.gt_Benchmark <- function(x, ...) {
  # init
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  extract_data <- function(x, ncell, type) {
    r <- data.frame(summary(x, 'ms'))[,c('expr', 'median')]
    names(r) <- c('name', 'run_time')
    r$package <- factor(paste(sapply(strsplit(as.character(r$name), '_'), `[[`, 1), 'package'))
    r$Operation <- factor(sapply(strsplit(as.character(r$name), '_'), `[[`, 2))
    r$type <- factor(type)
    r$ncell <- as.numeric(as.character(ncell))
    r
  }
  # extract data
  benchmark_data <- list()
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      benchmark_data <- append(benchmark_data,
                               list(extract_data(x[[i]][[j]],
                                    names(x[[i]])[j], names(x)[i])))
    }
  }
  benchmark_data <- do.call(rbind, benchmark_data)
  # create plot
  ggplot2::ggplot(data=benchmark_data,
                       mapping=ggplot2::aes_string(x='ncell', y='run_time', 
                                            color='Operation')) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::xlab('Number of cells') + 
  ggplot2::ylab('Time (ms)') + 
  ggplot2::facet_grid(package ~ type)
}

#' Print
#' 
#' This function prints an object.
#' @param x input object.
#' @param ... not used.
#' @return None. It is used for the side-effect of printing a message.
#' @name print
#' @docType methods
NULL

#' @method print gt_Benchmark
#' @export
print.gt_Benchmark <- function(x, ...) {
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  message(paste0(
  'class: gt_Benchmark object
  ncell: ', paste(names(x[[1]]), collapse=', '),'
  tests: ', paste(names(x), collapse=', ')))
}

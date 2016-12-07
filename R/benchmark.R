#' @include gt_RasterLayer.R
NULL

#' Benchmark geotrellis  functions
#'
#' This benchmarks functions in this package that leverage geotrellis
#' \url{http://geotrellis.io} and their counterparts in the raster package.
#' @param ncell \code{integer} number of cells in data to use for bencmarking.
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
    all(sapply(ncell, assertthat::is.count)),
    all(sapply(sqrt(ncell), assertthat::is.count)),
    all(ncell > 1L),
    assertthat::is.number(times),
    times > 0L,
    assertthat::is.flag(io),
    assertthat::is.flag(gp),
    assertthat::is.flag(stats),
    io || gp || stats)
  # simulate data
  raster_data <- lapply(ncell, function(x) {
    raster::raster(matrix(runif(ncell), ncol=sqrt(ncell)), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  })
  gt_data <- lapply(raster_data, function(x) {
    gt_raster(x)
  })
  ## run benchmarks
  b <- list()
  # input/output benchmarks
  if (io) {
    b[['input/output']] <- list(
      raster=lapply(raster_data, function(r) {
        # init
        input.path <- tempfile(fileext='.tif')
        output.path <- tempfile(fileext='.tif')
        writeRaster(r, input.path)
        # benchmark
        microbenchmark::microbenchmark(
          read={raster(input.path)},
          write={writeRaster(r, output.path, overwrite=TRUE)},
          values={values(r)},
          times=times)}),
      geotrellis=lapply(gt_data, function(g) {
        # init
        input.path <- tempfile(fileext='.tif')
        output.path <- tempfile(fileext='.tif')
        gt_writeRaster(g, input.path)
        # benchmark
        microbenchmark::microbenchmark(
          read={gt_raster(input.path)},
          write={gt_writeRaster(g, output.path, overwrite=TRUE)},
          values={values(g)},
          times=times)}))
    names(b[['input/output']][['geotrellis']]) <- as.character(ncell)
    names(b[['input/output']][['raster']]) <- as.character(ncell)
  }
  # geoprocessing benchmarks
  if (gp) {
    b[['geoprocessing']] <- list(
      raster = lapply(raster_data, function(r) {
        # init
        y <- raster::disaggregate(r, fact=2)
        res <- 10000
        crs <- sp::CRS('+init=epsg:3395')
        ext <- raster::extent(c(0.1, 1.5, 2.1, 5))
        m <- r
        m[,seq_len(floor(sqrt(ncol(r))/2))] <- NA # make half the columns NA values
        # benchmark
        microbenchmark::microbenchmark(
          resample={resample(r, y)},
          reproject={suppressWarnings(projectRaster(r, crs=crs, res=res))},
          crop={crop(r, ext)},
          mask={mask(r, m)},
          times=times)}),
      geotrellis=lapply(gt_data, function(g) {
        # init
        y <- gt_raster(raster::disaggregate(as.raster(g), fact=2))
        res <- 10000
        crs <- sp::CRS('+init=epsg:3395')
        ext <- raster::extent(c(0.1, 1.5, 2.1, 5))
        m <- as.raster(g)
        m[,seq_len(floor(sqrt(ncol(g))/2))] <- NA # make half the columns NA values
        m <- gt_raster(m)
        # benchmark
        microbenchmark::microbenchmark(
          resample={gt_resample(g, y)},
          reproject={gt_projectRaster(g, to=crs, res=res)},
          crop={gt_crop(g, ext)},
          mask={gt_mask(g, m)},
          times=times)}))
    names(b[['geoprocessing']][['geotrellis']]) <- as.character(ncell)
    names(b[['geoprocessing']][['raster']]) <- as.character(ncell)
  }    
  # statistics benchmarks
  if (stats) {
    b[['statistics']] <- list(
      raster = lapply(raster_data, function(r) {
        # init
        z <- raster::setValues(r, rep(seq_len(ncol(r)), each=nrow(r)))
        # benchmark
        microbenchmark::microbenchmark(
          cellStats={cellStats(r, stat='mean')}, # not that this is unfair for geotrellis 
                                                 # since it also calculates other statistics
          zonal={zonal(r, z, fun='mean')},
          times=times)}),
      geotrellis=lapply(gt_data, function(g) {
        # init
        z <- gt_raster(raster::setValues(as.raster(g), rep(seq_len(ncol(g)), each=nrow(g))))
        # benchmark
        microbenchmark::microbenchmark(
          cellSats={gt_cellStats(g)},
          zonal={gt_zonal(g, z)},
          times=times)}))
    names(b[['statistics']][['geotrellis']]) <- as.character(ncell)
    names(b[['statistics']][['raster']]) <- as.character(ncell)
  }
  ## export
  class(b) <- 'gt_Benchmark'
  b
}

#' Plot
#' 
#' This function plots an object.
#' @param x input object.
#' @return \code{\link{ggplot2}{ggplot2}} object.
#' @export
plot.gt_Benchmark <- function(x) {
  # init
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  extract_timings <- function(x, ncell, method, type) {
    r <- data.frame(summary(x, 's'))[,c('expr', 'median')]
    names(r) <- c('operation_name', 'run_time')
    r$operation_name <- factor(r$operation_name)
    r$operation_type <- factor(type)
    r$method <- factor(method)
    r$ncell <- as.numeric(as.character(ncell))
    return(r)
  }
  # extract data
  d <- list()
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]][[1]])) {
    d <- append(d, list(extract_timings(x[[i]][['geotrellis']][[j]],
                                        names(x[[i]][['geotrellis']])[j],
                                        'geotrellis', names(x)[i])))
    d <- append(d, list(extract_timings(x[[i]][['raster']][[j]],
                                        names(x[[i]][['raster']])[j],
                                        'raster', names(x)[i])))
    }
  }
  benchmark_data <- do.call(rbind, d)
  # create plot
  ggplot2::ggplot(data=benchmark_data,
                       mapping=ggplot2::aes(x=ncell, y=run_time, 
                                            color=operation_name,
                                            linetype=method)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::xlab('Number of cells') + 
  ggplot2::ylab('Time (seconds)') + 
  ggplot2::facet_wrap(~ operation_type, ncol=3) +
  ggplot2::scale_linetype_manual(name='Package',
                                 values = unique(benchmark_data$operation_type)) +
  ggplot2::scale_color_brewer(name='Operation', type='qual', palette='Paired') 
}

#' Print
#' 
#' This function prints an object.
#' @param x input object.
#' @return None. It is used for the side-effect of printing a message.
#' @export
print.gt_Benchmark <- function(x) {
  assertthat::assert_that(inherits(x, 'gt_Benchmark'))
  message(paste0(
'class: gt_Benchmark object
ncell: ', paste(names(x[[1]][[1]]), collapse=', '),'
tests: ', paste(names(x), collapse=', ')))
}

#' Parse projection arguments from CRS object
#'
#' This function takes a \code{\link[sp]{CRS}} object and returns the 
#' Scala command to turn it into a CRS object in geotrellis.
#' @param x \code{\link[sp]{CRS}} object.
#' @return \code{character} with projargs or \code{integer} EPSG code.
#' @example
#' geotrellis:::.parse.CRS(sp::CRS('+init=epsg:4326'))
#' geotrellis:::.parse.CRS(sp::CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#' @noRd
.parse.CRS <- function(x) {
  assertthat::assert_that(inherits(x, 'CRS'))
  x <- x@projargs
  if (grepl('+init=esri:', x, fixed=TRUE)) stop('ESRI codes in proj4strings are not supported') 
  if (grepl('+init=', x, fixed=TRUE)) {
    x <- strsplit(x, ' ')[[1]]
    x <- x[grep('+init=', x, fixed=TRUE)[1]]
    r <- as.integer(gsub('[^0-9]', '', x))
  } else {
    r <- x
  }
  r
}

#' Access raster options
#'
#' This function accesses raster options set using
#' \code{\link[raster]{RasterLayer}} without polluting
#' the console.
#' @return \code{list} object.
#' @noRd
.rasterOptions <- function() {
  a <- {utils::capture.output(r <- raster::rasterOptions())}
  r
}

#' Random raster
#'
#' This function makes a raster with random values.
#' @param x \code{integer} number of cells or
#' \code{\link[raster]{RasterLayer-class}} object.
#' @param ... passed to \code{\link[raster]{raster}}.
#' @param fun \code{function} with \code{n} argument to
#' many values to return. Defaults to \code{\link[base]{runif}}.
#' @return \code{\link[raster]{RasterLayer-class}}
#' @noRd
.random.raster <- function(x, ..., fun = stats::runif, toDisk=NULL) {
  assertthat::assert_that(
    inherits(x, 'RasterLayer') || (inherits(x, 'numeric') &  x == ceiling(x)))
  if (!inherits(x, 'RasterLayer')) {
    if (sqrt(x)==ceiling(sqrt(x))) {
      x <- raster(ncol=sqrt(x), nrow=sqrt(x), ...)
    } else if ((x %% 2) == 0) {
      nrow <- floor(sqrt(x))
      if (ceiling(x/nrow) != (x/nrow))
        nrow <- 2
      x <- raster(ncol=x/nrow, nrow=nrow, ...)
    } else {
      stop('invalid number of cells')
    }
  }
  if (raster::canProcessInMemory(x, 3) & (!isTRUE(toDisk) || is.null(NULL))) {
    w <- raster::setValues(x, fun(n=ncell(x)))
  } else {
    f <- file.path(tempdir(), basename(raster::rasterTmpFile()))
    bs <- raster::blockSize(x)
    w <- raster::writeStart(x, f)
    for (i in seq_len(bs$n)) {
      w <- raster::writeValues(w, fun(n=ncol(x) * bs$nrows[i]), bs$row[i])
    }
    w <- raster::writeStop(w)
  }
  w 
}


#' Parse command to create CRS object in geotrellis
#'
#' This function takes a \code{\link[sp]{CRS}} object and returns the 
#' Scala command to turn it into a CRS object in geotrellis.
#' @param x \code{\link[sp]{CRS}} object.
#' @return \code{character} object.
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
    x <- gsub('[^0-9]', '', x)
    r <- paste0('CRS.fromEpsgCode(',x,')')
  } else {
    r <- paste0('CRS.fromString("',x, '")')
  }
  r
}

#' Parse name of resampling method for geotellis
#'
#' This function takes the name of resampling method according to the
#' conventions used in \code{\link[raster]{raster}} and outputs
#' the name of the method according to the conventions used in
#' geotrellis package.
#' @param x \code{character} object.
#' @return \code{character} object.
#' @examples
#' .parse.resample.method('bilinear')
#' .parse.resample.method('ngb')
#' @noRd
.parse.resample.method <- function(x) {
  assertthat::assert_that(inherits(x, 'character'))
  if (x == 'bilinear') {
    r <- 'Bilinear'
  } else if (x == 'ngb') {
    r <- 'NearestNeighbor'
  } else {
    stop('Invalid name for resampling method')
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
  a <- {capture.output(r <- raster::rasterOptions())}
  r
}

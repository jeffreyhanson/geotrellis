#' Geotrellis raster layer class
#'
#' This class represents geotrellis raster data objects in R. Note that
#' only expert users should attempt to manipulate data using these
#' methods directly.
#'
#' @section Usage:
#' \preformatted{r <- gt_RasterLayer$new()}
#' 
#' r$read_data(path)
#' r$write_data(path)
#' r$delete_data()
#' r$copy_data()
#' r$finalize()
#' r$print()
#'
#' @section Arguments
#' \describe{
#' \item{path}{\code{character} file path for GeoTIFF raster.}
#' }
#'
#' @section Details:
#' \code{$new()} create a new raster layer object.
#' \code{$finalize()} destroy the object.
#' \ode{$read_data()} read data to asosciate with the object.
#' \ode{$write_data()} write data to asosciate with the object.
#' \code{$delete_data()} delete the data associated with the object.
#' \code{$print()} print the object.
#'
#' @name gt_RasterLayer
NULL

#' @export
R6::R6Class('gt_RasterLayer', 
  public = list(
    id = NULL,
    initialize = function() {
      # create id variable to store data in scala
      self$id <<- paste0('rst_', ids::random_id())
    },
    finalize = function() {
      # delete object in scala
      self$delete_data()
    
    },
    read_data = function(path) {
      rscala::scalaEval(get('s', .GlobalEnv), paste0(
        'val ',self$id,':SingleBandGeoTiff = GeoTiffReader.readSingleBand(',path,')'
      ))
    },
    delete_data = function() {
      stop('TODO')
    },
    write_data = function(path) {
      rscala::scalaEval(get('s', .GlobalEnv), paste0(
        self$id,'.write(',path,')'
      ))
    },
    print = function() {
      message('gt_RasterLayer object\n')
    }
  )
)

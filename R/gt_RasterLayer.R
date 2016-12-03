#' Geotrellis raster layer class
#'
#' This class represents geotrellis raster data objects in R. Note that
#' only expert users should attempt to manipulate data using these
#' methods directly.
#'
#' @section Usage:
#'
#' \preformatted{r <- gt_RasterLayer$new()}
#' 
#' r$print()
#'
#' r$read_data(path)
#' r$write_data(path)
#' r$delete_data()
#' r$copy_data()
#'
#' r$values()
#' r$extent()
#' r$crs()
#'
#' @section Arguments:
#'
#' \describe{
#' \item{path}{\code{character} file path for GeoTIFF raster.}
#' }
#'
#' @section Details:
#' 
#' \code{$new()} create a new raster layer object.
#' \code{$finalize()} destroy the object.
#' \code{$print()} print the object.
#' \code{$read_data()} read data to asosciate with the object.
#' \code{$write_data()} write data to asosciate with the object.
#' \code{$delete_data()} delete the data associated with the object.
#' \code{$values()} values in data associated with the object.
#' \code{$extent()} extent of the data associated with the object.
#' \code{$crs()} coordinate reference system associated with the object.
#'
#' @name gt_RasterLayer
#' @seealso \code{\link{gt_RasterLayer-methods}}.
NULL

#' @export
gt_RasterLayer <- R6::R6Class('gt_RasterLayer', 
  public = list(
    
    ## fields
    id = NULL,
    
    ## constructor and destructor methods
    initialize = function(id = ids::random_id()) {
      # create id variable to store data in scala
      self$id <<- paste0('rst_', id)
    },
    finalize = function() {
      # delete object in scala
      self$delete_data()
    },
    
    ## print methods
    print = function() {
      message('gt_RasterLayer object\n')
    },
    
    ## data management methods
    read_data = function(path) {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',self$id,':SinglebandGeoTiff = GeoTiffReader.readSingleband("',path,'")'
      ))
    },
    delete_data = function() {
      print('TODO: raster garbage collection in Scala')
    },
    write_data = function(path) {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        self$id,'.write("',path,'")'
      ))
    },
    
    ## data access methods
    values = function() {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val v = ',self$id,'.toArrayDouble()'
      ))
      rscala::scalaGet(get('s', asNamespace('geotrellis')), 'v')
    },
    crs = function() {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val p = ',self$id,'.crs.toProj4String'
      ))
      sp::CRS(rscala::scalaGet(get('s', asNamespace('geotrellis')), 'p'))
    },
    extent = function() {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val e = Array(',self$id,'.extent.xmin,', self$id,'.extent.xmax,',
            self$id,'.extent.ymin,', self$id,'.extent.ymax)'
      ))
      raster::extent(rscala::scalaGet(get('s', asNamespace('geotrellis')), 'e'))
    }
    
  )
)


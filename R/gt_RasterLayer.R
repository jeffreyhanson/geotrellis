#' @include internal.R
#' @import raster
#' @import sp
NULL

#' Geotrellis raster layer class
#'
#' This class represents geotrellis raster data objects in R. Note that
#' only expert users should attempt to manipulate data using these
#' methods directly. Instead, most users should use the methods provided
#' in the package.
#'
#' @section Fields:
#' 
#' \itemize{
#' \item{$id}{\code{character} name of object in Scala interpreter.}
#' \item{$crs}{\code{\link[sp]{CRS}} coordinate reference system.}
#' \item{$extent}{\code{\link[raster]{Extent}} extent of spatial data.}
#' \item{$res}{\code{numeric} cell resolution (width and height).}
#' \item{$nrow}{\code{integer} number of rows.}
#' \item{$ncol}{\code{integer} number of rows.}
#' \item{$ncell}{\code{integer} number of cells.}
#' \item{$no_data_type}{\code{numeric} value used to represent missing data.}
#' \item{$data_type}{\code{character} description of data type.}
#' }
#'
#' @section Usage:
#'
#' \preformatted{r <- gt_RasterLayer$new()}
#' 
#' r$print()
#'
#' r$read_data(path)
#' r$read_metadata()
#' r$write_data(path)
#' r$delete_data()
#' r$values()
#' r$compare(y)
#'
#' r$project.to.crs(crs, res, method)
#' r$project.to.raster(y, method)
#' r$resample(y, method)
#' r$mask(y, maskvalue)
#' r$crop(extent)
#' r$cellStats(stat)
#' r$zonal(y, stat)
#'
#' @section Arguments:
#'
#' \describe{
#' \item{crs}{\code{\link[sp]{CRS}} coordinate reference system object.}
#' \item{maskvalue}{\code{numeric} value in \code{y} to mask out values.}
#' \item{method}{\code{character} name of method to use for resampling/reprojection.}
#' \item{path}{\code{character} file path for GeoTIFF raster.}
#' \item{stat}{\code{character} name of statistic to calculate.}
#' \item{res}{\code{numeric} resolution for new data.}
#' \item{y}{\code{\link{gt_RasterLayer}} object.}
#' }
#' 
#' @section Details:
#' 
#' \code{$new()} create a new raster layer object.
#' \code{$finalize()} destroy the object.
#'
#' \code{$print()} print the object.
#'
#' \code{$read_data(path)} read spatial data to asosciate with the object.
#' \code{$read_metdata()} load spatial metadata.
#' \code{$write_data()} write data to asosciate with the object.
#' \code{$delete_data()} delete the data associated with the object.
#'
#' \code{$values()} values in data associated with the object.
#' \code{$compare(y)} compare the spatial properties of the object with another object.
#'
#' \code{$project.to.crs(crs, res, method)} project data to new coordinate system.
#' \code{$project.to.raster(y, method)} project data to new coordinate system using
#' another object as a template.
#' \code{$resample(y, method)} resample a data to match the spatial properties of another object.
#' \code{$mask(y, maskvalue)} mask data by another object.
#' \code{$crop(extent)} crop data to a specified extent.
#' \code{$cellStats(stat)} calculate a statistic based on the data.
#' \code{$zonal(y, stat)} calculate summary statistics for each zone.
#'
#' @name gt_RasterLayer
#' @seealso \code{\link{gt_RasterLayer-methods}}.
NULL

#' @export
gt_RasterLayer <- R6::R6Class('gt_RasterLayer', 
  public = list(
    
    ## fields
    id = NULL,
    crs = NULL,
    extent = NULL,
    no_data_value = NULL,
    data_type = NULL,
    res = NULL,
    nrow = NULL,
    ncol = NULL,
    ncell =  NULL,
    
    ## constructor and destructor methods
    initialize = function(id = ids::random_id()) {
      # create id variable to store data in scala
      self$id <<- paste0('rst_', id)
    },
    finalize = function() {
      self$delete_data()
    },
    
    ## print methods
    print = function() {
      message(paste0(
'class       : gt_RasterLayer
dimensions  : ', self$ncol, ', ', self$ncol, ', ', self$ncell, ' (nrow, ncol, ncell)
resolution  : ', round(self$res[1], 6), ', ', round(self$res[2], 6), ' (x, y)
extent      : ', round(self$extent@xmin, 6), ', ', round(self$extent@xmax, 6), ', ', 
                 round(self$extent@ymin, 6), ', ', round(self$extent@ymax, 6), 
                 ' (xmin, xmax, ymin, ymax)
coord. ref. : ', self$crs@projargs,'
data type   : ',self$data_type,'
data source : Scala interpreter\n'))
    },
    
    ## data management methods
    read_data = function(path) {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',self$id,' = GeoTiffReader.readSingleband("',path,'").projectedRaster'
      ))
    },
    delete_data = function() {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',self$id,' = null'
      ))
    },
    write_data = function(path) {
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
       'SinglebandGeoTiff(',self$id,'.tile,',
                            self$id,'.raster.extent,',
                            self$id,'.crs).write("',path,'")'
      ))
    },
    read_metadata = function() {
      # create variables in Scala interpreter
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0('
        val p = ',self$id,'.crs.toProj4String
        val e = Array(',self$id,'.extent.xmin,', self$id,'.extent.xmax,',
            self$id,'.extent.ymin,', self$id,'.extent.ymax)
        val n = ',self$id,'.raster.size
        val nr = ',self$id,'.raster.rows
        val nc = ',self$id,'.raster.cols
        val t  = ',self$id,'.raster.cellType.toString()
        val r = Array(',self$id,'.raster.cellSize.width, ',self$id,'.raster.cellSize.height)
        val nd:Double = ',self$id,'.raster.cellType match {
          case DoubleUserDefinedNoDataCellType(noDataValue) => noDataValue
          case IntUserDefinedNoDataCellType(noDataValue) => noDataValue
          case FloatUserDefinedNoDataCellType(noDataValue) => noDataValue
          case UShortUserDefinedNoDataCellType(noDataValue) => noDataValue
          case ShortUserDefinedNoDataCellType(noDataValue) => noDataValue
          case ByteUserDefinedNoDataCellType(noDataValue) => noDataValue
          case UByteUserDefinedNoDataCellType(noDataValue) => noDataValue
          case _ => throw new Exception("")
        }'
      ))
      # retreive variables from Scala interpreter
      self$no_data_value <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'nd')
      self$crs <<- sp::CRS(rscala::scalaGet(get('s', asNamespace('geotrellis')), 'p'))
      self$extent <<- raster::extent(rscala::scalaGet(get('s', asNamespace('geotrellis')), 'e'))
      self$ncell <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'n')
      self$nrow <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'nr')
      self$ncol <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'nc')
      self$res <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'r')
      self$data_type <<- rscala::scalaGet(get('s', asNamespace('geotrellis')), 't')
      if (!is.numeric(self$no_data_value)) 
        self$no_data_value <- NA_real_
      # check that retreived values are valid
      assertthat::assert_that(
        is.numeric(self$no_data_value),
        inherits(self$crs, 'CRS'),
        inherits(self$extent, 'Extent'),
        assertthat::is.count(self$ncell),
        assertthat::is.count(self$nrow),
        assertthat::is.count(self$ncol),
        (length(self$res) ==2),
        all(is.finite(self$res)),
        all(self$res > 0),
        is.character(self$data_type)
      )
    },
    
    ## data access methods
    values = function() {
      if (grepl('^Double.*$', self$data_type) || grepl('^Float.*$', self$data_type)) {
        rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
          'val v = ',self$id,'.raster.toArrayDouble()'
        ))
      } else {
        rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
          'val v = ',self$id,'.raster.toArray()'
        ))
      }
      r <- rscala::scalaGet(get('s', asNamespace('geotrellis')), 'v')
      r <- replace(r, r == self$no_data_value, NA)
      r
    },
    compare = function(y, extent, rowcol, crs, res, tolerance, stopiffalse, showwarning) {
      # run tests
      r <- TRUE
      if (r && extent)
        r <- r && (abs(self$extent@xmin - y$extent@xmin) <= tolerance) &&
                 (abs(self$extent@xmax - y$extent@xmax) <= tolerance)
                 (abs(self$extent@ymin - y$extent@ymin) <= tolerance)
                 (abs(self$extent@ymax - y$extent@ymax) <= tolerance)
      if (r && rowcol)
        r <- r && (self$ncol == y$ncol) && (self$nrow == y$nrow)
      if (r && crs)
        r <- r && raster::compareCRS(self$crs, y$crs)
      if (r && res)
        r <- r && all(abs(self$res - y$res) <= tolerance)
      # post
      if (!r & stopiffalse)
        stop('data are not comparable')
      if (!r & showwarning)
        warning('data are not comparable')
      r
    },
    
    ## geoprocessing methods
    project.to.crs = function(to, res, method) {
      r <- gt_RasterLayer$new()
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',r$id,' = ',self$id,'.reproject(dest=',.parse.CRS(to),',',
        'options=Reproject.Options(method=',.parse.resample.method(method),',',
                                  'targetCellSize=Option(CellSize(',res[1],',',res[2],'))))'
      ))
      r$read_metadata()
      r
    },
    project.to.raster = function(to, method) {
      r <- gt_RasterLayer$new()
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',r$id,' = ',self$id,'.reproject(dest=',.parse.CRS(to$crs),',',
        'options=Reproject.Options(method=',.parse.resample.method(method),',',
                                  'targetRasterExtent=Option(',to$id,'.rasterExtent)))'
      ))
      r$read_metadata()
      r
    },
    resample = function(y, method) {
      r <- gt_RasterLayer$new()
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',r$id,' = ProjectedRaster(',self$id,'.raster.resample(method=',.parse.resample.method(method),',',
                                                   'target=',y$id,'.rasterExtent),',
                                        self$id,'.crs)'
      ))
      r$read_metadata()
      r
    }, 
    mask = function(y, maskvalue, updatevalue) {
      r <- gt_RasterLayer$new()
      if (is.na(maskvalue)) maskvalue <- 'NODATA'
      if (is.na(updatevalue)) updatevalue <- 'NODATA'
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',r$id,' = ProjectedRaster(Raster(',self$id,'.tile.localMask(r=',y$id,',',
                                                                         'readMask=',maskvalue,',',
                                                                         'writeMask=',updatevalue,'),',
                                               self$id,'.extent),',
                                        self$id,'.crs)'
      ))
      r$read_metadata()
      r
      
    },
    crop = function(extent) {
      r <- gt_RasterLayer$new()
      rscala::scalaEval(get('s', asNamespace('geotrellis')), paste0(
        'val ',r$id,' = ProjectedRaster(',self$id,'.raster.crop(Extent(xmin=',extent@xmin,',',
                                                                      'xmax=',extent@xmax,',',
                                                                      'ymin=',extent@ymin,',',
                                                                      'ymax=',extent@ymax,')),',
                                        self$id,'.crs)'
      ))
      r$read_metadata()
      r
   },
    
    ### statistics methods
    cellStats = function(stat) {
      stop('TODO')
    },
    zonal = function(y, stat) {
      stop('TODO')
    }

  )
)


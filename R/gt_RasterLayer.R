#' @include internal.R
#' @import raster
#' @import sp
NULL

#' @importFrom methods setOldClass
#' @importFrom methods setMethod
#' @export
setOldClass('gt_RasterLayer')

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
    data = NULL,
    crs = NULL,
    extent = NULL,
    no_data_value = NULL,
    data_type = NULL,
    res = NULL,
    nrow = NULL,
    ncol = NULL,
    ncell =  NULL,
    
    ## constructor and destructor methods
    initialize = function(data) {
      # create data
      if (inherits(data, 'ScalaInterpreterReference')) {
        self$data <<- data
        self$read_metadata()
      } else if (inherits(data, 'character')) {
        self$read_data(data)
        self$read_metadata()
      } else {
        stop('data type not valid')
      }
      
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
      self$data <<- get('.read_data', asNamespace('geotrellis'))(path, as.reference=TRUE)
    },
    delete_data = function() {
      self$data <- NULL
    },
    write_data = function(path) {
      invisible(get('.write_data', asNamespace('geotrellis'))(self$data, path))
    },
    read_metadata = function() {
      # retreive variables from Scala interpreter
      self$no_data_value <<- get('.read_metadata_no_data_value', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      self$crs <<- sp::CRS(get('.read_metadata_crs', asNamespace('geotrellis'))(self$data, as.reference=FALSE))
      self$extent <<- raster::extent(get('.read_metadata_extent', asNamespace('geotrellis'))(self$data, as.reference=FALSE))
      self$ncell <<- get('.read_metadata_ncell', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      self$nrow <<- get('.read_metadata_nrow', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      self$ncol <<- get('.read_metadata_ncol', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      self$res <<- get('.read_metadata_res', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      self$data_type <<- get('.read_metadata_data_type', asNamespace('geotrellis'))(self$data, as.reference=NA)
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
        r <- get('.values_double', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      } else {
        r <- get('.values_integer', asNamespace('geotrellis'))(self$data, as.reference=FALSE)
      }
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
    project_to_crs = function(to, res, method) {
      to <- .parse.CRS(to)
      if (is.numeric(to)) {
        r <- gt_RasterLayer$new(get('.project_to_epsg_crs', asNamespace('geotrellis'))(self$data, to, res, method, as.reference=TRUE))
      } else {
        r <- gt_RasterLayer$new(get('.project_to_unnamed_crs', asNamespace('geotrellis'))(self$data, to, res, method, as.reference=TRUE))
      }
      r
    },
    project_to_raster = function(to, method) {
      gt_RasterLayer$new(get('.project_to_raster', asNamespace('geotrellis'))(self$data, to$data, method, as.reference=TRUE))
    },
    resample = function(y, method) {
      gt_RasterLayer$new(get('.resample', asNamespace('geotrellis'))(self$data, y$data, method, as.reference=TRUE))
    }, 
    mask = function(y, maskvalue, updatevalue) {
      gt_RasterLayer$new(get('.mask', asNamespace('geotrellis'))(self$data, y$data, maskvalue, updatevalue, as.reference=TRUE))
    },
    crop = function(extent) {
      gt_RasterLayer$new(get('.crop', asNamespace('geotrellis'))(self$data, extent@xmin, extent@xmax, extent@ymin, extent@ymax, as.reference=TRUE))
    },
  
    ### statistics methods
    cellStats = function() {
      structure(get('.cellStats', asNamespace('geotrellis'))(self$data, as.reference=FALSE),
                names=c('mean', 'median', 'mode', 'sd', 'min', 'max'))
    },
    zonal = function(y) {
      r <- as.data.frame(get('.zonal', asNamespace('geotrellis'))(self$data, y$data, as.reference=FALSE))
      names(r) <- c('zone', 'mean', 'median', 'mode', 'sd')
      r <- r[is.finite(r[[1]]),]
      r <- r[order(r[[1]]),]
      rownames(r) <- NULL
      r
    }
  )
)

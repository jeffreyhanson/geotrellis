.onAttach <- function(libname, pkgname) {
  # instantiate scala interpreter
  s <- rscala::scala(classpath = system.file('java/geotrellis.jar', package='geotrellis'), stdout=FALSE, stderr=FALSE)

  # load scala dependencies
  rscala::scalaEval(s, paste0('
    import geotrellis.proj4.CRS
    import geotrellis.vector._
    import geotrellis.raster._
    import geotrellis.raster.io.geotiff._
    import geotrellis.raster.io.geotiff.reader.GeoTiffReader
    import geotrellis.raster.io.geotiff.writer.GeoTiffWriter
    import geotrellis.raster.reproject._
    import geotrellis.raster.resample._
    import geotrellis.raster.mapalgebra._
    type CellType = DataType with NoDataHandling
  '))
  
  # for reasons unknown...
  # the first scalaDef that uses geotrellis always results in an error
  # so trigger this error and then moeve onto the real definitations
  r <- capture.output(try(rscala::scalaDef(s,'x:geotrellis.raster.ProjectedRaster[geotrellis.raster.Tile]', 'x'), silent=TRUE))
  
  # create scala functions
  .read_data <- rscala::scalaDef(s,'path:String',
                                'GeoTiffReader.readSingleband(path).projectedRaster')
  .write_data <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], path: String',     
                                 'SinglebandGeoTiff(x.tile,x.raster.extent,x.crs).write(path)')
  .read_metadata_data_type <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.cellType.toString()')
  .read_metadata_crs <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.crs.toProj4String')
  .read_metadata_extent <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 
                                           'Array(x.extent.xmin, x.extent.xmax, x.extent.ymin, x.extent.ymax)')
  .read_metadata_ncell <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.size')
  .read_metadata_nrow <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.rows')
  .read_metadata_ncol <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.cols')
  .read_metadata_res <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]',
                                        'Array(x.raster.cellSize.width, x.raster.cellSize.height)')
  .read_metadata_no_data_value <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]',
    'val nd:Double = x.raster.cellType match {
      case DoubleUserDefinedNoDataCellType(noDataValue) => noDataValue
      case IntUserDefinedNoDataCellType(noDataValue) => noDataValue
      case FloatUserDefinedNoDataCellType(noDataValue) => noDataValue
      case UShortUserDefinedNoDataCellType(noDataValue) => noDataValue
      case ShortUserDefinedNoDataCellType(noDataValue) => noDataValue
      case ByteUserDefinedNoDataCellType(noDataValue) => noDataValue
      case UByteUserDefinedNoDataCellType(noDataValue) => noDataValue
      case _ => throw new Exception("")
      }
      nd')
  .values_double <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.toArrayDouble()')
  .values_integer <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]', 'x.raster.toArray()')
  .project_to_unnamed_crs <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], to:String, res:Array[Double], method:String',
                         'if (method=="ngb") {
                          x.reproject(dest=CRS.fromString(to), options=Reproject.Options(method=NearestNeighbor, targetCellSize=Option(CellSize(res(0), res(1)))))
                         } else {
                          x.reproject(dest=CRS.fromString(to), options=Reproject.Options(method=Bilinear, targetCellSize=Option(CellSize(res(0), res(1)))))
                         }') 
  .project_to_epsg_crs <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], to:Int, res:Array[Double], method:String',
                         'if (method=="ngb") {
                            x.reproject(dest=CRS.fromEpsgCode(to), options=Reproject.Options(method=NearestNeighbor, targetCellSize=Option(CellSize(res(0), res(1)))))
                          } else {
                            x.reproject(dest=CRS.fromEpsgCode(to), options=Reproject.Options(method=Bilinear, targetCellSize=Option(CellSize(res(0), res(1)))))
                          }')
  .project_to_raster <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], to:ProjectedRaster[Tile], method:String',
                         'if (method=="ngb") {
                            x.reproject(dest=to.crs, options=Reproject.Options(method=NearestNeighbor, targetRasterExtent=Option(to.rasterExtent)))
                          } else {
                            x.reproject(dest=to.crs, options=Reproject.Options(method=Bilinear, targetRasterExtent=Option(to.rasterExtent)))
                          }')
  .resample <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], to:ProjectedRaster[Tile], method:String',
                         'if (method=="ngb") {
                            ProjectedRaster(x.raster.resample(method=NearestNeighbor, target=to.rasterExtent),x.crs)
                          } else {
                            ProjectedRaster(x.raster.resample(method=Bilinear, target=to.rasterExtent),x.crs)
                          }')
  .mask <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], y:ProjectedRaster[Tile], maskvalue:Int, updatevalue:Int',
                         'ProjectedRaster(Raster(x.tile.localMask(r=y, readMask=maskvalue, writeMask=updatevalue), x.extent), x.crs)')
  .crop <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], xmin:Double, xmax:Double, ymin:Double, ymax:Double',
                         'ProjectedRaster(x.raster.crop(Extent(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)), x.crs)')
  .cellStats <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile]',
                         'var s = x.tile.statisticsDouble.get
                          Array(s.mean, s.median, s.mode, s.stddev, s.zmin, s.zmax)')
  .zonal <- rscala::scalaDef(s, 'x:ProjectedRaster[Tile], y:ProjectedRaster[Tile]',
                         'val a = x.tile.zonalStatisticsDouble(y)
                                  .map{case (k,v) => Array(k, v.mean, v.median, v.mode, v.stddev)}
                                  .toArray
                          for (i <- 0 until a.length) {
                            if (a(i)(0).toInt == NODATA) {
                              a(i)(0) = Double.NaN;
                            }
                          }
                          a')

}
.onUnload <- function(libpath) {
  try(rscala::close(get('s', asNamespace('geotrellis'))), silent=TRUE)
}


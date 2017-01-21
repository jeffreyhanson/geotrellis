
.pkgenv <- new.env(parent=emptyenv())

.onAttach <- function(libname, pkgname) {
  # get amount of memory to use for java virtual machine
  m <- options()$rscala.heap.maximum
  if (is.null(m))
    m <- .memory.size()

  # instantiate scala interpreter
  assign('s', envir=.pkgenv,
    rscala::scala(classpath = system.file('java/geotrellis.jar', package='geotrellis'), stdout=TRUE, stderr=TRUE, heap.maximum=m))

  # load scala dependencies
  rscala::scalaEval(get('s', .pkgenv), paste0('
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
  r <- try(rscala::scalaDef(get('s', .pkgenv),
                                'x:ProjectedRaster[Tile]', 'x'), silent=TRUE)

#   print(1)
#   r <- try(.silent.scalaDef(get('s', .pkgenv),
#                                 'x:ProjectedRaster[Tile]', 'x'), silent=TRUE)
#   print(2)
  
  # create scala functions
  assign('.read_data', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv),'path:String',
                         'GeoTiffReader.readSingleband(path).projectedRaster'))
  assign('.write_data', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], path: String',     
                         'SinglebandGeoTiff(x.tile,x.raster.extent,x.crs).write(path)'))
  assign('.read_metadata_data_type', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.cellType.toString()'))
  assign('.read_metadata_crs', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.crs.toProj4String'))
  assign('.read_metadata_extent', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 
                         'Array(x.extent.xmin, x.extent.xmax, x.extent.ymin, x.extent.ymax)'))
  assign('.read_metadata_ncell', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.size'))
  assign('.read_metadata_nrow', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.rows'))
  assign('.read_metadata_ncol', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.cols'))
  assign('.read_metadata_res', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]',
                         'Array(x.raster.cellSize.width, x.raster.cellSize.height)'))
  assign('.read_metadata_no_data_value', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]',
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
                          nd'))
  assign('.values_double', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.toArrayDouble()'))
  assign('.values_integer', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]', 'x.raster.toArray()'))
  assign('.project_to_unnamed_crs', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], to:String, res:Array[Double], method:String',
                         'if (method=="ngb") {
                          x.reproject(dest=CRS.fromString(to), options=Reproject.Options(method=NearestNeighbor, targetCellSize=Option(CellSize(res(0), res(1)))))
                         } else {
                          x.reproject(dest=CRS.fromString(to), options=Reproject.Options(method=Bilinear, targetCellSize=Option(CellSize(res(0), res(1)))))
                         }')) 
  assign('.project_to_epsg_crs', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], to:Int, res:Array[Double], method:String',
                         'if (method=="ngb") {
                            x.reproject(dest=CRS.fromEpsgCode(to), options=Reproject.Options(method=NearestNeighbor, targetCellSize=Option(CellSize(res(0), res(1)))))
                          } else {
                            x.reproject(dest=CRS.fromEpsgCode(to), options=Reproject.Options(method=Bilinear, targetCellSize=Option(CellSize(res(0), res(1)))))
                          }'))
  assign('.project_to_raster', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], to:ProjectedRaster[Tile], method:String',
                         'if (method=="ngb") {
                            x.reproject(dest=to.crs, options=Reproject.Options(method=NearestNeighbor, targetRasterExtent=Option(to.rasterExtent)))
                          } else {
                            x.reproject(dest=to.crs, options=Reproject.Options(method=Bilinear, targetRasterExtent=Option(to.rasterExtent)))
                          }'))
  assign('.resample', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], to:ProjectedRaster[Tile], method:String',
                         'if (method=="ngb") {
                            ProjectedRaster(x.raster.resample(method=NearestNeighbor, target=to.rasterExtent),x.crs)
                          } else {
                            ProjectedRaster(x.raster.resample(method=Bilinear, target=to.rasterExtent),x.crs)
                          }'))
  assign('.mask', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], y:ProjectedRaster[Tile], maskvalue:Int, updatevalue:Int',
                         'ProjectedRaster(Raster(x.tile.localMask(r=y, readMask=maskvalue, writeMask=updatevalue), x.extent), x.crs)'))
  assign('.crop', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], xmin:Double, xmax:Double, ymin:Double, ymax:Double',
                         'ProjectedRaster(x.raster.crop(Extent(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax)), x.crs)'))
  assign('.aggregate', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], fact_x:Double, fact_y:Double, fun:String',
                         'val newRasterExtent=x.rasterExtent.withResolution(targetCellWidth=x.raster.cellSize.width*fact_x,
                                                                        targetCellHeight=x.raster.cellSize.height*fact_y)
                          if (fun=="mean") {
                            ProjectedRaster(x.raster.resample(method=Average, target=newRasterExtent),x.crs)
                          } else if (fun=="median"){
                            ProjectedRaster(x.raster.resample(method=Median, target=newRasterExtent),x.crs)
                          } else if (fun=="mode"){
                            ProjectedRaster(x.raster.resample(method=Mode, target=newRasterExtent),x.crs)
                          } else if (fun=="max"){
                            ProjectedRaster(x.raster.resample(method=Max, target=newRasterExtent),x.crs)
                          } else {
                            ProjectedRaster(x.raster.resample(method=Min, target=newRasterExtent),x.crs)
                          }
                          '))
  assign('.disaggregate', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], fact_x:Double, fact_y:Double, method:String',
                         'val newRasterExtent=x.rasterExtent.withResolution(targetCellWidth=x.raster.cellSize.width/fact_x,
                                                                        targetCellHeight=x.raster.cellSize.height/fact_y)
                         if (method=="ngb") {
                            ProjectedRaster(x.raster.resample(method=NearestNeighbor, target=newRasterExtent),x.crs)
                          } else {
                            ProjectedRaster(x.raster.resample(method=Bilinear, target=newRasterExtent),x.crs)
                          }'))
  assign('.cellStats', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile]',
                         'var s = x.tile.statisticsDouble.get
                          Array(s.mean, s.median, s.mode, s.stddev, s.zmin, s.zmax)'))
  assign('.zonal', envir=.pkgenv,
         rscala::scalaDef(get('s', .pkgenv), 'x:ProjectedRaster[Tile], y:ProjectedRaster[Tile]',
                         'val a = x.tile.zonalStatisticsDouble(y)
                                  .map{case (k,v) => Array(k, v.mean, v.median, v.mode, v.stddev)}
                                  .toArray
                          for (i <- 0 until a.length) {
                            if (a(i)(0).toInt == NODATA) {
                              a(i)(0) = Double.NaN;
                            }
                          }
                          a'))  
}
.onUnload <- function(libpath) {
  try(rscala:::close.ScalaInterpreter(get('s', .pkgenv)), silent=TRUE)
}


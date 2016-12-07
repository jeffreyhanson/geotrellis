.onAttach <- function(libname, pkgname) {
  # instantiate scala interpreter
  rscala::.rscalaPackage(pkgname, system.file('java/geotrellis.jar', package='geotrellis'))
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
}

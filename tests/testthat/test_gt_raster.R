context('gt_raster')

test_that('gt_raster.character', {
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3))
  input.path <- tempfile(fileext='.tif')
  output.path <- tempfile(fileext='.tif')
  raster::writeRaster(rst, input.path, overwrite=TRUE)
  # send data to Scala interpreter
  g <- gt_raster(input.path)
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(rst[], rst2[])
})

test_that('gt_raster.RasterLayer', {
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3))
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(rst[], rst2[])
})

test_that('gt_raster.default', {
  # create data
  mtx <- matrix(1:9, ncol=3)
  # send data to Scala interpreter
  g <- gt_raster(mtx)
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(c(t(mtx)), rst2[])
})

test_that('gt_raster methods', {
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=1, ymn=2, ymx=3)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # tests
  expect_true(raster::compareCRS(rst@crs, crs(g)))
  expect_equal(rst@extent, extent(g))
  expect_equal(raster::values(rst), values(g))
})




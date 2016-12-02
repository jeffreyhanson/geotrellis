context('gt_raster')

test_that('gt_raster.character', {
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3))
  input.path <- tempfile(fileext='.tif')
  output.path <- tempfile(fileext='.tif')
  raster::writeRaster(rst, input.path, overwrite=TRUE)
  # send data to Scala interpreter
  g <- gt_raster(path)
  # receive data from Scala interpreter
  rst2 <- as.RasterLayer(g)
  # tests
  expect_equal(rst[], rst2[])
})

# test_that('gt_raster.RasterLayer', {
#   # create data
#   rst <- raster::raster(matrix(1:9, ncol=3))
#   # send data to Scala interpreter
#   g <- gt_raster(rst)
#   # receive data from Scala interpreter
#   rst2 <- as.RasterLayer(g)
#   # tests
#   expect_equal(rst[], rst2[])
# })
# 
# test_that('gt_raster.default', {
#   # create data
#   mtx <- matrix(1:9, ncol=3)
#   # send data to Scala interpreter
#   g <- gt_raster(mtx)
#   # receive data from Scala interpreter
#   rst2 <- as.RasterLayer(g)
#   # tests
#   expect_equal(c(t(mtx)), rst2[])
# })
# 

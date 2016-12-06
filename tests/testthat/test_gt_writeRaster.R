context('gt_writeRaster')

test_that('gt_writeRaster', {
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=1, xmx=3, ymn=2, ymx=8)
  path <- tempfile(fileext='.tif')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # write data from Scala interpreter
  gt_writeRaster(g, path)
  # load data from Scala interpreter
  rst2 <- raster(path)
  # tests
  expect_true(raster::compareCRS(rst@crs, (rst2@crs)))
  expect_equal(raster::extent(rst), raster::extent(rst2))
  expect_equal(raster::values(rst), raster::values(rst2))
  expect_equal(raster::res(rst), raster::res(rst2))
})

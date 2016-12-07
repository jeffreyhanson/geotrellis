context('gt_raster')

test_that('gt_raster (x=character)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'))
  input.path <- tempfile(fileext='.tif')
  output.path <- tempfile(fileext='.tif')
  raster::writeRaster(rst, input.path, overwrite=TRUE)
  # send data to Scala interpreter
  g <- gt_raster(input.path)
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(rst[], rst2[])
  expect_true(raster::compareRaster(rst, rst2, stopiffalse=FALSE))
})

test_that('gt_raster (x=RasterLayer)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'))
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(rst[], rst2[])
  expect_true(raster::compareRaster(rst, rst2, stopiffalse=FALSE))
})

test_that('gt_raster (x=ANY)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  mtx <- matrix(1:9, ncol=3)
  # send data to Scala interpreter
  g <- gt_raster(mtx, crs=sp::CRS('+init=epsg:4326'))
  # receive data from Scala interpreter
  rst2 <- as.raster(g)
  # tests
  expect_equal(c(t(mtx)), rst2[])
})

test_that('gt_RasterLayer (methods)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA,1:5), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # tests
  print(g)
  expect_true(raster::compareCRS(rst@crs, crs(g)))
  expect_equal(rst@extent, extent(g))
  expect_equal(raster::values(rst), values(g))
  expect_equal(raster::res(rst), res(g))
  expect_equal(raster::ncell(rst), ncell(g))
  expect_equal(raster::ncol(rst), ncol(g))
  expect_equal(raster::nrow(rst), nrow(g))
  expect_equal(raster::as.matrix(rst), as.matrix(g))
})


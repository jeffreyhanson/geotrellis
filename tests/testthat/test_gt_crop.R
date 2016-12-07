context('gt_crop')

test_that('gt_crop (x=gt_RasterLayer, y=Extent)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(1:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  ext <- raster::extent(c(0, 3, 2, 6))
  rst2 <- raster::crop(rst, ext)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(rst2)
  # crop data
  g2 <- gt_crop(g, ext)
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::extent(rst2), extent(g2))
  expect_equal(raster::values(rst2), values(g2))
  expect_equal(raster::res(rst2), res(g2))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))
})

test_that('gt_crop (x=gt_RasterLayer, y=gt_RasterLayer)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(1:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::raster(matrix(1, ncol=3), xmn=0, xmx=3, ymn=2, ymx=6)
  rst3 <- raster::crop(rst, rst2)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(rst2)
  # crop data
  g2 <- gt_crop(g, g2)
  # tests
  expect_true(raster::compareCRS(rst3@crs, crs(g2)))
  expect_equal(raster::extent(rst3), extent(g2))
  expect_equal(raster::values(rst3), values(g2))
  expect_equal(raster::res(rst3), res(g2))
  expect_equal(raster::nrow(rst3), nrow(g2))
  expect_equal(raster::ncol(rst3), ncol(g2))
  expect_equal(raster::ncell(rst3), ncell(g2))
})

test_that('gt_crop (expected errors)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=1, xmx=3, ymn=2, ymx=8)
  rst2 <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=10, xmx=30, ymn=20, ymx=80)
  rst3 <- rst
  rst3@crs <- sp::CRS('+init=epsg:3395')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(rst2)
  g3 <- gt_raster(rst3)
  # tests
  expect_error(gt_crop(g, NULL))
  expect_error(gt_crop(g, g2)) # rasters don't intersect
  expect_error(gt_crop(g, g3)) # rasters have different crs
})

 

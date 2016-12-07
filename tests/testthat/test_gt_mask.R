context('gt_mask')

test_that('gt_mask (x=gt_RasterLayer, y=gt_RasterLayer)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(1:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  mask <- raster::setValues(rst, c(NA, 1:4, NA))
  rst2 <- mask(rst, mask)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(mask)
  # mask data
  g3 <- gt_mask(g, g2)
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g3)))
  expect_equal(raster::extent(rst2), extent(g3))
  expect_equal(raster::values(rst2), values(g3))
  expect_equal(raster::res(rst2), res(g3))
  expect_equal(raster::nrow(rst2), nrow(g3))
  expect_equal(raster::ncol(rst2), ncol(g3))
  expect_equal(raster::ncell(rst2), ncell(g3))
})

test_that('gt_mask (x=gt_RasterLayer, y=gt_RasterLayer, maskvalue, updatevalue)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(1:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  mask <- raster::setValues(rst, c(9, 1:4, 9))
  rst2 <- mask(rst, mask, maskvalue=9, updatevalue=12)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(mask)
  # mask data
  g3 <- gt_mask(g, g2, maskvalue=9, updatevalue=12)
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g3)))
  expect_equal(raster::extent(rst2), extent(g3))
  expect_equal(raster::values(rst2), values(g3))
  expect_equal(raster::res(rst2), res(g3))
  expect_equal(raster::nrow(rst2), nrow(g3))
  expect_equal(raster::ncol(rst2), ncol(g3))
  expect_equal(raster::ncell(rst2), ncell(g3))
})

test_that('gt_mask (expected errors)', {
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
  expect_error(gt_mask(g, NULL))
  expect_error(gt_mask(g, g2)) # rasters don't intersect
  expect_error(gt_mask(g, g3)) # rasters have different crs
})

 

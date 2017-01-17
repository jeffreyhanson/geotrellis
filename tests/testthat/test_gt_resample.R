context('gt_resample')

test_that('gt_resample (method=ngb)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::resample(rst, raster::disaggregate(rst, 2, method=''), method='ngb')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(raster::setValues(rst2, 1))
  # resample data
  g2 <- gt_resample(g, g2, method='ngb')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::extent(rst2), extent(g2))
  expect_equal(raster::values(rst2), values(g2))
  expect_equal(raster::res(rst2), res(g2))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))  
})

test_that('gt_resample (method=bilinear)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::resample(rst, raster::disaggregate(rst, 2, method=''), method='bilinear')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # resample data
  g2 <- gt_resample(g, gt_raster(raster::setValues(rst2, 1)), method='bilinear')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::extent(rst2), extent(g2))
  expect_equal(raster::values(rst2), values(g2))
  expect_equal(raster::res(rst2), res(g2))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))  
})

test_that('gt_resample (expected errors)', {
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
  expect_error(gt_resample(g, NULL))
  expect_error(gt_resample(g, g2)) # rasters don't intersect
  expect_error(gt_resample(g, g3)) # rasters have different crs
})


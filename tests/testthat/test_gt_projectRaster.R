context('gt_projectRaster')

test_that('gt_projectRaster (x=gt_RasterLayer, y=CRS, method=ngb)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- suppressWarnings(raster::projectRaster(rst, crs=sp::CRS('+init=epsg:3395'), res=50000, method='ngb'))
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # reproject data
  g2 <- gt_projectRaster(g, rst2@crs, res=res(rst2), method='ngb')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::res(rst2), res(g2))
})

test_that('gt_projectRaster (x=gt_RasterLayer, y=CRS, method=bilinear)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- suppressWarnings(raster::projectRaster(rst, crs=sp::CRS('+init=epsg:3395'), res=50000, method='bilinear'))
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # reproject data
  g2 <- gt_projectRaster(g, rst2@crs, res=res(rst2), method='bilinear')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::res(rst2), res(g2))
})

test_that('gt_projectRaster (x=gt_RasterLayer, y=gt_RasterLayer, method=ngb)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::crop(rst, extent(c(1,1.5,2,6)))
  rst2 <- suppressWarnings(raster::projectRaster(rst2, crs=sp::CRS('+init=epsg:3395'), res=50000, method='ngb'))
  rst2@crs@projargs <- gsub('=0 ', '=0.0 ', rst2@crs@projargs, fixed=TRUE)
  rst2@crs@projargs <- gsub('=1 ', '=1.0 ', rst2@crs@projargs, fixed=TRUE)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(raster::setValues(rst2, 1))
  # reproject data
  g3 <- gt_projectRaster(g, g2, method='ngb')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g3)))
  expect_equal(raster::extent(rst2), extent(g3))
  expect_equal(raster::values(rst2), values(g3))
  expect_equal(raster::res(rst2), res(g3))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))  
})

test_that('gt_projectRaster (x=gt_RasterLayer, y=gt_RasterLayer, method=bilinear)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  rst <- raster::raster(matrix(c(NA, 2:6), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::crop(rst, extent(c(1,1.5,2,6)))
  rst2 <- suppressWarnings(raster::projectRaster(rst2, crs=sp::CRS('+init=epsg:3395'), res=50000, method='bilinear'))
  rst2@crs@projargs <- gsub('=0 ', '=0.0 ', rst2@crs@projargs, fixed=TRUE)
  rst2@crs@projargs <- gsub('=1 ', '=1.0 ', rst2@crs@projargs, fixed=TRUE)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(raster::setValues(rst2, 1))
  # reproject data
  g3 <- gt_projectRaster(g, g2, method='bilinear')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g3)))
  expect_equal(raster::extent(rst2), extent(g3))
  expect_equal(raster::values(rst2), values(g3))
  expect_equal(raster::res(rst2), res(g3))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))  
})

test_that('gt_projectRaster (expected errors)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=1, xmx=3, ymn=2, ymx=8)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # tests
  expect_error(gt_projectRaster(g, NA, 50000))
  expect_error(gt_projectRaster(g, NULL, 50000))
  expect_error(gt_projectRaster(g, sp::CRS(), 50000))
  expect_error(gt_projectRaster(g, sp::CRS('+init=epsg:3395'), NA))
  expect_error(gt_projectRaster(g, sp::CRS('+init=epsg:3395'), NULL))
  expect_error(gt_projectRaster(g, sp::CRS('+init=epsg:3395'), -5))  
})


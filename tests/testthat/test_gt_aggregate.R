context('gt_aggregate')

test_that('gt_aggregate (fun=mean)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA, 2:8), ncol=4), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst2 <- raster::aggregate(rst, 2, fun='mean')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # resample data
  g2 <- gt_aggregate(g, 2, fun='mean')
  # tests
  expect_true(raster::compareCRS(rst2@crs, crs(g2)))
  expect_equal(raster::extent(rst2), extent(g2))
  expect_equal(raster::values(rst2), values(g2))
  expect_equal(raster::res(rst2), res(g2))
  expect_equal(raster::nrow(rst2), nrow(g2))
  expect_equal(raster::ncol(rst2), ncol(g2))
  expect_equal(raster::ncell(rst2), ncell(g2))  
})

test_that('gt_aggregate (expected errors)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(1:9, ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=1, xmx=3, ymn=2, ymx=8)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # tests
  expect_error(gt_aggregate(g, Inf))
  expect_error(gt_aggregate(g, NA))
  expect_error(gt_aggregate(g, -5))
  expect_error(gt_aggregate(g, 2.5))
})

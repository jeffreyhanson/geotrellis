context('gt_zonal')

test_that('gt_zonal (x=gt_RasterLayer, y=gt_RasterLayer)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(seq_len(81), ncol=9), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  rst[,8] <- rst[,9]
  zones <- raster::setValues(rst, replace(rep(1:9, each=9), 1, NA))
  m <- lapply(c(mean, median, 
                function(x, na.rm=TRUE) {as.numeric(names(which.max(table(x))))},
                function(x, na.rm=TRUE) {n <- sum(!is.na(x)); n <- sqrt((n-1)/n); sd(x, na.rm=TRUE) * n}),
              zonal, x=rst, z=zones)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(zones)
  # calculate zonal stats
  m2 <- gt_zonal(g, g2)
  # tests
  expect_equal(m[[1]][,1], m2$zone)
  expect_equal(m[[1]][,2], m2$mean)
  expect_equal(m[[2]][,2], m2$median)
  expect_equal(m[[3]][,2], m2$mode)
  expect_equal(m[[4]][,2], m2$sd)
})

test_that('gt_zonal (expected errors)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(seq_len(81), ncol=9), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  zones <- raster::setValues(rst, replace(rep(1:9, each=9), 1, NA))
  zones2 <- zones
  extent(zones2) <- extent(c(10, 13, 12, 18))
  zones3 <- zones
  zones3@crs <- sp::CRS('+init=epsg:3395')
  # send data to Scala interpreter
  g <- gt_raster(rst)
  g2 <- gt_raster(zones2)
  g3 <- gt_raster(zones3)
  # tests
  expect_error(gt_zonal(g, NULL))
  expect_error(gt_zonal(g, g2)) # rasters don't intersect
  expect_error(gt_zonal(g, g3)) # rasters have different crs
})

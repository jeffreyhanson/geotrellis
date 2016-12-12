context('internal functions')

test_that('.parse.CRS', {
  expect_equal(.parse.CRS(CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
                          '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
  expect_equal(.parse.CRS(CRS('+init=epsg:4326')), 4326L)
  expect_error(.parse.CRS(CRS('+init=esri:37206')))
  expect_error(.parse.CRS('+init=epsg:4326'))
})

test_that('.random.raster (dimensions)', {
  # generate data
  r1 <- .random.raster(10, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90)
  r2 <- .random.raster(12, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90)
  r3 <- .random.raster(9, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90)
  # tests
  expect_is(r1, 'RasterLayer')
  expect_is(r2, 'RasterLayer')
  expect_is(r3, 'RasterLayer')
  expect_equal(dim(r1), c(2,5,1))
  expect_equal(dim(r2), c(3,4,1))
  expect_equal(dim(r3), c(3,3,1))
})

test_that('.random.raster (number generation)', {
  # generate data
  set.seed(500)
  r <- .random.raster(100, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90)
  set.seed(500)
  r2 <- .random.raster(100, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90, toDisk=TRUE)
  set.seed(500)
  r3 <- .random.raster(r, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90, toDisk=TRUE)
  # tests
  expect_is(r, 'RasterLayer')
  expect_is(r2, 'RasterLayer')
  expect_is(r3, 'RasterLayer')
  expect_equal(raster::values(r), raster::values(r2), raster::values(r3))
  expect_true(raster::compareRaster(r, r2, r3, stopiffalse=FALSE))
})

test_that('.random.raster (errors)', {
  expect_error(.random.raster(3, crs=sp::CRS('+init=epsg:4326'), xmn=-180, xmx=180, ymn=-90, ymx=90))
})

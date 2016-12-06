context('internal functions') 

test_that('.parse.resample.method', {
  expect_equal(.parse.resample.method('bilinear'), 'Bilinear')
  expect_equal(.parse.resample.method('ngb'), 'NearestNeighbor')
  expect_error(.parse.resample.method('other'))
  expect_error(.parse.resample.method(NA))
  expect_error(.parse.resample.method(NULL))
})

test_that('.parse.CRS', {
  expect_equal(.parse.CRS(CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')),
                          'CRS.fromString(\"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0\")')
  expect_equal(.parse.CRS(CRS('+init=epsg:4326')), 'CRS.fromEpsgCode(4326)')
  expect_error(.parse.CRS(CRS('+init=esri:37206')))
  expect_error(.parse.CRS('+init=epsg:4326'))
})


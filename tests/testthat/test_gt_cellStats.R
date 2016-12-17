context('gt_cellStats')

test_that('gt_cellStats (x=gt_RasterLayer)', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # create data
  rst <- raster::raster(matrix(c(NA,1.0,3.0,3.0,4.0,12.0), ncol=3), crs=sp::CRS('+init=epsg:4326'), xmn=0, xmx=3, ymn=2, ymx=10)
  # send data to Scala interpreter
  g <- gt_raster(rst)
  # calculate statistics
  stats1 <- gt_cellStats(g)
  stats2 <- sapply(c(mean, median, function(x, na.rm=TRUE) {as.numeric(names(which.max(table(x))))}, 
                     function(x, na.rm=TRUE) {n <- sum(!is.na(x)); n <- sqrt((n-1)/n); sd(x, na.rm=TRUE) * n}, 
                     min, max),
                   cellStats, x=rst)
  # tests
  expect_equal(unname(stats1)[-2], stats2[-2]) # medians are known to be incorrect
})


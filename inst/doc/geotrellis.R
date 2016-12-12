## ---- include=FALSE------------------------------------------------------
devtools::load_all()

## ------------------------------------------------------------------------
# load in data using file path
g <- gt_raster(system.file('extdata/test.tif', package='geotrellis'))

# load in data using a RasterLayer object
g <- gt_raster(raster(system.file('extdata/test.tif', package='geotrellis')))

## ------------------------------------------------------------------------
print(g)  # print summary of object
crs(g)    # coordinate reference system
extent(g) # spatial extent of data
res(g)    # resolution
ncell(g)  # number of data cells
ncol(g)   # number of columns
nrow(g)   # number of rows

## ------------------------------------------------------------------------
values(g)    # convert data as a numeric vector
as.matrix(g) # convert data to a matrix
as.raster(g) # convert data to a RasterLayer

## ------------------------------------------------------------------------
# save data to disk in a GeoTiff format
gt_writeRaster(g, tempfile(fileext='.tif')) 

## ------------------------------------------------------------------------
# create new layer with different spatial resolution
l <- gt_raster(raster::disaggregate(as.raster(g), 2, method=''))

# resample data to new resolution
gt_resample(g, l, method='ngb')

## ------------------------------------------------------------------------
gt_projectRaster(g, sp::CRS('+init=epsg:3395'), res=50000, method='ngb')

## ------------------------------------------------------------------------
gt_crop(g, extent(raster::extent(c(0, 3, 2, 6))))

## ------------------------------------------------------------------------
# create mask layer
m <- as.raster(g)
m <- raster::setValues(m, sample(c(1,NA), ncell(m), replace=TRUE))
m <- gt_raster(m)

# run mask analysis
gt_mask(g, m)

## ------------------------------------------------------------------------
gt_cellStats(g)

## ------------------------------------------------------------------------
# make layer with zones
z <- as.raster(g)
z <- raster::setValues(z, sample(1:2, ncell(z), replace=TRUE))
z <- gt_raster(z)

# run zonal statistics
gt_zonal(g, z)

## ------------------------------------------------------------------------
# run benchmark
b <- benchmark(ncell=c(1e+1, 1e+2, 1e+3, 1e+4, 1e+5), times=10L)
# print benchmark
print(b)
# plot benchmark
plot(b)


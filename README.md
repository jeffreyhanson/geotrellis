geotrellis
==========

[![Travis Build Status](https://img.shields.io/travis/jeffreyhanson/geotrellis/master.svg?label=Mac%20OSX%20%26%20Linux)](https://travis-ci.org/jeffreyhanson/geotrellis)
[![AppVeyor Build Status](https://img.shields.io/appveyor/ci/jeffreyhanson/geotrellis/master.svg?label=Windows)](https://ci.appveyor.com/project/jeffreyhanson/geotrellis)
[![Coverage Status](https://codecov.io/github/jeffreyhanson/geotrellis/coverage.svg?branch=master)](https://codecov.io/github/jeffreyhanson/geotrellis?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geotrellis)](https://CRAN.R-project.org/package=geotrellis)


To install the [development version from GitHub](https://github.com/jeffreyhanson/geotrellis), use this R code:

```
if (!require('devtools'))
	install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('jeffreyhanson/geotrellis')
```

Once this package has been installed, you can read through the vignette for a tutorial on how to use it.

[View it here](https://rawgit.com/geotrellis/geotrellis/master/inst/doc/geotrellis.html), or by running this R code:

```
# open vignette in web browser
vignette('geotrellis', package='geotrellis')
```
[![Project Status: Concept - Minimal or no implementation has been done yet.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Travis Build Status](https://img.shields.io/travis/jeffreyhanson/geotrellis/master.svg?label=Linux)](https://travis-ci.org/jeffreyhanson/geotrellis)
[![Coverage Status](https://codecov.io/github/jeffreyhanson/geotrellis/coverage.svg?branch=master)](https://codecov.io/github/jeffreyhanson/geotrellis?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/geotrellis)](https://CRAN.R-project.org/package=geotrellis)

# Leverage [geotrellis](http://geotrellis.io) to read, write, manipulate, and analyze spatial data from within [R](https://cran.r-project.org).

This package was developed as a proof of concept to determine if [geotrellis](http://geotrellis.io) could be leveraged to provide greater performance for processing spatial data sets. **Generally, this package seems to be comparable to the [raster package](https://CRAN.R-project.org/package=raster), but there is little to no benefit in most cases. I would not recommend this package for any real analysis.** 

![Benchmark comparing functions from the "geotrellis" and "raster" R packages.](inst/vign/readme-figure/unnamed-chunk-1-1.png)

## Installation

This package requires Java version 8. Check what version you have installed using the following system command `javac -version`. If you see `1.8.0+` then you're good to go. Otherwise, you need to update Java. Note that this might cause problems with other programs installed on your computer. **Do this at your own risk.**

To update Java on Ubuntu (14.04 LTS) using the following system commands:

```
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 642AC823
sudo apt-get update
sudo apt-get install oracle-java8-set-default
```

To update Java on Mac OSX (Mavericks) using the following system commands:

```
brew update
brew cask install java
```

To install the [development version from GitHub](https://github.com/jeffreyhanson/geotrellis), use this R code:

```
if (!require('devtools'))
  install.packages('devtools', repo='http://cran.rstudio.com', dep=TRUE)
devtools:::install_github('jeffreyhanson/geotrellis')
```

Once this package has been installed, you can read through the vignette for a tutorial on how to use it.

[View it here](https://cdn.rawgit.com/jeffreyhanson/geotrellis/master/inst/doc/geotrellis.html), or by running this code in R:

```
vignette('geotrellis', package='geotrellis')
```


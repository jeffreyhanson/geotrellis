all: clean jar docs readme site vignettes check build

clean:
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*

jar: inst/java/geotrellis.jar

inst/java/geotrellis.jar: inst/java/geotrellis.sbt inst/java/project/build.properties inst/java/project/plugins.sbt
	cd inst/java && sbt assembly
	\mv -f inst/java/target/scala-2.11/geotrellis.jar inst/java/geotrellis.jar
	rm -rf inst/java/project/target
	rm -rf inst/java/project/project
	rm -rf inst/java/target

docs:
	R --slave -e "devtools::document()"

install: jar
	R --slave -e "devtools::install_local('../geotrellis')"

readme:
	rm -rf inst/vign/readme-figure
	cd inst/vign;\
	R --slave -e "knitr::knit('README.Rmd')";
	mv -f inst/vign/README.md ./README.md
	mv -f inst/vign/figure inst/vign/readme-figure
	sed -i 1,5d README.md
	sed -i 's|figure|inst/vign/readme-figure|g' README.md

site: docs readme
	cp -f inst/vign/geotrellis.Rmd vignettes/geotrellis.Rmd
	R --slave -e "devtools::load_all();pkgdown::build_site()"
	rm -rf vignettes/*
	rm -rf inst/doc/*

vignettes:
	cp -f inst/vign/geotrellis.Rmd vignettes/geotrellis.Rmd
	R --slave -e "devtools::load_all();devtools::build_vignettes()"
	rm -rf vignettes/*
	cp -f inst/vign/placeholder.Rmd vignettes/geotrellis.Rmd
	touch inst/doc/geotrellis.*

test: docs
	R --slave -e "devtools::test()"

check:
	R --slave -e "devtools::check()"
	R --slave -e "devtools::build_win()"

build:
	R --slave -e "devtools::build()"

.PHONY: clean jar install docs readme site vignettes test check build 

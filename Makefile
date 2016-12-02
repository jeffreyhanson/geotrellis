all: jar clean docs check build

jar: 
	cd geotrellis && sbt assembly
	\cp -f geotrellis/target/scala-2.11/geotrellis.jar inst/java/geotrellis.jar

clean:
	rm -rf docs/*
	rm -rf inst/doc/*
	rm -rf vignettes/*
	rm -rf inst/geotrellis/project/target
	rm -rf inst/geotrellis/target

docs: 
	R -e "devtools::install_local('../geotrellis')"
	R -e "devtools::load_all()"
	R -e "devtools::document()"
	R -e "staticdocs::build_site()"

test:
	R -e "devtools::test()"

check:
	R -e "devtools::check()"
	R -e "devtools::build_win()"

build:
	R -e "devtools::build()"


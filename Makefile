# This is the start of a makefile, currently still rather incomplete

# inspired by code from https://github.com/yihui/knitr/master/Makefile
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docu install check

docu:
	Rscript -e "require(devtools); devtools::build_vignettes()"
	rm -f inst/doc/$(PKGNAME)_$(PKGVERS).pdf
	# for just the functions, use this
	# R CMD Rd2pdf --title='$(PKGNAME) Package' --no-preview -o inst/doc/$(PKGNAME)_$(PKGVERS).pdf man/*.Rd
	# for the whole package
	 R CMD Rd2pdf --no-preview -o inst/doc/$(PKGNAME)_$(PKGVERS).pdf . 

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran


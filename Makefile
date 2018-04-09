PKG := $(shell head -1 DESCRIPTION | sed 's/Package: //' | cat)
VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)
BINARY := $(PKG)_$(VERSION).tar.gz
MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
R_ARGS := --no-site-file --no-environ --no-save \
	  --no-restore --no-resave-data --no-manual --quiet

all: clean docs data readme build check install

quick: clean 

clean:
	@rm -rf src/*.so src/*.o *tar.gz *Rcheck*

build: 
	R $(R_ARGS) CMD build .  

build-cran:
	R CMD build . --no-resave-data --no-manual
	
check: 
	R CMD check $(BINARY)

check-cran: 
	R CMD check --as-cran $(BINARY)

check-quick $(BINARY):
	R $(R_ARGS) CMD build . 
	R CMD check $(BINARY)

install: $(BINARY)
	R CMD INSTALL --no-multiarch --with-keep.source $(BINARY)

install-quick:
	R CMD INSTALL --no-multiarch --no-docs --no-html \
	  --with-keep.source .

readme: README.Rmd
	R --vanilla --slave -e "rmarkdown::render('README.Rmd')"

docs:
	R --vanilla --slave -e "devtools::document()"

data: $(wildcard data/*.rda)

data/%.rda: data-ext/%.R
	cd data-ext && Rscript $(<F)

data-ext/%.feather: data-ext/%.R
	cd data-ext && Rscript $(<F)

# The `precinct_validity` dataset specifies valid candidate IDs from the
# `candidates` dataset
data/precinct_validity.rda: data/candidates.rda

# We create it with `candidates.R`, which relies on state precinct returns, FEC
# data, and @uscongress project data
data/candidates.rda: data-ext/candidates/candidates.R \
  data-ext/candidates/fec.feather \
  data-ext/candidates/federal-legislators.feather
	cd data-ext/candidates && Rscript $(<F)

# Create example raw precinct data
data/virginia_precincts.rda: data-ext/example-returns/virginia_precincts.R data-ext/example-returns/virginia_precincts.csv 
	cd data-ext/example-returns && Rscript $(<F)

site:
	R --vanilla --slave -e "pkgdown::build_site()"

.PHONY: clean docs data

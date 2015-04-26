R_HOME := $(shell R RHOME)
Rscript := '$(R_HOME)/bin/Rscript' --vanilla -e
R := "${R_HOME}/bin/R"

PKG_VERSION := $(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME := $(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

#DATA_FILES := $(wildcard data/*.rda)
R_FILES := $(wildcard R/*.R)
TEST_FILES := $(wildcard tests/*.R) $(wildcard tests/testthat/*.R)
ALL_SRC_FILES := $(wildcard src/*.cpp) $(wildcard src/*.h) src/Makevars.in
SRC_FILES := $(filter-out src/RcppExports.cpp, $(ALL_SRC_FILES))
HEADER_FILES := $(wildcard src/*.h)
RCPPEXPORTS := src/RcppExports.cpp R/RcppExports.R
ROXYGENFILES := $(wildcard man/*.Rd) NAMESPACE 
PKG_FILES := DESCRIPTION $(ROXYGENFILES) $(R_FILES) $(SRC_FILES) \
	$(HEADER_FILES) $(TEST_FILES) $(RCPPEXPORTS)
OBJECTS := $(wildcard src/*.o) $(wildcard src/*.o-*) $(wildcard src/*.dll) $(wildcard src/*.so) $(wildcard src/*.rds)
CHECKPATH := $(PKG_NAME).Rcheck
CHECKLOG := `cat $(CHECKPATH)/00check.log`

.PHONY: all build check manual install clean compileAttributes

all: 
	install
	
build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	@make roxygen
	$(R) CMD build --resave-data --no-build-vignettes .

roxygen: $(R_FILES)
	$(Rscript) 'library(roxygen2); roxygenize(clean = TRUE)'
	
$(RCPPEXPORTS): compileAttributes
	
compileAttributes: $(SRC_FILES)
	@echo $(SRC_FILES)
	$(Rscript) 'library(Rcpp); Rcpp::compileAttributes()'
	
check: $(PKG_NAME)_$(PKG_VERSION).tar.gz 
	@rm -rf $(CHECKPATH)
	$(R) CMD check --no-manual --no-clean --as-cran $(PKG_NAME)_$(PKG_VERSION).tar.gz

00check.log: check
	@mv $(CHECKPATH)\\00check.log .
	@rm -rf $(CHECKPATH)

manual: $(PKG_NAME)-manual.pdf

$(PKG_NAME)-manual.pdf: $(ROXYGENFILES)
	$(R) CMD Rd2pdf --no-preview -o $(PKG_NAME)-manual.pdf .
	
install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R) CMD INSTALL --byte-compile $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	@rm -f $(OBJECTS)
	@rm -rf $(wildcard *.Rcheck)
	@rm -rf $(wildcard *.cache)
	@rm -f $(wildcard *.tar.gz)
	@rm -f $(wildcard *.pdf)
	@echo '*** PACKAGE CLEANUP COMPLETE ***'

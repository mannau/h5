# h5
[![Build Status](https://travis-ci.org/mannau/h5.svg?branch=master)](https://travis-ci.org/mannau/h5) [![codecov.io](http://codecov.io/github/mannau/h5/coverage.svg?branch=master)](http://codecov.io/github/mannau/h5?branch=master) [![License](https://img.shields.io/badge/license-BSD%202%20clause-blue.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)

**h5** provides an interface to the HDF5 API through S4-objects. It supports fast storage and retrieval of R-objects like vectors, matrices and arrays to binary files in a language independent format. The package can therefore be used as an alternative to R's save/load mechanism. Since h5 is able to access only subsets of stored data it can handle potentially big(ger than memory) data sets.

## Documentation
Further documentation is available at http://h5.readthedocs.org/en/latest.

## Notes
This library is has already been released on [CRAN](http://cran.r-project.org/web/packages/h5/index.html). 

## Install
See http://h5.readthedocs.org/en/latest/install/ for installation notes.

## Usage

Old style:
```python
library(h5)
testmat <- matrix(rnorm(120), ncol = 3)
fname <- "test.h5"
file <- h5file(fname, "a")
group <- createGroup(file, "/testgroup")
dset <- createDataSet(group, "testmat1", testmat)
h5close(dset)
h5close(group)
h5close(file)
```

or a bit easier using subsetting operators:
```python
library(h5)
testmat <- matrix(rnorm(120), ncol = 3)
fname <- "test.h5"
file <- h5file(fname, "a")
file["/testgroup/testmat1"] <- testmat
# extract first 3 rows from file
dset <- file["/testgroup/testmat1"][1:3, ]
h5close(file)
```







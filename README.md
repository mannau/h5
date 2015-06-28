# h5
[![Build Status](https://travis-ci.org/mannau/h5.svg?branch=master)](https://travis-ci.org/mannau/h5) [![License](https://img.shields.io/badge/license-BSD%202%20clause-blue.svg?style=flat)](http://opensource.org/licenses/BSD-2-Clause)

**h5** provides an interface to the HDF5 API through S4-objects. It supports fast storage and retrieval of R-objects like vectors, matrices and arrays to binary files in a language independent format. The package can therefore be used as an alternative to R's save/load mechanism. Since h5 is able to access only subsets of stored data it can handle potentially big(ger than memory) data sets.

## Documentation
Further documentation is available at www.predictingdaemon.com/h5.

## Notes
This library is has already been released on [CRAN](http://cran.r-project.org/web/packages/h5/index.html). 

## Install
Using **devtools** you can easily install the latest development version of **h5** from github using the following commands:

```python
library(devtools)
install_github("mannau/h5")
```
Please note that this version has been tested with the current hdf5 library 1.8.14 (and 1.8.13 for Mac) - you should therefore install the most current hdf5 library including its C++ API for your platform. This package already ships the library for windows operating systems through (h5-libwin)[https://github.com/mannau/h5-libwin]. For MacOSX and Debian-based Linux systems the library files have to be installed using the commands as described below.

### MacOSX
Using MacOSX and Homebrew (http://brew.sh) you can use the following command to install HDF5 library dependencies and headers:
```shell
brew install homebrew/science/hdf5 --enable-cxx
```

### Linux (e.g. Debian, Ubuntu)
With Debian-based Linux systems you can use the following command to install the 
dependencies:
```shell
sudo apt-get install libhdf5-dev
```

For older versions (Debian Squeeze, Ubuntu precise) it is required to install
```shell
sudo apt-get install libhdf5-serial-dev
```

Since **h5** requires the 'new' v18 API version it might be necessary to install
the dependency libhdf5-serial-dev through the 
[ppa:marutter/rrutter](https://launchpad.net/~marutter/+archive/ubuntu/rrutter) 
repository (Ubuntu) or soon directly the **h5** package via 
[cran2deb](http://debian-r.debian.net) (Debian).

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
```
library(h5)
testmat <- matrix(rnorm(120), ncol = 3)
fname <- "test.h5"
file <- h5file(fname, "a")
file["/testgroup/testmat1"] <- testmat
# extract first 3 rows from file
dset <- file["/testgroup/testmat1"][1:3, ]
h5close(file)
```







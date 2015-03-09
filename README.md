# h5
[![Build Status](https://travis-ci.org/mannau/h5.svg?branch=master)](https://travis-ci.org/mannau/h5) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

**h5** provides an interface to the HDF5 API through S4-objects. It supports fast storage and retrieval of R-objects like vectors, matrices and arrays to binary files in a language independent format. The package can therefore be used as an alternative to R's save/load mechanism. Since h5 is able to access only subsets of stored data it can handle potentially big(ger than memory) data sets.

## Notes
This library is currently under development and will be released on CRAN soon.

## Install
Using **devtools** you can easily install the latest development version of **h5** from github using the following commands:

```python
library(devtools)
install_github("mannau/h5")
```
Please note that this version has been tested with the current hdf5 library 1.8.14 (and 1.8.13 for Mac) - you should therefore install the most current hdf5 library including its C++ API for your platform. This package already ships the library for windows operating systems. For MacOSX and Debian-based Linux systems the library files have to be installed using the commands as described below.

### MacOSX
Using MacOSX and Homebrew (http://brew.sh) you can use the following command to install HDF5 library dependencies and headers:
```shell
brew install hdf5 --enable-cxx
```

### Linux (e.g. Debian, Ubuntu)
With Debian-based Linux systems you can use the following command to install the dependencies:
```shell
sudo apt-get install libhdf5-7 libhdf5-dev libhdf5-serial-dev
```

## Usage

```python
library(h5)
testmat <- matrix(rnorm(120), ncol = 3)
fname <- "test.h5"
file <- new( "H5File", fname, "a")
group <- createGroup(file, "/testgroup")
dset <- createDataSet(group, "testmat1", testmat)
h5close(dset)
h5close(group)
h5close(file)
```

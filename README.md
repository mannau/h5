# h5
[![Build Status](https://travis-ci.org/mannau/h5.svg?branch=master)](https://travis-ci.org/mannau/h5) [![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)

**h5** is an R-package to provide an interface to the HDF5 C++ API. It facilitates the way to handle HDF5 files and create files, groups, datasetsets and attributes. 

## Notes
This library is currently under development and will be released on CRAN soon.

## Install
Using **devtools** you can easily install the latest development version of **h5** from github using the following commands:

```python
library(devtools)
install_github("mannau/h5")
```
Please note that this version has been tested with the current hdf5 library 1.8.14 - you should therefore install the most current hdf5 library including its C++ API for your platform. This package already ships the library for windows operating systems. For MacOSX and linux the library files have to installed using the commands as described below.

### MacOSX
With MacOSX and Homebrew (http://brew.sh) you can use the following command to install HDF5 library dependencies and headers:
```shell
brew install hdf5 --enable-cxx
```

### Linux (e.g. Debian, Ubuntu)
With linux (Debian-based) you can use the following command to install the dependencies:
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
closeh5(dset)
closeh5(group)
closeh5(file)
```

## License
**h5** is released under the [GNU General Public License Version 3](http://www.gnu.org/copyleft/gpl.html)

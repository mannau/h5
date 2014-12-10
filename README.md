# h5

**h5** is an R-package to provide a quite low--level interface to the HDF5 C++ API. It therefore facilitates the way to handle HDF5 files and create files, groups, datasetsets and attributes. 

## Notes
This library is currently under development and will be released on CRAN by end of the year (2014).

## Install
Using **devtools** you can easily install the latest development version of **h5** from github using the following commands:

```python
library(devtools)
install_github("mannau/h5")
```

Please note that this version has been tested with the current hdf5 library 1.8.4 - you therefore need to install the most current hdf5 library including its C++ API for your platform.

### MacOSX
Under MacOSX and using Homebrew (http://brew.sh) you can use the following command:
```shell
brew install hdf5 --enable-cxx
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

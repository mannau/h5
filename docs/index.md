# h5 - HDF5 interface for R

**[h5](http://cran.r-project.org/web/packages/h5/index.html)** is an R 
interface to the [HDF5](https://www.hdfgroup.org/HDF5) library under active development. It is available on [Github](https://github.com/mannau/h5) and already released on [CRAN](https://cran.r-project.org/web/packages/h5/index.html) for all major platforms (Windows, OS X, Linux). 

[HDF5](https://www.hdfgroup.org/HDF5/) is an excellent library and data model to 
store huge amounts of data in a binary file format. Supporting most major 
platforms and programming languages it can be used to exchange data files in a 
language independent format. Compared to R's integrated *save()* and *load()* 
functions it also supports access to only parts of the binary data files and can
therefore be used to process data not fitting into memory.

**[h5](http://cran.r-project.org/web/packages/h5/index.html)** utilizes the 
[HDF5 C++ API](https://www.hdfgroup.org/HDF5/doc/cpplus_RM/) through 
**[Rcpp](http://cran.r-project.org/web/packages/Rcpp/index.html)** and S4 classes. 
The package is covered by 200+ test cases with a [coverage](https://codecov.io/github/mannau/h5?branch=master) greater than 80%.



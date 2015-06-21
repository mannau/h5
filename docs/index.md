After quite some work I'm happy to announce the CRAN release of 
**[h5](http://cran.r-project.org/web/packages/h5/index.html)** - an R 
interface to the [HDF5](https://www.hdfgroup.org/HDF5/) library. It utilizes the 
[HDF5 C++ API](https://www.hdfgroup.org/HDF5/doc/cpplus_RM/) through 
**[Rcpp](http://cran.r-project.org/web/packages/Rcpp/index.html)** and S4 classes. 
Although the package is still under heavy development with some 
[issues](https://github.com/mannau/h5/issues) remaining it is already covered by 
200+ test cases and runs on all major platforms (Windows, OS X, Linux). I 
started working on the package after experiencing some speed and usability issues 
with existing packages (e.g. **[rhdf5](http://www.bioconductor.org/packages/release/bioc/html/rhdf5.html)**). 

<!--more-->

[HDF5](https://www.hdfgroup.org/HDF5/) is an excellent library and data model to 
store huge amounts of data in a binary file format. Supporting most major 
platforms and programming languages it can be used to exchange data files in a 
language independent format. Compared to R's integrated *save()* and *load()* 
functions it also supports access to only parts of the binary data files and can
therefore be used to process data not fitting into memory.

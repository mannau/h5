# Installation

**h5** has already been released on [CRAN](https://cran.r-project.org/web/packages/h5/index.html), and can therefore be installed using
```python
install.packages("h5")
```

The most recent development version can be installed from [Github](https://github.com/mannau/h5) using [**devtools**](https://cran.r-project.org/web/packages/devtools/index.html):

```python
library(devtools)
install_github("mannau/h5")
```
Please note that this version has been tested with the current hdf5 library 1.8.14 (and 1.8.13 for OS X) - you should therefore install the most current hdf5 library including its C++ API for your platform. 

## Requirements

### Windows
This package already ships the library for windows operating systems through [h5-libwin](https://github.com/mannau/h5-libwin). No additional requirements need to be installed.


### OS X
Using OS X and [Homebrew](http://brew.sh) you can use the following command to install HDF5 library dependencies and headers:
```shell
brew install homebrew/science/hdf5 --enable-cxx
```

### Linux (e.g. Debian, Ubuntu)
With Debian-based Linux systems you can use the following command to install the dependencies:
```shell
sudo apt-get install libhdf5-dev
```

For older versions (Debian Squeeze, Ubuntu Precise) it is required to install **libhdf5-serial-dev**:
```shell
sudo apt-get install libhdf5-serial-dev
```

Since **h5** requires the 'new' v18 API version which does not seem to be installed on e.g. Precise it might be necessary to install
the dependency libhdf5-serial-dev through the 
[ppa:marutter/rrutter](https://launchpad.net/~marutter/+archive/ubuntu/rrutter) 
repository (Ubuntu) or soon directly the **h5** package via 
[cran2deb](http://debian-r.debian.net) (Debian).
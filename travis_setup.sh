#!/bin/bash

# Brew install removed
#if [ "$TRAVIS_OS_NAME" == "osx" ]; then # use homebrew version
#  brew install --build-from-source hdf5 --enable-cxx
#else # install from source
cd ..
wget "$HDF5_RELEASE_URL/hdf5-${HDF5_VERSION%.*}/hdf5-$HDF5_VERSION/src/hdf5-$HDF5_VERSION.tar.gz"
tar -xzf "hdf5-$HDF5_VERSION.tar.gz"
cd "hdf5-$HDF5_VERSION"
./configure --prefix=/usr/local --enable-cxx
sudo make install
cd ../h5
#fi

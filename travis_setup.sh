#!/bin/bash

if [ "$TRAVIS_OS_NAME" == "osx" ]; then # use homebrew version
  brew install homebrew/science/hdf5
else # install from source
  cd ..
  if [ "$HDF5_VERSION" == "1.10.0-patch1" ]; then
    wget "$HDF5_RELEASE_URL/hdf5-1.10/hdf5-1.10.0-patch1/src/hdf5-1.10.0-patch1.tar.gz"
  else
    wget "$HDF5_RELEASE_URL/hdf5-$HDF5_VERSION/src/hdf5-$HDF5_VERSION.tar.gz"
  fi
  tar -xzf "hdf5-$HDF5_VERSION.tar.gz"
  cd "hdf5-$HDF5_VERSION"
  ./configure --prefix=/usr/local
  sudo make install
  cd ../h5
fi

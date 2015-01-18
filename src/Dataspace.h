#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"

#ifndef __Dataspace_h__
#define __Dataspace_h__
// Dataspace functions
Rcpp::XPtr<H5::DataSpace> GetDataspace(Rcpp::XPtr<H5::DataSet> dataset, Rcpp::NumericVector offset, Rcpp::NumericVector count);
bool CloseDataspace(Rcpp::XPtr<H5::DataSpace> dataspace);
#endif // __Dataspace_h__

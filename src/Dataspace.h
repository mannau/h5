#include <H5Cpp.h>
#include <Rcpp.h>

#ifndef __Dataspace_h__
#define __Dataspace_h__
// Dataspace functions
Rcpp::XPtr<H5::DataSpace> GetDataspace(Rcpp::XPtr<H5::DataSet> dataset);
Rcpp::XPtr<H5::DataSpace> SelectHyperslab(Rcpp::XPtr<H5::DataSpace> dataspace, Rcpp::NumericVector offset, Rcpp::NumericVector count);
Rcpp::XPtr<H5::DataSpace> SelectElem(Rcpp::XPtr<H5::DataSpace> dataspace, Rcpp::NumericMatrix coords);
Rcpp::XPtr<H5::DataSpace> SelectAll(Rcpp::XPtr<H5::DataSpace> dataspace);
bool CloseDataspace(Rcpp::XPtr<H5::DataSpace> dataspace);
#endif // __Dataspace_h__

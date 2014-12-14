#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"

#ifndef __CommonFG_h__
#define __CommonFG_h__
// CommonFG functions
Rcpp::XPtr<H5::DataSet> CreateDataset(Rcpp::XPtr<H5::CommonFG> file, std::string datasetname, char datatype,
    Rcpp::NumericVector dimensions, Rcpp::NumericVector maxshape, int compressionlevel, int size);
Rcpp::XPtr<H5::DataSet> OpenDataset(Rcpp::XPtr<H5::CommonFG> file, std::string datasetname);
#endif // __CommonFG_h__

#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"

#ifndef __Dataset_h__
#define __Dataset_h__
// Dataset functions
Rcpp::XPtr<H5::DataSet> CreateDataset(Rcpp::XPtr<H5::CommonFG> file, std::string datasetname, char datatype,
    Rcpp::NumericVector dimensions, Rcpp::NumericVector maxshape, int compressionlevel, int size);
bool WriteDataset(Rcpp::XPtr<H5::DataSet> dataset, SEXP mat, char datatype);
SEXP ReadDataset(Rcpp::XPtr<H5::DataSet> dataset, char datatype);
#endif // __Dataset_h__

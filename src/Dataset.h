#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"

#ifndef __Dataset_h__
#define __Dataset_h__

#define CHUNKSIZE 1e6
// Dataset functions
bool WriteDataset(Rcpp::XPtr<H5::DataSet> dataset, SEXP mat, char datatype);
SEXP ReadDataset(Rcpp::XPtr<H5::DataSet> dataset, char datatype);
bool CloseDataset(Rcpp::XPtr<H5::DataSet> dataset);
Rcpp::XPtr<H5::DataSet> CreateDataset(Rcpp::XPtr<H5::CommonFG> file, std::string datasetname, char datatype,
    Rcpp::NumericVector dimensions, Rcpp::NumericVector chunksize, Rcpp::NumericVector maxshape, int compressionlevel, int size);
Rcpp::XPtr<H5::DataSet> OpenDataset(Rcpp::XPtr<H5::CommonFG> file, std::string datasetname);
#endif // __Dataset_h__

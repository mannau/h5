#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"

#ifndef __Dataset_h__
#define __Dataset_h__
// Dataset functions
bool WriteDataset(Rcpp::XPtr<H5::DataSet> dataset, SEXP mat, char datatype);
SEXP ReadDataset(Rcpp::XPtr<H5::DataSet> dataset, char datatype);
bool CloseDataset(Rcpp::XPtr<H5::DataSet> dataset);
#endif // __Dataset_h__

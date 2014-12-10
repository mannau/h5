#include <H5Cpp.h>
#include <Rcpp.h>
#include "Helpers.h"


#ifndef __Attribute_h__
#define __Attribute_h__
bool WriteAttribute(Rcpp::XPtr<H5::H5Object> loc, std::string attributename, SEXP mat, char datatype,
    Rcpp::NumericVector dimensions, int size);
#endif // __Dataset_h__


#include <H5Cpp.h>
#include <Rcpp.h>

#ifndef __Helpers_h__
#define __Helpers_h__
// Dataset functions
H5::PredType GetDataType(const char datatype, int size);
char GetTypechar(const H5::DataType &dtype);
void *ConvertBuffer(const SEXP &mat, char datatype, int stsize);
#endif // __Dataset_h__

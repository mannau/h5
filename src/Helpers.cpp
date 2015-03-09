#include "Helpers.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

PredType GetDataType(const char datatype, int size = -1) {
  switch(datatype){
    case 'd': return PredType::NATIVE_DOUBLE;
    case 'i': return PredType::NATIVE_INT32;
    case 'l': return PredType::NATIVE_INT32;
    case 'c': {
     if (size < 0) {
       throw Rcpp::exception("Parameter size has to be defined");
     }
     PredType type = PredType::C_S1;
     type.setSize(size);
     return type;
    }
    default: throw Rcpp::exception("Unknown data type.");
    }
}

char GetTypechar(const DataType &dtype) {
  if(dtype == PredType::NATIVE_DOUBLE) {
    return 'd';
  } else if(dtype == PredType::NATIVE_INT32) {
    return 'i';
  } else if (dtype == PredType::C_S1 || dtype.getClass() == H5T_STRING) {
    return 'c';
  } else {
    throw Rcpp::exception("Datatype unknown.");
  }
  return 'd'; // Never reached
}



void *ConvertBuffer(const SEXP &mat, char datatype, int stsize) {
  switch(datatype){
       case 'd': return REAL(mat);
       case 'i': return INTEGER(mat);
       case 'l': return LOGICAL(mat);
       case 'c': {
         char * strbuf = (char *)R_alloc(LENGTH(mat), stsize);
         int z=0;
         int j;
         for (int i=0; i < LENGTH(mat); i++) {
           int stringlength = LENGTH(STRING_ELT(mat,i));
           int stringsize = (stsize-1);
           for (j=0; (j < stringlength) & (j < stringsize); j++) {
             strbuf[z++] = CHAR(STRING_ELT(mat,i))[j];
           }
           for (; j < stsize; j++) {
             strbuf[z++] = '\0';
           }
         }
         return strbuf;
       }; break;
       default: throw Rcpp::exception("Unknown data type.");
     }
}

/*
hsize_t *ProcessDimensions(const NumericVector &dimensions) {
  int rank = dimensions.length();
  hsize_t dims[rank];
  for(int i = 0; i < rank; i++) {
    dims[i] = dimensions[i];
  }
  return &dims[0];
}

hsize_t ProcessMaxDimensions(const NumericVector &maxshape) {
  hsize_t maxdims[rank];
  for(int i = 0; i < rank; i++) {
    if (R_IsNA(maxshape[i])) {
       maxdims[i] = H5S_UNLIMITED;
    } else {
      maxdims[i] = maxshape[i];
    }
  }
  return maxdims;
}
*/

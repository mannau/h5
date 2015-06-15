#include "Helpers.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

#define CPTR(VAR,CONST) ((VAR)=(CONST),&(VAR))

DataType GetDataType(const char datatype, int size = -1) {
  switch(datatype){
    case 'd': return PredType::NATIVE_DOUBLE;
    case 'i': return PredType::NATIVE_INT32;
    case 'l': {
    	bool val;
    	EnumType boolenumtype = EnumType(sizeof(bool));
    	boolenumtype.insert("FALSE", CPTR(val, FALSE));
    	boolenumtype.insert("TRUE", CPTR(val, TRUE));
    	return boolenumtype;
    }
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
  } else if (dtype == GetDataType('l')) {
	return 'l';
  } else {
    throw Rcpp::exception("Datatype unknown.");
  }
  return 'd'; // Never reached
}

H5S_seloper_t GetOperator(string opstring) {
  if (opstring == "SET") {
    return H5S_SELECT_SET;
  } else if (opstring == "OR") {
	  return H5S_SELECT_OR;
  } else if (opstring == "AND") {
	  return H5S_SELECT_AND;
  } else if (opstring == "XOR") {
	  return H5S_SELECT_XOR;
  } else if (opstring == "NOTB") {
	  return H5S_SELECT_NOTB;
  } else if (opstring == "NOTA") {
	  return H5S_SELECT_NOTA;
  }
  throw Rcpp::exception("Unknown operator.");
}


void *ConvertBuffer(const SEXP &mat, char datatype, int stsize) {
  switch(datatype){
       case 'd': return REAL(mat);
       case 'i': return INTEGER(mat);
       case 'l': {
    	   //int logsize = sizeof(LGLSXP);
           bool *boolbuf = (bool *)R_alloc(LENGTH(mat), sizeof(bool));
           int z=0;
           for (int i = 0; i < LENGTH(mat); i++) {
        	   boolbuf[z++] = LOGICAL(mat)[i];
           }
	   return boolbuf;
       }; break;
       case 'c': {
         char *strbuf = (char *)R_alloc(LENGTH(mat), stsize);
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

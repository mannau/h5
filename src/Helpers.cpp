#include "Helpers.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

#define CPTR(VAR,CONST) ((VAR)=(CONST),&(VAR))

DataType GetDataType(const DTYPE datatype, int size = -1) {
  switch(datatype){
    case T_DOUBLE: return PredType::NATIVE_DOUBLE;
    case T_INTEGER: return PredType::NATIVE_INT32;
    case T_LOGICAL: {
    	bool val;
    	EnumType boolenumtype = EnumType(sizeof(bool));
    	boolenumtype.insert("FALSE", CPTR(val, FALSE));
    	boolenumtype.insert("TRUE", CPTR(val, TRUE));
    	return boolenumtype;
    }
    case T_CHARACTER: {
     if (size < 0) {
       throw Rcpp::exception("Parameter size has to be defined");
     }
     PredType type = PredType::C_S1;
     type.setSize(size);
     return type;
    }
    case T_VLEN_FLOAT: {
      DataType type = PredType::NATIVE_DOUBLE;
	  return VarLenType(&type);
    }
    case T_VLEN_DOUBLE: {
      DataType type = PredType::NATIVE_DOUBLE;
  	  return VarLenType(&type);
    }
    case T_VLEN_INTEGER: {
      DataType type = PredType::NATIVE_INT32;
	  return VarLenType(&type);
    }
    case T_VLEN_LOGICAL: {
      DataType type = GetDataType(T_VLEN_LOGICAL);
  	  return VarLenType(&type);
    }
    default: throw Rcpp::exception("Unknown data type.");
  }
}

struct cmpDataType {
    bool operator()(const hid_t& a, const hid_t& b) const {
        return H5Tequal(a, b);
    }
};

DTYPE GetTypechar(const DataType &dtype) {

	if ( (dtype == PredType::NATIVE_FLOAT) ||
		 (dtype == PredType::NATIVE_DOUBLE) ||
		 (dtype == PredType::NATIVE_INT64) ||
		 (dtype == PredType::NATIVE_UINT32) ||
		 (dtype == PredType::NATIVE_UINT64)) {
	  return T_DOUBLE;
	}

	if( (dtype == PredType::NATIVE_INT) ||
		(dtype == PredType::NATIVE_INT8) ||
		(dtype == PredType::NATIVE_INT16) ||
		(dtype == PredType::NATIVE_INT32) ||
		(dtype == PredType::NATIVE_UINT8) ||
		(dtype == PredType::NATIVE_UINT16) ) {
	  return T_INTEGER;
	}
	if (dtype == PredType::C_S1 || dtype.getClass() == H5T_STRING) {
	  return T_CHARACTER;
	}
	if (dtype == GetDataType(T_LOGICAL)) {
	  return T_LOGICAL;
	}
	if ( (dtype == VarLenType(&PredType::NATIVE_FLOAT)) ||
	     (dtype == VarLenType(&PredType::NATIVE_DOUBLE)) ||
	     (dtype == VarLenType(&PredType::NATIVE_INT64)) ||
		 (dtype == VarLenType(&PredType::NATIVE_UINT32)) ||
		 (dtype == VarLenType(&PredType::NATIVE_UINT64))) {
		return T_VLEN_DOUBLE;
	}
	if ( (dtype == VarLenType(&PredType::NATIVE_INT)) ||
		 (dtype == VarLenType(&PredType::NATIVE_INT8)) ||
		 (dtype == VarLenType(&PredType::NATIVE_INT16)) ||
		 (dtype == VarLenType(&PredType::NATIVE_INT32)) ||
		 (dtype == VarLenType(&PredType::NATIVE_UINT8)) ||
		 (dtype == VarLenType(&PredType::NATIVE_UINT16)) ) {
	  return T_VLEN_INTEGER;
	}

	/*
	if (dtype == GetDataType(T_VLEN_LOGICAL)) {
	  return T_VLEN_LOGICAL;
	} */

	throw Rcpp::exception("Datatype unknown.");
}

DTYPE GetTypechar(char typechar) {
	switch(typechar) {
		case 'd': return T_DOUBLE;
		case 'i': return T_INTEGER;
		case 'l': return T_LOGICAL;
		case 'c': return T_CHARACTER;
		case 'x': return T_VLEN_DOUBLE;
		case 'y': return T_VLEN_INTEGER;
		case 'z': return T_VLEN_LOGICAL;
		default: throw new Exception("Typechar unknown");
	}
}

char GetTypechar(DTYPE typechar) {
	switch(typechar) {
		case T_DOUBLE: return 'd';
		case T_INTEGER: return 'i';
		case T_LOGICAL: return 'l';
		case T_CHARACTER: return 'c';
		case T_VLEN_DOUBLE: return 'x';
		case T_VLEN_INTEGER: return 'y';
		case T_VLEN_LOGICAL: return 'z';
		default: throw new Exception("Typechar unknown");
	}
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


void *ConvertBuffer(const SEXP &mat, DTYPE datatype, int stsize) {
  switch(datatype){
       case T_DOUBLE: return REAL(mat);
       case T_INTEGER: return INTEGER(mat);
       case T_LOGICAL: {
    	   //int logsize = sizeof(LGLSXP);
           bool *boolbuf = (bool *)R_alloc(LENGTH(mat), sizeof(bool));
           int z=0;
           for (int i = 0; i < LENGTH(mat); i++) {
        	   boolbuf[z++] = LOGICAL(mat)[i];
           }
	   return boolbuf;
       }; break;
       case T_CHARACTER: {
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

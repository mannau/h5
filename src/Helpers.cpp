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

struct cmpDataType {
    bool operator()(const hid_t& a, const hid_t& b) const {
        return H5Tequal(a, b);
    }
};

char GetTypechar(const DataType &dtype) {

	// old comparison function
	if(dtype == PredType::NATIVE_DOUBLE) {
	  return 'd';
	}
	if(dtype == PredType::NATIVE_INT32) {
	  return 'i';
	}
	if (dtype == PredType::C_S1 || dtype.getClass() == H5T_STRING) {
	  return 'c';
	}
	if (dtype == GetDataType('l')) {
	  return 'l';
	}

	std::map<hid_t, char, cmpDataType> m;

	m[H5T_STD_I8BE] = 'i';

	m[H5T_STD_I8LE] = 'i';
	m[H5T_STD_I16BE] = 'i';
	m[H5T_STD_I16LE] = 'i';
	m[H5T_STD_I32BE] = 'i';
	m[H5T_STD_I32LE] = 'i';
	m[H5T_STD_I64BE] = 'd';
	m[H5T_STD_I64LE] = 'd';
	m[H5T_STD_U8BE] = 'i';
	m[H5T_STD_U8LE] = 'i';
	m[H5T_STD_U16BE] = 'i';
	m[H5T_STD_U16LE] = 'i';
	m[H5T_STD_U32BE] = 'd';
	m[H5T_STD_U32LE] = 'd';
	m[H5T_STD_U64BE] = 'd';
	m[H5T_STD_U64LE] = 'd';
	//m[H5T_STD_B8BE] = 'l';
	//m[H5T_STD_B8LE] = 'l';
	//m[H5T_STD_B16BE] = 'l';
	//m[H5T_STD_B16LE] = 'l';
	//m[H5T_STD_B32BE] = 'l';
	//m[H5T_STD_B32LE] = 'l';
	//m[H5T_STD_B64BE] = 'l';
	//m[H5T_STD_B64LE] = 'l';
	//m[H5T_STD_REF_OBJ] = 'l';
	//m[H5T_STD_REF_DSETREG] = 'l';

	m[H5T_C_S1] = 'c';
	m[H5T_FORTRAN_S1] = 'c';

	m[H5T_IEEE_F32BE] = 'd';
	m[H5T_IEEE_F32LE] = 'd';
	m[H5T_IEEE_F64BE] = 'd';
	m[H5T_IEEE_F64LE] = 'd';

	m[H5T_UNIX_D32BE] = 'd';
	m[H5T_UNIX_D32LE] = 'd';
	m[H5T_UNIX_D64BE] = 'd';
	m[H5T_UNIX_D64LE] = 'd';

	m[H5T_INTEL_I8] = 'i';
	m[H5T_INTEL_I16] = 'i';
	m[H5T_INTEL_I32] = 'i';
	m[H5T_INTEL_I64] = 'd';
	m[H5T_INTEL_U8] = 'i';
	m[H5T_INTEL_U16] = 'i';
	m[H5T_INTEL_U32] = 'd';
	m[H5T_INTEL_U64] = 'd';
	//m[H5T_INTEL_B8] = 'l';
	//m[H5T_INTEL_B16] = 'l';
	//m[H5T_INTEL_B32] = 'l';
	//m[H5T_INTEL_B64] = 'l';
	m[H5T_INTEL_F32] = 'd';
	m[H5T_INTEL_F64] = 'd';

	m[H5T_ALPHA_I8] = 'i';
	m[H5T_ALPHA_I16] = 'i';
	m[H5T_ALPHA_I32] = 'i';
	m[H5T_ALPHA_I64] = 'd';
	m[H5T_ALPHA_U8] = 'i';
	m[H5T_ALPHA_U16] = 'i';
	m[H5T_ALPHA_U32] = 'd';
	m[H5T_ALPHA_U64] = 'd';
	//m[H5T_ALPHA_B8] = 'l';
	//m[H5T_ALPHA_B16] = 'l';
	//m[H5T_ALPHA_B32] = 'l';
	//m[H5T_ALPHA_B64] = 'l';
	m[H5T_ALPHA_F32] = 'd';
	m[H5T_ALPHA_F64] = 'd';

	m[H5T_MIPS_I8] = 'i';
	m[H5T_MIPS_I16] = 'i';
	m[H5T_MIPS_I32] = 'i';
	m[H5T_MIPS_I64] = 'd';
	m[H5T_MIPS_U8] = 'i';
	m[H5T_MIPS_U16] = 'i';
	m[H5T_MIPS_U32] = 'd';
	m[H5T_MIPS_U64] = 'd';
	//m[H5T_MIPS_B8] = 'l';
	//m[H5T_MIPS_B16] = 'l';
	//m[H5T_MIPS_B32] = 'l';
	//m[H5T_MIPS_B64] = 'l';
	m[H5T_MIPS_F32] = 'l';
	m[H5T_MIPS_F64] = 'l';

	m[H5T_NATIVE_CHAR] = 'c';
	m[H5T_NATIVE_SCHAR] = 'c';
	m[H5T_NATIVE_UCHAR] = 'c';
	m[H5T_NATIVE_SHORT] = 'l';
	m[H5T_NATIVE_USHORT] = 'l';
	m[H5T_NATIVE_INT] = 'i';
	m[H5T_NATIVE_UINT] = 'l';
	m[H5T_NATIVE_LONG] = 'l';
	m[H5T_NATIVE_ULONG] = 'l';
	m[H5T_NATIVE_LLONG] = 'l';
	m[H5T_NATIVE_ULLONG] = 'l';
	m[H5T_NATIVE_FLOAT] = 'l';
	m[H5T_NATIVE_DOUBLE] = 'l';
	m[H5T_NATIVE_LDOUBLE] = 'l';
	//m[H5T_NATIVE_B8] = 'l';
	//m[H5T_NATIVE_B16] = 'l';
	//m[H5T_NATIVE_B32] = 'l';
	//m[H5T_NATIVE_B64] = 'l';
	//m[H5T_NATIVE_OPAQUE] = 'l';
	m[H5T_NATIVE_HSIZE] = 'l';
	m[H5T_NATIVE_HSSIZE] = 'l';
	m[H5T_NATIVE_HERR] = 'i';
	m[H5T_NATIVE_HBOOL] = 'i';

	m[H5T_NATIVE_INT8] = 'i';
	m[H5T_NATIVE_UINT8] = 'i';
	m[H5T_NATIVE_INT16] = 'i';
	m[H5T_NATIVE_UINT16] = 'i';
	m[H5T_NATIVE_INT32] = 'i';
	m[H5T_NATIVE_UINT32] = 'd';
	m[H5T_NATIVE_INT64] = 'd';

	map<hid_t, char>::const_iterator it = m.find(dtype.getId());

	if (it == m.end()) {
		throw Rcpp::exception("Datatype unknown.");
	}
	return it->second;
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

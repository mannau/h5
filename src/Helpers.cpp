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
    	if ( (size_t)size == H5T_VARIABLE ) { // Assume Variable string size
    		StrType datatype(0, H5T_VARIABLE);
    		return datatype;
    	} else {
    		PredType datatype = PredType::C_S1;
    		datatype.setSize(size);
    		return datatype;
    	}
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
       case T_DOUBLE: {
    	   return REAL(mat);
       }
       case T_INTEGER: {
    	   return INTEGER(mat);
       }
       case T_LOGICAL: {
           bool *boolbuf = (bool *)R_alloc(LENGTH(mat), sizeof(bool));
           int z=0;
           for (int i = 0; i < LENGTH(mat); i++) {
        	   boolbuf[z++] = LOGICAL(mat)[i];
           }
           return boolbuf;
       }
       case T_CHARACTER: {
    	 if( (size_t)stsize == H5T_VARIABLE ) { // Assume variable string size
    	   char ** strbuf = (char **)R_alloc(LENGTH(mat), sizeof(char *));
    	   for (int i = 0; i < LENGTH(mat); i++) {
		     strbuf[i] = (char *)CHAR(STRING_ELT(mat, i));
		   }
    	   return strbuf;
         } else {
        	 char *strbuf = (char *)R_alloc(LENGTH(mat), stsize);
				 unsigned int z=0;
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
         }
         break; // Never reached
       }
       case T_VLEN_DOUBLE: {
    	   DataType dtypein = GetDataType(datatype, stsize);
    	   int n = LENGTH(mat);
    	   hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
    	   List listvlen(mat);
    	   for (int i = 0; i < listvlen.length(); i++) {
    		  Rcpp::NumericVector vecvlen(listvlen[i]);
    		  dbuf[i].p = vecvlen.begin();
    		  dbuf[i].len = vecvlen.length();
		   }
    	   return dbuf;
       }
       case T_VLEN_INTEGER: {
    	   DataType dtypein = GetDataType(datatype, stsize);
		   int n = LENGTH(mat);
		   hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
		   List listvlen(mat);
		   for (int i = 0; i < listvlen.length(); i++) {
			  Rcpp::IntegerVector vecvlen(listvlen[i]);
			  dbuf[i].p = vecvlen.begin();
			  dbuf[i].len = vecvlen.length();
		   }
		   return dbuf;
       }
       /* case T_VLEN_LOGICAL:
		   bool ** lvlenbuf = (bool **)R_alloc(LENGTH(mat), sizeof(bool *));
		   for (int i = 0; i < LENGTH(mat); i++) {
			   lvlenbuf[i] = LOGICAL(VECTOR_ELT(mat, i));
		   }
		   return lvlenbuf; */
       default:
    	   throw Rcpp::exception("Unknown data type.");
     }
}

SEXP AllocateRData(DTYPE tchar, NumericVector count) {
	int ndim = count.length();
	SEXP data;

	NumericVector count_rev = clone<NumericVector>(count);
	std::reverse(count_rev.begin(), count_rev.end());



	switch(tchar) {
		case T_DOUBLE:
			if (ndim == 1) {
			data = PROTECT(Rf_allocVector(REALSXP, count[0]));
		  } else if (ndim == 2) {
			data = PROTECT(Rf_allocMatrix(REALSXP, count[1], count[0]));
		  } else { //(ndim > 2)
			data = PROTECT(Rf_allocArray(REALSXP, (IntegerVector)count_rev));
		  }
		  break;
		case T_INTEGER:
		  if (ndim == 1) {
			data = PROTECT(Rf_allocVector(INTSXP, count[0]));
		  } else if (ndim == 2) {
			data = PROTECT(Rf_allocMatrix(INTSXP, count[1], count[0]));
		  } else {//(ndim > 2)
			data = PROTECT(Rf_allocArray(INTSXP, (IntegerVector)count_rev));
		  }
		  break;
		case T_LOGICAL:
			if (ndim == 1) {
			  data = PROTECT(Rf_allocVector(LGLSXP, count[0]));
			} else if (ndim == 2) {
			  data = PROTECT(Rf_allocMatrix(LGLSXP, count[1], count[0]));
			} else {//(ndim > 2)
			  data = PROTECT(Rf_allocArray(LGLSXP, (IntegerVector)count_rev));
			}
			break;
		case T_CHARACTER:
		  if (ndim == 1) {
		    data = PROTECT(Rf_allocVector(STRSXP, count[0]));
		  } else if (ndim == 2) {
		    data = PROTECT(Rf_allocMatrix(STRSXP, count[1], count[0]));
		  } else {//(ndim > 2)
		    data = PROTECT(Rf_allocArray(STRSXP, (IntegerVector)count_rev));
		  }
		  break;
		case T_VLEN_DOUBLE:
		  data = NULL;
		  break;
		case T_VLEN_INTEGER:
		  data = NULL;
		  break;
		default:
		  throw Rcpp::exception("Datatype unknown.");
	}
	return data; // Never reached
}

SEXP ReadRData(DTYPE tchar, SEXP data,
			XPtr<DataSet> dataset,
			XPtr<DataSpace> memspace,
			XPtr<DataSpace> dataspace ) {
	try {
		switch(tchar) {
			case T_DOUBLE:
				dataset->read(REAL(data), PredType::NATIVE_DOUBLE, *memspace, *dataspace);
				break;
			case T_INTEGER:
				dataset->read(INTEGER(data), PredType::NATIVE_INT32, *memspace, *dataspace);
				break;
			case T_LOGICAL: {
				hsize_t n = dataspace->getSelectNpoints();
				bool *boolbuf = (bool *)R_alloc(n, sizeof(bool));
				dataset->read(boolbuf, GetDataType(T_LOGICAL), *memspace, *dataspace);
				for(unsigned int i = 0; i < n; i++) {
				  LOGICAL(data)[i] = boolbuf[i];
				}
				break;
			}
			case T_CHARACTER: {
				hsize_t n = dataspace->getSelectNpoints();
				DataType dtype = dataset->getDataType();
				size_t stsize = dtype.getSize();

				if(!H5Tis_variable_str(dtype.getId())) {
					char *strbuf = (char *)R_alloc(n, stsize);
					dataset->read(strbuf, dtype, *memspace, *dataspace);
					for(unsigned int i = 0; i < n; i++) {
					  SET_STRING_ELT(data, i, Rf_mkChar(strbuf));
					  strbuf += stsize;
					}
				} else { // Assume variable-length string
					//char ** strbuf = new char *[n];
					char ** strbuf = (char **)R_alloc(n, sizeof(char *));
					dataset->read(strbuf, dtype, *memspace, *dataspace);
					for(unsigned int i = 0; i < n; i++) {
					  Rcpp::String readstr(strbuf[i]);
					  SET_STRING_ELT(data, i, readstr.get_sexp());
					}
				}
				dtype.close();
				break;
			}
			case T_VLEN_DOUBLE: {
				 hsize_t n = dataspace->getSelectNpoints();
				 DataType dtypein = GetDataType(tchar, -1);
				 hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
				 dataset->read(dbuf, dtypein, *memspace, *dataspace);

				 vector<vector<double> > datvec;
				 double *ptr = (double *)NULL;
				 for (unsigned int i=0; i < n; i++) {
					 ptr = (double *)dbuf[i].p;
					 vector<double> rowvec(ptr, ptr + dbuf[i].len);
					 datvec.push_back(rowvec);
				 }

				 data = wrap(datvec);
				 memspace->close();
				 return data;
			}
			case T_VLEN_INTEGER: {
				 hsize_t n = dataspace->getSelectNpoints();
				 DataType dtypein = GetDataType(tchar, -1);
				 hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
				 dataset->read(dbuf, dtypein, *memspace, *dataspace);

				 vector<vector<int> > datvec;
				 int *ptr = (int *)NULL;
				 for (unsigned int i=0; i < n; i++) {
					 ptr = (int *)dbuf[i].p;
					 vector<int> rowvec(ptr, ptr + dbuf[i].len);
					 datvec.push_back(rowvec);
				 }

				 data = wrap(datvec);
				 return data;
			}
			default:
				throw Rcpp::exception("Datatype unknown.");
		}
		UNPROTECT(1);
		return data;
  } catch(Exception& error) {
	string msg = error.getDetailMsg() + " in " + error.getFuncName();
	throw Rcpp::exception(msg.c_str());
  }
}

SEXP ReadRDataAttribute(DTYPE tchar, SEXP data,
			XPtr<Attribute> attribute) {
	try {
		switch(tchar) {
			case T_DOUBLE:
				attribute->read(PredType::NATIVE_DOUBLE, REAL(data));
				break;
			case T_INTEGER:
				attribute->read(PredType::NATIVE_INT32, INTEGER(data));
				break;
			case T_LOGICAL: {
				hsize_t n = attribute->getSpace().getSelectNpoints();
				bool *boolbuf = (bool *)R_alloc(n, sizeof(bool));
				attribute->read(GetDataType(T_LOGICAL), boolbuf);
				for(unsigned int i = 0; i < n; i++) {
				  LOGICAL(data)[i] = boolbuf[i];
				}
				break;
			}
			case T_CHARACTER: {
				DataType dtype = attribute->getDataType();
				size_t stsize = dtype.getSize();
				hsize_t n = attribute->getSpace().getSelectNpoints();

				if(!H5Tis_variable_str(dtype.getId())) {
					char *strbuf = (char *)R_alloc(n, stsize);
					attribute->read(dtype, strbuf);
					for(unsigned int i = 0; i < n; i++) {
					  SET_STRING_ELT(data, i, Rf_mkChar(strbuf));
					  strbuf += stsize;
					}
				} else { // Assume variable-length string
					char ** strbuf = new char *[n];
					attribute->read(dtype, strbuf);
					for(unsigned int i = 0; i < n; i++) {
					  Rcpp::String readstr(strbuf[i]);
					  SET_STRING_ELT(data, i, readstr.get_sexp());
					}
					delete [] strbuf;
				}
				break;
			}
			case T_VLEN_DOUBLE: {
				 hsize_t n = attribute->getSpace().getSelectNpoints();
				 DataType dtypein = GetDataType(tchar, -1);
				 hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
				 attribute->read(dtypein, dbuf);

				 vector<vector<double> > datvec;
				 double *ptr = (double *)NULL;
				 for (unsigned int i=0; i < n; i++) {
					 ptr = (double *)dbuf[i].p;
					 vector<double> rowvec(ptr, ptr + dbuf[i].len);
					 datvec.push_back(rowvec);
				 }

				 data = wrap(datvec);
				 delete ptr;
				 return data;
			}
			case T_VLEN_INTEGER: {
				 hsize_t n = attribute->getSpace().getSelectNpoints();
				 DataType dtypein = GetDataType(tchar, -1);
				 hvl_t * dbuf = (hvl_t *)R_alloc(n, dtypein.getSize());
				 attribute->read(dtypein, dbuf);

				 vector<vector<int> > datvec;
				 int *ptr = (int *)NULL;
				 for (unsigned int i=0; i < n; i++) {
					 ptr = (int *)dbuf[i].p;
					 vector<int> rowvec(ptr, ptr + dbuf[i].len);
					 datvec.push_back(rowvec);
				 }

				 data = wrap(datvec);
				 delete ptr;
				 return data;
			}
			default:
				throw Rcpp::exception("Datatype unknown.");
		}

		UNPROTECT(1);
		//dataspace.close();
		return data;
  } catch(Exception& error) {
	string msg = error.getDetailMsg() + " in " + error.getFuncName();
	throw Rcpp::exception(msg.c_str());
  }
}

#include "Attribute.h"

using namespace Rcpp;
using namespace H5;
using namespace std;

// [[Rcpp::export]]
XPtr<Attribute> CreateAttribute_CommonFG(XPtr<CommonFG> loc, string attributename,
		char datatype, NumericVector dimensions, int size) {

	return CreateAttribute_internal(loc->getLocId(), attributename,
			datatype, dimensions, size);
}

// [[Rcpp::export]]
XPtr<Attribute> CreateAttribute_DataSet(XPtr<DataSet> loc, string attributename,
		char datatype, NumericVector dimensions, int size) {

	return CreateAttribute_internal(loc->getId(), attributename,
				datatype, dimensions, size);
}


XPtr<Attribute> CreateAttribute_internal(int id, string attributename,
		char datatype, NumericVector dimensions, int size) {
  try {
	vector<hsize_t> dims(dimensions.begin(), dimensions.end());
	DataSpace dataspace (dimensions.length(), &dims[0]);

	PredType dtype = GetDataType(datatype, size);

	hid_t attrid = H5Acreate(id, attributename.c_str(), dtype.getId(),
			dataspace.getId(), H5P_DEFAULT, H5P_DEFAULT);

	if (attrid == -1) {
		H5Aclose(attrid);
		dataspace.close();
		throw Rcpp::exception("Creation of Attribute failed. Maybe attribute with same name is already existing at location.");
	}
	return XPtr<Attribute>(new Attribute(attrid));
  } catch (Exception& error) {
	  string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool WriteAttribute(XPtr<Attribute> attribute, SEXP mat,
		char datatype, NumericVector count) {
  try {
    size_t stsize = attribute->getDataType().getSize();
    const void *buf = ConvertBuffer(mat, datatype, stsize);
    attribute->write(GetDataType(datatype, stsize), buf);
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
SEXP ReadAttribute(XPtr<Attribute> attribute, NumericVector count) {
  try {
    int ndim = count.length();
    unsigned int nelem = std::accumulate(count.begin(), count.end(), 1,
    		std::multiplies<unsigned int>());
    DataType dtype = attribute->getDataType();
    char tchar = GetTypechar(dtype);

    NumericVector count_rev = clone<NumericVector>(count);
    std::reverse(count_rev.begin(), count_rev.end());

    SEXP data;
    if (tchar == 'd') {
      if (ndim == 1) {
        data = PROTECT(Rf_allocVector(REALSXP, count[0]));
      } else if (ndim == 2) {
        data = PROTECT(Rf_allocMatrix(REALSXP, count[1], count[0]));
      } else {//(ndim > 2)
        data = PROTECT(Rf_allocArray(REALSXP, (IntegerVector)count_rev));
      }
      attribute->read(dtype, REAL(data));
    } else if (tchar == 'i') {
      if (ndim == 1) {
        data = PROTECT(Rf_allocVector(INTSXP, count[0]));
      } else if (ndim == 2) {
        data = PROTECT(Rf_allocMatrix(INTSXP, count[1], count[0]));
      } else {//(ndim > 2)
        data = PROTECT(Rf_allocArray(INTSXP, (IntegerVector)count_rev));
      }
      attribute->read(dtype, INTEGER(data));
    } else if (tchar == 'c') {
       size_t stsize = dtype.getSize();
        if (ndim == 1) {
         data = PROTECT(Rf_allocVector(STRSXP, count[0]));
        } else if (ndim == 2) {
         data = PROTECT(Rf_allocMatrix(STRSXP, count[1], count[0]));
        } else {//(ndim > 2)
         data = PROTECT(Rf_allocArray(STRSXP, (IntegerVector)count_rev));
        }
        char *strbuf = (char *)R_alloc(nelem, stsize);
        attribute->read(dtype, strbuf);
        for(unsigned int i = 0; i < nelem; i++) {
          SET_STRING_ELT(data, i, Rf_mkChar(strbuf));
          strbuf += stsize;
        }
        //delete strbuf; TODO: should R_free be called on strbuf?
    } else {
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

// [[Rcpp::export]]
bool CloseAttribute(XPtr<Attribute> attribute) {
  attribute->close();
  return true;
}

// [[Rcpp::export]]
XPtr<Attribute> OpenAttribute_CommonFG(XPtr<CommonFG> loc, string attributename) {
	return OpenAttribute_internal(loc->getLocId(), attributename);
}

// [[Rcpp::export]]
XPtr<Attribute> OpenAttribute_DataSet(XPtr<DataSet> loc, string attributename) {
	return OpenAttribute_internal(loc->getId(), attributename);
}

XPtr<Attribute> OpenAttribute_internal(int id, string attributename) {
  try {
	hid_t attrid = H5Aopen(id, attributename.c_str(), H5P_DEFAULT);
	if (attrid == -1) {
		H5Aclose(attrid);
		throw Rcpp::exception("Opening Attribute failed.");
	}
	return XPtr<Attribute>(new Attribute(attrid));
  } catch (Exception& error) {
	  string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
char GetAttributeType(XPtr<Attribute> attribute) {
  try {
    DataType dtype = attribute->getDataType();
    return GetTypechar(dtype);
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
NumericVector GetAttributeDimensions(XPtr<Attribute> attribute) {
  DataSpace dataspace = attribute->getSpace();
  int ndim = dataspace.getSimpleExtentNdims();
  vector<hsize_t> dims_out(ndim);
  dataspace.getSimpleExtentDims(&dims_out[0], NULL);
  return NumericVector(dims_out.begin(), dims_out.end());
}


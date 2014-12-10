#include "Attribute.h"

using namespace H5;
using namespace Rcpp;
using namespace std;


// [[Rcpp::export]]
bool WriteAttribute(XPtr<H5Object> loc, string attributename, SEXP mat, char datatype,
    NumericVector dimensions, int size) {
  try {
    int rank = dimensions.length();
     hsize_t dims[rank];
     for(int i = 0; i < rank; i++) {
       dims[i] = dimensions[i];
     }

    //hsize_t dims = ProcessDimensions(dimensions);
    if (loc->attrExists(attributename.c_str())) {
       loc->removeAttr(attributename);
    }
    DataSpace dataspace (dimensions.length(), dims);
    Attribute attribute = loc->createAttribute(attributename, GetDataType(datatype, size), dataspace);
    const void *buf = ConvertBuffer(mat, datatype, size);
    attribute.write(GetDataType(datatype, size), buf);
    attribute.close();
    return TRUE;

  } catch(AttributeIException& error) {
    throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(FileIException& error) {
    throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(DataSetIException& error) {
      throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(DataSpaceIException& error) {
      throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(...) {
    throw Rcpp::exception("c++ exception (unknown reason)");
  }
}

/*
// [[Rcpp::export]]
SEXP ReadAttribute(XPtr<DataSet> dataset, char datatype) {
  try {
    DataSpace dataspace = dataset->getSpace();
    int ndim = dataspace.getSimpleExtentNdims();
    hsize_t dims[ndim];
    dataspace.getSimpleExtentDims(dims, NULL);

    DataType dtype = dataset->getDataType();

    if(datatype == 'd') {
      NumericMatrix out(dims[0], dims[1]);
      dataset->read(out.begin(), dtype, dataspace);
      dataspace.close();
      return out;
    } else if(datatype == 'i') {
      IntegerMatrix out(dims[0], dims[1]);
      dataset->read(out.begin(), dtype, dataspace);
      dataspace.close();
      return out;
    } else if(datatype == 'l') {
      LogicalMatrix out(dims[0], dims[1]);
      dataset->read(out.begin(), dtype, dataspace);
      dataspace.close();
      return out;
    } else if(datatype == 'c') {
      size_t stsize = dtype.getSize();
      int n = dims[0] * dims[1];
      char *strbuf = (char *)R_alloc(n, stsize);
      dataset->read(strbuf, dtype, dataspace);

      CharacterMatrix out(dims[0], dims[1]);
      for (int i = 0; i < n; i++) {
        out[i] = strbuf;
        strbuf += out[i].size() + 1;
      }
      // TODO: should R_free be called on strbuf?
      return out;
    } else {
      throw Rcpp::exception("Unknown datatype.");
    }
  } catch(FileIException& error) {
    throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(DataSetIException& error) {
      throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(DataSpaceIException& error) {
      throw Rcpp::exception(error.getDetailMsg().c_str());
  } catch(Rcpp::exception& error) {
    throw error;
  }
}
*/






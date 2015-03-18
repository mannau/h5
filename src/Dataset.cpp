#include "Dataset.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
bool WriteDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, SEXP mat,
		char datatype, NumericVector count) {
  try {
    vector<hsize_t> count_t(count.begin(), count.end());
    DataSpace *memspace = new DataSpace(count.length(), &count_t[0]);
    size_t stsize = dataset->getDataType().getSize();

    const void *buf = ConvertBuffer(mat, datatype, stsize);
    dataset->write(buf, GetDataType(datatype, stsize), *memspace, *dataspace);
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool ExtendDataset(XPtr<DataSet> dset, NumericVector dimsnew) {
  try {
	vector<hsize_t> dimsnew_t(dimsnew.begin(), dimsnew.end());
    dset->extend(&dimsnew_t[0]);
    return TRUE;
  } catch(Exception& error) {
       string msg = error.getDetailMsg() + " in " + error.getFuncName();
       throw Rcpp::exception(msg.c_str());
  }
  return dset;
}

// [[Rcpp::export]]
char GetDataSetType(XPtr<DataSet> dataset) {
  try {
    DataType dtype = dataset->getDataType();
    return GetTypechar(dtype);
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
SEXP ReadDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, NumericVector count) {
  try {
    int ndim = count.length();
    vector<hsize_t> count_t(count.begin(), count.end());
    DataSpace *memspace = new DataSpace(ndim, &count_t[0]);
    DataType dtype = dataset->getDataType();
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
      dataset->read(REAL(data), dtype, *memspace, *dataspace);
    } else if (tchar == 'i') {
      if (ndim == 1) {
        data = PROTECT(Rf_allocVector(INTSXP, count[0]));
      } else if (ndim == 2) {
        data = PROTECT(Rf_allocMatrix(INTSXP, count[1], count[0]));
      } else {//(ndim > 2)
        data = PROTECT(Rf_allocArray(INTSXP, (IntegerVector)count_rev));
      }
      dataset->read(INTEGER(data), dtype, *memspace, *dataspace);
    } else if (tchar == 'c') {
       size_t stsize = dtype.getSize();
        hsize_t n = dataspace->getSelectNpoints();
        if (ndim == 1) {
         data = PROTECT(Rf_allocVector(STRSXP, count[0]));
        } else if (ndim == 2) {
         data = PROTECT(Rf_allocMatrix(STRSXP, count[1], count[0]));
        } else {//(ndim > 2)
         data = PROTECT(Rf_allocArray(STRSXP, (IntegerVector)count_rev));
        }
        char *strbuf = (char *)R_alloc(n, stsize);
        dataset->read(strbuf, dtype, *memspace, *dataspace);
        for(unsigned int i = 0; i < n; i++) {
          SET_STRING_ELT(data, i, Rf_mkChar(strbuf));
          strbuf += stsize;
        }
        //delete strbuf; TODO: should R_free be called on strbuf?
    } else {
      throw Rcpp::exception("Datatype unknown.");
    }

    UNPROTECT(1);
    memspace->close();
    delete memspace;
    //dataspace.close();
    return data;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool CloseDataset(XPtr<DataSet> dataset) {
  try {
    dataset->close();
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSet> CreateDataset(XPtr<CommonFG> file, string datasetname, char datatype,
    NumericVector dimensions, NumericVector chunksize, NumericVector maxshape, int compressionlevel, int size) {
  try {
    //TODO: expect dimensions.length() == maxshape.length()
	vector<hsize_t> dims(dimensions.begin(), dimensions.end());
    // Set maximum dimensions
	vector<hsize_t> maxdims(maxshape.begin(), maxshape.end());

	int rank = dimensions.length();
	for(int i = 0; i < rank; i++) {
      if (R_IsNA(maxshape[i])) {
         maxdims[i] = H5S_UNLIMITED;
      }
    }

    // Create the data space for the dataset.
    DataSpace dataspace (dimensions.length(), &dims[0], &maxdims[0]);
    // Set chunksize
    vector<hsize_t> chunk_dims(chunksize.begin(), chunksize.end());

    DSetCreatPropList prop;
    prop.setDeflate(compressionlevel);
    // TODO: set chunk dims appropriately
    prop.setChunk(rank, &chunk_dims[0]);
    DataSet dataset = file->createDataSet((H5std_string)datasetname,
        GetDataType(datatype, size), dataspace, prop);

    if (dataset.getId() == -1) {
      dataset.close();
      prop.close();
      dataspace.close();
      throw Rcpp::exception("Creation of DataSet failed. Maybe dataset with same name is already existing at location.");
    }
    prop.close();
    dataspace.close();
    return XPtr<DataSet>(new DataSet(dataset));
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSet> OpenDataset(XPtr<CommonFG> file, string datasetname) {
  try {
    DataSet *dataset = new DataSet(file->openDataSet((H5std_string)datasetname));
    return XPtr<DataSet>(dataset);
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
NumericVector GetDataSetDimensions(XPtr<DataSet> dataset) {
  DataSpace dataspace = dataset->getSpace();
  int ndim = dataspace.getSimpleExtentNdims();
  vector<hsize_t> dims_out(ndim);
  dataspace.getSimpleExtentDims( &dims_out[0], NULL);
  return NumericVector(dims_out.begin(), dims_out.end());
}

// [[Rcpp::export]]
NumericVector GetDataSetMaxDimensions(XPtr<DataSet> dataset) {
  DataSpace dataspace = dataset->getSpace();
  int ndim = dataspace.getSimpleExtentNdims();
  vector<hsize_t> dims_out(ndim);
  vector<hsize_t> maxdims_out(ndim);
  dataspace.getSimpleExtentDims(&dims_out[0], &maxdims_out[0]);
  return NumericVector(maxdims_out.begin(), maxdims_out.end());
}

// [[Rcpp::export]]
NumericVector GetDataSetChunksize(XPtr<DataSet> dataset) {
  DSetCreatPropList cparms = dataset->getCreatePlist();
  if( H5D_CHUNKED == cparms.getLayout()) {
	  DataSpace dataspace = dataset->getSpace();
	  int ndim = dataspace.getSimpleExtentNdims();
	  vector<hsize_t> chunk_dims(ndim);
	  cparms.getChunk( ndim, &chunk_dims[0]);
	  return NumericVector(chunk_dims.begin(), chunk_dims.end());
  }
  return NA_REAL;
}

// [[Rcpp::export]]
CharacterVector GetDataSetCompression(XPtr<DataSet> dataset) {
  DSetCreatPropList cparms = dataset->getCreatePlist();
  int numfilt = cparms.getNfilters();
  size_t nelmts={1}, namelen={1};
  unsigned  flags, filter_info, cd_values[1];
  char name[1];
  H5Z_filter_t filter_type;

  CharacterVector outvec(numfilt);

  for (int i = 0; i < numfilt; i++) {
      nelmts = 0;
      filter_type = cparms.getFilter(i, flags, nelmts, cd_values, namelen, name , filter_info);
      switch (filter_type) {
        case H5Z_FILTER_DEFLATE:
          outvec(i) = "H5Z_FILTER_DEFLATE";
          break;
        case H5Z_FILTER_SZIP:
          outvec(i) = "H5Z_FILTER_SZIP";
          break;
        default:
          outvec(i) = "UNKNOWN";
        }
  }
  return outvec;
}



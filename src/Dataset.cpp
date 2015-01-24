#include "Dataset.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

/*
hsize_t* GetSelectCount(const DataSpace dataspace) {
  int ndim = dataspace.getSimpleExtentNdims();
  hsize_t start_t[ndim];
  hsize_t end_t[ndim];
  dataspace.getSelectBounds(start_t, end_t);
  hsize_t *count_t = new hsize_t[ndim];
  for (int i = 0; i < ndim; i++) {
    count_t[i] = end_t[i] - start_t[i] + 1;
  }
  return count_t;
}*/

// [[Rcpp::export]]
bool WriteDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, SEXP mat, char datatype, NumericVector count) {
  try {
    int ndim = count.length();
    hsize_t count_t[ndim];
    std::copy(count.begin(), count.end(), count_t);
    DataSpace *memspace = new DataSpace(ndim, count_t);
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
    hsize_t dimsnew_t[dimsnew.length()];
    std::copy(dimsnew.begin(), dimsnew.end(), dimsnew_t);

    dset->extend(dimsnew_t);
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
    hsize_t count_t[ndim];
    std::copy(count.begin(), count.end(), count_t);
    DataSpace *memspace = new DataSpace(ndim, count_t);

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
        hsize_t n = dataspace->getSimpleExtentNpoints();
        if (ndim == 1) {
         data = PROTECT(Rf_allocVector(STRSXP, count[0]));
        } else if (ndim == 2) {
         data = PROTECT(Rf_allocMatrix(STRSXP, count[1], count[0]));
        } else {//(ndim > 2)
         data = PROTECT(Rf_allocArray(STRSXP, (IntegerVector)count_rev));
        }
        char *strbuf = (char *)R_alloc(n, stsize);
        dataset->read(strbuf, dtype, *memspace, *dataspace);
        for(int i = 0; i < n; i++) {
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
    // Preprocess paramters
    //hsize_t dims = ProcessDimensions(dimensions);
    //hsize_t maxdims = ProcessMaxDimensions(maxshape);
    int rank = dimensions.length();
    hsize_t dims[rank];
    for(int i = 0; i < rank; i++) {
     dims[i] = dimensions[i];
    }

    // Set maximum dimensions
    hsize_t maxdims[rank];
    for(int i = 0; i < rank; i++) {
      if (R_IsNA(maxshape[i])) {
         maxdims[i] = H5S_UNLIMITED;
      } else {
        maxdims[i] = maxshape[i];
      }
    }

    // Create the data space for the dataset.
    DataSpace dataspace (dimensions.length(), dims, maxdims);
    // Set chunksize
    hsize_t chunk_dims[rank];
    for(int i = 0; i < rank; i++) {
      if (R_IsNA(chunksize[i])) {
        chunk_dims[i] = CHUNKSIZE;
      } else {
        chunk_dims[i] = chunksize[i];
      }
    }
    DSetCreatPropList prop;
    prop.setDeflate(compressionlevel);
    // TODO: set chunk dims appropriately
    prop.setChunk(rank, chunk_dims);
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
  hsize_t dims_out[ndim];
  dataspace.getSimpleExtentDims( dims_out, NULL);
  return NumericVector(dims_out, dims_out + sizeof dims_out / sizeof dims_out[0]);
}

// [[Rcpp::export]]
NumericVector GetDataSetMaxDimensions(XPtr<DataSet> dataset) {
  DataSpace dataspace = dataset->getSpace();
  int ndim = dataspace.getSimpleExtentNdims();
  hsize_t dims_out[ndim];
  hsize_t maxdims_out[ndim];
  dataspace.getSimpleExtentDims( dims_out, maxdims_out);
  return NumericVector(maxdims_out, maxdims_out + sizeof maxdims_out / sizeof maxdims_out[0]);
}

// [[Rcpp::export]]
NumericVector GetDataSetChunksize(XPtr<DataSet> dataset) {
  DSetCreatPropList cparms = dataset->getCreatePlist();
  DataSpace dataspace = dataset->getSpace();
  int ndim = dataspace.getSimpleExtentNdims();
  hsize_t chunk_dims[ndim];
  int rank_chunk;
  if( H5D_CHUNKED == cparms.getLayout()) {
    rank_chunk = cparms.getChunk(ndim, chunk_dims);
    return NumericVector(chunk_dims, chunk_dims + sizeof chunk_dims / sizeof chunk_dims[0]);
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



#include "Dataset.h"
#include "Helpers.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
bool WriteDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, SEXP mat,
		char datatype, NumericVector count) {
  try {
    vector<hsize_t> count_t(count.begin(), count.end());
    DataSpace *memspace = new DataSpace(count.length(), &count_t[0]);

    size_t stsize = -1;
    DataType dsettype = dataset->getDataType();
    if (!H5Tis_variable_str(dsettype.getId())) {
    	stsize = dsettype.getSize();
    }
    DTYPE type = GetTypechar(datatype);
    const void *buf = ConvertBuffer(mat, type, stsize);
    DataType dtype = GetDataType(type, stsize);
    dataset->write(buf, dtype, *memspace, *dataspace);
    delete memspace;
    dtype.close();
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
    DTYPE type = GetTypechar(dtype);
    char tchar = GetTypechar(type);
    dtype.close();
    return tchar;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
SEXP ReadDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, NumericVector count) {
  try {
    vector<hsize_t> count_t(count.begin(), count.end());
    Rcpp::XPtr<DataSpace> memspace(new DataSpace(count.length(), &count_t[0]));
    DataType dtype = dataset->getDataType();
    DTYPE tchar = GetTypechar(dtype);

    SEXP data = AllocateRData(tchar, count);
	data = ReadRData(tchar, data, dataset, memspace, dataspace);
	memspace->close();
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

    DSetCreatPropList prop;
    DataSpace dataspace(dimensions.length(), &dims[0]);

    if (!R_IsNA(chunksize[0])) {
    	for(int i = 0; i < rank; i++) {
		  if (R_IsNA(maxshape[i])) {
			 maxdims[i] = H5S_UNLIMITED;
		  }
		}
		// Create the data space for the dataset.
		dataspace.setExtentSimple(dimensions.length(), &dims[0], &maxdims[0]);
    	vector<hsize_t> chunk_dims(chunksize.begin(), chunksize.end());
    	prop.setChunk(rank, &chunk_dims[0]);
    	prop.setDeflate(compressionlevel);
    }

    if(size > 0) { // Adjust for null-termination character
	  size += 1;
	}
    DataType dsettype = GetDataType(GetTypechar(datatype), size);

    DataSet dataset = file->createDataSet(datasetname.c_str(),
    		dsettype, dataspace, prop);

    if (dataset.getId() == -1) {
      dataset.close();
      prop.close();
      dataspace.close();
      throw Rcpp::exception("Creation of DataSet failed.");
    }
    dsettype.close();
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
    DataSet *dataset = new DataSet(file->openDataSet(datasetname.c_str()));
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
  return NumericVector::create(NA_REAL);
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



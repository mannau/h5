#include "CommonFG.h"
#define CHUNKSIZE 1e6

using namespace H5;
using namespace Rcpp;
using namespace std;

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
    DataSet *dataset = new DataSet(file->createDataSet((H5std_string)datasetname, GetDataType(datatype, size), dataspace, prop));
    prop.close();
    dataspace.close();
    return XPtr<DataSet>(dataset);
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


// [[Rcpp::export]]
XPtr<DataSet> OpenDataset(XPtr<CommonFG> file, string datasetname) {
  try {
    DataSet *dataset = new DataSet(file->openDataSet((H5std_string)datasetname));
    return XPtr<DataSet>(dataset);
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




#include "Dataspace.h"
#include "Dataset.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
XPtr<DataSpace> GetDataspace(XPtr<DataSet> dataset, NumericVector offset, NumericVector count) {
  try {
    DataSpace *dataspace = new DataSpace(dataset->getSpace());
    bool minoffset = is_true(all(offset == 0.0));
    bool maxcount = is_true(all(count == GetDataSetDimensions(dataset)));
    bool hyperslab = !(minoffset && maxcount);

    // if hyperslab is selected
    if (hyperslab) {
     hsize_t count_t[count.length()];
     std::copy(count.begin(), count.end(), count_t);

     hsize_t offset_t[offset.length()];
     std::copy(offset.begin(), offset.end(), offset_t);

     dataspace->selectHyperslab(H5S_SELECT_SET, count_t, offset_t);
    }
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
      string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSpace> GetDataspaceElem(XPtr<DataSet> dataset, NumericMatrix coords) {
  try {
    DataSpace *dataspace = new DataSpace(dataset->getSpace());

    hsize_t coords_t[coords.length()];
    std::copy(coords.begin(), coords.end(), coords_t);

    dataspace->selectElements(H5S_SELECT_SET, coords.ncol(), coords_t);
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
      string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSpace> GetDataspaceAll(XPtr<DataSet> dataset) {
  try {
    DataSpace *dataspace = new DataSpace(dataset->getSpace());
    dataspace->selectAll();
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
      string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool CloseDataspace(XPtr<DataSpace> dataspace) {
  try {
    dataspace->close();
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

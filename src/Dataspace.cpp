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
      vector<hsize_t> count_t(count.begin(), count.end());
      vector<hsize_t> offset_t(offset.begin(), offset.end());
      dataspace->selectHyperslab(H5S_SELECT_SET, &count_t[0], &offset_t[0]);
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
    vector<hsize_t> coords_t(coords.begin(), coords.end());
    dataspace->selectElements(H5S_SELECT_SET, coords.ncol(), &coords_t[0]);
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

#include "Dataspace.h"
#include "Dataset.h"
#include "Helpers.h"

using namespace H5;
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
XPtr<DataSpace> GetDataspace(XPtr<DataSet> dataset) {
  try {
    DataSpace *dataspace = new DataSpace(dataset->getSpace());
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
      string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSpace> SelectHyperslab(XPtr<DataSpace> dataspace, NumericVector offset,
		NumericVector count, string seloper = "SET") {
  try {
	vector<hsize_t> count_t(count.begin(), count.end());
	vector<hsize_t> offset_t(offset.begin(), offset.end());
	dataspace->selectHyperslab(GetOperator(seloper), &count_t[0], &offset_t[0]);
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
      string msg = error.getDetailMsg() + " in " + error.getFuncName();
      throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSpace> SelectElem(XPtr<DataSpace> dataspace, NumericMatrix coords) {
  try {
    vector<hsize_t> coords_t(coords.begin(), coords.end());
    dataspace->selectElements(H5S_SELECT_SET, coords.ncol(), &coords_t[0]);
    return XPtr<DataSpace>(dataspace);
  } catch(Exception& error) {
	string msg = error.getDetailMsg() + " in " + error.getFuncName();
	throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<DataSpace> SelectAll(XPtr<DataSpace> dataspace) {
  try {
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

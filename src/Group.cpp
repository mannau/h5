#include "Group.h"

using namespace Rcpp;
using namespace H5;
using namespace std;

// [[Rcpp::export]]
XPtr<Group> CreateGroup(XPtr<CommonFG> file, string groupname) {
  try {
    hid_t group_id = H5Gcreate(file->getLocId(), groupname.c_str(), 0, 0, 0);
    if (group_id < 0) {
      throw Exception("createGroup", "H5Gcreate failed");
    }
    Group* group = new Group(group_id);
    return XPtr<Group>(group);
  } catch (Exception& error) {
     string msg = error.getDetailMsg() + " in " + error.getFuncName();
     throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
XPtr<Group> OpenGroup(XPtr<CommonFG> file, string groupname) {
  try {
    hid_t group_id = H5Gopen(file->getLocId(), groupname.c_str(), H5P_DEFAULT);
    if (group_id < 0) {
      throw Exception("openGroup", "H5Gopen failed");
    }
    Group* group = new Group( group_id );
    return XPtr<Group>(group);
  } catch (Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
 }
}

// [[Rcpp::export]]
bool CloseGroup(XPtr<Group> group) {
  group->close();
  return true;
}

// [[Rcpp::export]]
bool ExistsGroup(XPtr<CommonFG> file, string groupname) {
  try {
     hid_t group_id = H5Gopen(file->getLocId(), groupname.c_str(), H5P_DEFAULT);
     if (group_id < 0) {
       return false;
     }
     return true;
   } catch (Exception& error) {
     string msg = error.getDetailMsg() + " in " + error.getFuncName();
     throw Rcpp::exception(msg.c_str());
  }
}

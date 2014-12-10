#include "Group.h"

using namespace Rcpp;
using namespace H5;
using namespace std;

// [[Rcpp::export]]
XPtr<Group> CreateGroup(XPtr<CommonFG> file, string groupname) {
  Group* group = new Group( file->createGroup((H5std_string)groupname));
  return XPtr<Group>(group);
}

// [[Rcpp::export]]
XPtr<Group> OpenGroup(XPtr<CommonFG> file, string groupname) {
  Group* group = new Group(file->openGroup((H5std_string)groupname));
  return XPtr<Group>(group);
}

// [[Rcpp::export]]
bool CloseGroup(XPtr<Group> group) {
  group->close();
  return TRUE;
}

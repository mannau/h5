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
     H5Gclose(group_id);
     return true;
   } catch (Exception& error) {
     string msg = error.getDetailMsg() + " in " + error.getFuncName();
     throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
void GetFGInfo(XPtr<CommonFG> file, string path) {
	//Rcout << path << endl;
	file->iterateElems(path, NULL, file_info, NULL);
}

herr_t file_info(hid_t loc_id, const char *name, void *opdata)
{
    H5G_stat_t statbuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library. Some magic :-)
     */
    H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
    switch (statbuf.type) {
    case H5G_GROUP:
    	Rprintf("  + %s\n", name);
         break;
    case H5G_DATASET:
    	Rprintf("  D %s\n", name);
         break;
    case H5G_TYPE:
    	Rprintf("  T %s\n", name);
         break;
    default:
    	Rprintf("<ERROR: Unable to identify an object>\n");
    }
    return 0;
 }

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
CharacterVector GetGroupNames(XPtr<CommonFG> file, string path, bool recursive) {
	try {
		CharacterVector(out);
		if(recursive) {
			H5Giterate(file->getLocId(), path.c_str(), NULL, group_info_recursive, &out);
		} else{
			H5Giterate(file->getLocId(), path.c_str(), NULL, group_info, &out);
		}
		return out;
	} catch (Exception& error) {
	     string msg = error.getDetailMsg() + " in " + error.getFuncName();
	     throw Rcpp::exception(msg.c_str());
	}
}

herr_t group_info(hid_t loc_id, const char *name, void *opdata) {
	try {
		H5G_stat_t statbuf;
		H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
		if (statbuf.type == H5G_GROUP) {
			((CharacterVector *) opdata)->push_back(name);
		}
		return 0;
	 } catch (Exception& error) {
		 return 1;
	 }
}

herr_t group_info_recursive(hid_t loc_id, const char *name, void *opdata) {
	try {
		H5G_stat_t statbuf;
		H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
		if (statbuf.type == H5G_GROUP) {
			((CharacterVector *) opdata)->push_back(name);
			H5Giterate(loc_id, name, NULL, group_info_recursive, opdata);
		}
		return 0;
	 } catch (Exception& error) {
		 return 1;
	 }
}

// [[Rcpp::export]]
CharacterVector GetDataSetNames(XPtr<CommonFG> file, string path) {
	try {
		CharacterVector(out);
		H5Giterate(file->getLocId(), path.c_str(), NULL, dset_info, &out);
		return out;
	} catch (Exception& error) {
		 string msg = error.getDetailMsg() + " in " + error.getFuncName();
		 throw Rcpp::exception(msg.c_str());
	}
}

herr_t dset_info(hid_t loc_id, const char *name, void *opdata) {
	try {
		H5G_stat_t statbuf;
		H5Gget_objinfo(loc_id, name, FALSE, &statbuf);
		if (statbuf.type == H5G_DATASET) {
			((CharacterVector *) opdata)->push_back(name);
		}
		return 0;
	 } catch (Exception& error) {
		 return 1;
	 }
}

// [[Rcpp::export]]
bool Unlink(XPtr<CommonFG> file, string path) {
  try {
	  file->unlink(path.c_str());
  } catch (Exception& error) {
    Function warning("warning");
    warning(error.getDetailMsg());
    return false;
  }
  return true;
}

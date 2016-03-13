#include <H5Cpp.h>
#include <Rcpp.h>

#ifndef __Group_h__
#define __Group_h__
// File functions
Rcpp::XPtr<H5::Group> CreateGroup(Rcpp::XPtr<H5::CommonFG> file, std::string groupname);
Rcpp::XPtr<H5::Group> OpenGroup(Rcpp::XPtr<H5::CommonFG> file, std::string groupname);
bool CloseGroup(Rcpp::XPtr<H5::Group> group);
bool ExistsGroup(Rcpp::XPtr<H5::CommonFG> file, std::string groupname);
herr_t file_info(hid_t loc_id, const char *name, void *opdata);
Rcpp::CharacterVector GetGroupNames(Rcpp::XPtr<H5::CommonFG> file, std::string path);
herr_t group_info(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);
Rcpp::CharacterVector GetDataSetNames(Rcpp::XPtr<H5::CommonFG> file, std::string path);
herr_t dset_info(hid_t loc_id, const char *name, void *opdata);
bool Unlink(Rcpp::XPtr<H5::CommonFG> file, std::string path);
#endif // __File_h__

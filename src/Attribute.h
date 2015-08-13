#include <H5Cpp.h>
#include <Rcpp.h>

#ifndef __Attribute_h__
#define __Attribute_h__
// Attribute functions
Rcpp::XPtr<H5::Attribute> CreateAttribute(Rcpp::XPtr<H5::CommonFG> loc, std::string attributename,
		char datatype, Rcpp::NumericVector dimensions, int size);

Rcpp::XPtr<H5::Attribute> CreateAttribute(Rcpp::XPtr<H5::DataSet> loc, std::string attributename,
		char datatype, Rcpp::NumericVector dimensions, int size);

Rcpp::XPtr<H5::Attribute> CreateAttribute_internal(int id, std::string attributename, char datatype,
		Rcpp::NumericVector dimensions, int size);

Rcpp::XPtr<H5::Attribute> OpenAttribute_CommonFG(Rcpp::XPtr<H5::CommonFG> loc,
		std::string attributename);
Rcpp::XPtr<H5::Attribute> OpenAttribute_DataSet(Rcpp::XPtr<H5::DataSet> loc, std::string attributename);
Rcpp::XPtr<H5::Attribute> OpenAttribute_internal(int id, std::string attributename);

char GetAttributeType(Rcpp::XPtr<H5::Attribute> attribute);
Rcpp::NumericVector GetAttributeDimensions(Rcpp::XPtr<H5::Attribute> attribute);

Rcpp::CharacterVector GetAttributeNames_CommonFG(Rcpp::XPtr<H5::CommonFG> file, std::string path);
Rcpp::CharacterVector GetAttributeNames_DataSet(Rcpp::XPtr<H5::DataSet> file, std::string path);
herr_t attr_info(hid_t loc_id, const char *name, const H5A_info_t *ainfo, void *opdata);

#endif // __Attribute_h__

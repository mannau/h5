#include <H5Cpp.h>
#include <Rcpp.h>

#ifndef __File_h__
#define __File_h__
// File functions
bool file_exist (const std::string& name);
Rcpp::XPtr<H5::H5File> OpenFile(std::string filePath, std::string mode);
bool CloseFile(Rcpp::XPtr<H5::H5File> file);
#endif // __File_h__

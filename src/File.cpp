#include "File.h"

using namespace H5;
using namespace Rcpp;
using namespace std;


bool file_exist (const std::string& name) {
    if (FILE *file = fopen(name.c_str(), "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }
}

// [[Rcpp::export]]
XPtr<H5File> OpenFile(string filePath, string mode) {
  try {
    Exception::dontPrint();
    map<std::string, unsigned int> filemodes;
    filemodes["r"] = H5F_ACC_RDONLY;
    filemodes["r+"] = H5F_ACC_RDWR;
    filemodes["w"] = H5F_ACC_TRUNC;
    filemodes["w-"] = H5F_ACC_EXCL;

    bool fileexists = file_exist(filePath);
    if(fileexists) {
      filemodes["a"] = H5F_ACC_RDWR;
    } else {
      filemodes["a"] = H5F_ACC_EXCL;
    }

    if (filemodes.find(mode) == filemodes.end()) {
      throw Rcpp::exception("Given file mode not found");
    }

    H5File *file = new H5File((H5std_string)filePath, filemodes[mode]);
    return XPtr<H5File>(file);
} catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}


// [[Rcpp::export]]
bool CloseFile(XPtr<H5File> file) {
  Function warning("warning");
  try {
    file->flush(H5F_SCOPE_LOCAL);
  } catch (Exception& error) {
    Function warning("warning");
    warning(error.getDetailMsg());
  }
  try{
    file->close();
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool FlushFile(XPtr<H5File> file) {
  try {
    file->flush(H5F_SCOPE_LOCAL);
  } catch (Exception& error) {
    Function warning("warning");
    warning(error.getDetailMsg());
    return FALSE;
  }
  return TRUE;
}

// [[Rcpp::export]]
bool IsHDF5File(string fname) {
  try {
	  return H5Fis_hdf5(fname.c_str());
  } catch (Exception& error) {
    Function warning("warning");
    warning(error.getDetailMsg());
    return false;
  }
  return true;
}



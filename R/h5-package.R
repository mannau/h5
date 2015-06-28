#' H5 - Interface to the HDF5 API
#' 
#' \pkg{h5} provides an interface to the HDF5 API through
#' S4-classes. HDF5 is a binary data format designed for flexible and 
#' efficient I/O, high--volume and complex data. An HDF5 file 
#' can be structured in a hierarchical way to store data sets in groups---quite 
#' similar to the folder structure in a file system. It supports fast storage and 
#' retrieval of R-objects like vectors, matrices and arrays to binary files in a 
#' language independent format (currently no data.frames). The package can 
#' therefore be used as an alternative to R's save/load mechanism. Since
#' h5 is able to access only subsets of stored data it can also handle data
#' sets which do not fit into memory.
#' 
#' \pkg{h5} can currently only handle homogeneous data sets consisting of 
#' one single data type like \code{numeric}, \code{integer}, \code{character} or 
#' \code{logical}. The creation of metadata through attributes is also supported.
#' 
#' The following objects are supported by \pkg{h5} and represented through S4 
#' classes:
#' \describe{
#'   \item{\link{H5File}}{holds the pointer to the binary HDF5 file which can 
#'     include various \code{DataSets} in a hierarchical structure defined by 
#'     \code{H5Groups}.}
#'   \item{\link{H5Group}}{can hold various HDF5 objects like \code{DataSets} 
#'     and other \code{H5Groups}.}
#'   \item{\link{DataSet}}{stores homogeneous data like vectors, matrices and 
#'     arrays.}
#'   \item{\link{Attribute}}{stores metadata about other HDF5 
#'     objects like \code{H5Group}, \code{H5File} and \code{DataSet}.}
#'   \item{\link{DataSpace}}{Objects defining selections on specified 
#'     \code{DataSets}.}
#' }
#' 
#' These classes share common functionality through the following base classes:
#' \describe{
#'   \item{\link{CommonFG}}{implements common functionality for 
#' \code{H5File} and \code{H5Group} to create/access 
#' sub--\code{H5Group}s and \code{DataSet}s.}
#'   \item{\link{H5Location}}{is the base class of \code{H5File}, 
#' \code{H5Group} and \code{DataSet} and implements functions for
#' \code{Attribute} creation and retrieval.}
#' }
#' 
#' The example below shows some typical use cases handling data with HDF5:
#' \enumerate{
#'   \item{Create/Open HDF5 File using \code{\link{H5File}}, specifying file 
#'     access mode.}
#'   \item{Create/Open Groups and DataSets either implicitly using subsetting 
#'     operators or explicitly using the S4--methods like 
#'     \code{\link{createGroup}/\link{openGroup}} or 
#'       \code{\link{createDataSet}/\link{openDataSet}}, see also 
#'     \link{CommonFG}, \link{CommonFG-Group} and 
#'     \link{CommonFG-DataSet}.}
#'   \item{Create/Open meta data for HDF5 objects using e.g.
#'     \code{\link{h5attr}}, see also \link{H5Location-Attribute} and 
#'     \link{Attribute}.}
#'   \item{Retrieve data from \code{\link{DataSet}s} either implicitly using subsetting 
#'     operators or explicitly with \code{\link{readDataSet}} which
#'     requires a \code{\link{DataSpace}} object to specify the selection area, see 
#'     also \link{DataSet}, \link{DataSet-Subset} and \link{DataSpace}.}
#'   \item{Extend \code{DataSet} using predefined functions like \code{c} for 1-dim.
#'     vectors or \code{rbind}/\code{cbind} 2-dimensional \code{DataSets}, see also 
#'     \link{DataSet-Extend}.}
#'   \item{Close \code{H5File}--, \code{H5Group}-- \code{DataSet}-- or 
#'     \code{DataSpace} objects using \code{\link{h5close}}.}
#' }
#' 
#' @examples
#' # 1. Create/Open file 'test.h5' (mode set to 'a'ppend)
#' file <- h5file("test.h5", 'a')
#' 
#' # 2. Store character vector in group '/test' and dataset 'testvec'
#' file["test/testvec"] <- LETTERS[1:9]
#' # Store integer matrix in group '/test/testmat' and dataset 'testmat'
#' mat <- matrix(1:9, nrow = 3)
#' rownames(mat) <- LETTERS[1:3]
#' colnames(mat) <- c("A", "BE", "BUU")
#' file["test/testmat/testmat"] <- mat
#' # Store numeric array in group '/test' and dataset 'testarray'
#' file["test/testarray"] <- array(as.numeric(1:45), dim = c(3, 3, 5))
#' 
#' # 3. Store rownames and column names of matrix as attributes
#' # Get created data set as object
#' dset <- file["test/testmat/testmat"]
#' # Store rownames in attribute 'dimnames_1'
#' h5attr(dset, "dimnames_1") <- rownames(mat)
#' # Store columnnames in attribute 'dimnames_2'
#' h5attr(dset, "dimnames_2") <- colnames(mat)
#' 
#' # 4. Read first 3 elements of testvec
#' testvec <- file["test/testvec"]
#' testvec[1:3]
#' # Read first 2 rows of testmat
#' testmat <- file["test/testmat/testmat"]
#' res <- testmat[1:2, ]
#' # attach rownames and columnnames
#' rownames(res) <- attr(testmat, "rownames")[1:2]
#' colnames(res) <- attr(testmat, "colnames")
#' 
#' # 5. Extend testvec 
#' testvec <- c(testvec, LETTERS[10:26])
#' # Retrieve entire testvec
#' testvec[]
#' 
#' # 6. Close open handles
#' h5close(testvec)
#' h5close(testmat)
#' h5close(file)
#' @docType package
#' @name h5-package
#' @useDynLib h5
#' @importFrom Rcpp sourceCpp
#' @importFrom methods setClass setGeneric setMethod
NULL

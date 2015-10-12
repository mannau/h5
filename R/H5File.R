#' HDF5 File Objects
#' 
#' *H5File* objects are are the main entry point to access HDF5 data from binary 
#' files. The *H5File* S4 class directly maps 
#' \href{H5File}{https://www.hdfgroup.org/HDF5/doc/cpplus_RM/class_h5_1_1_h5_file.html} 
#' objects from the C++ API to R. Through the implemented class hierarchy it 
#' shares common functionality with *H5Group*.
#' 
#' HDF5 files can be opened or generated using the \code{h5file()} function and
#' a specified file access mode. \code{h5file()} returns a \code{H5File} object
#' which can be used to access \code{\link{H5Group}}s and \code{\link{DataSet}}s 
#' using subsetting parameters or according class methods.
#' 
#' HDF5 files which have been created or opened through \code{h5file()} need 
#' to be closed afterwards using \code{h5close()}. 
#' 
#' \code{h5flush()} can be used to flush unwritten data to an HDF5 file. 
#' 
#' HDF5 Files can contain the following objects:
#' \describe{
#'   \item{Groups}{Similar to a file system folder, used to organize HDF5 
#' objects in a hierarchical way.}
#'   \item{Datasets}{Objects to store actual data.}
#'   \item{Attributes}{Meta data objects to store extra informatino about Files, 
#' Groups and Datasets.}
#' }
#' 
#' @section Reading and Writing Files:
#' HDF5 files can be created and accessed using \code{h5file()}:
#' \code{file <- h5file(name = "test.h5", mode = "a")}
#' 
#' The following access-modes are defined:
#' 
#' \tabular{cl}{
#'   \strong{Mode} \tab \strong{Description}\cr
#'   \strong{a}    \tab Read/write if exists, create otherwise (default).\cr
#'   \strong{r}    \tab Read only, file must exist.\cr
#'   \strong{r+}   \tab Read/write, file must exist.\cr
#'   \strong{w}    \tab Create file, truncate if exists.\cr
#'   \strong{w-}   \tab Create file, fail if exists.
#' }
#' 
#' @section Show File Contents:
#' HDF5 objects stored in a file are shown with the following symbols:
#' \tabular{cl}{
#'   \strong{Mode} \tab \strong{Description}\cr
#'   \strong{+}    \tab HDF5 Group.\cr
#'   \strong{D}    \tab HDF5 Dataset.\cr
#'   \strong{A}    \tab HDF5 Attribute.
#' }
#' 
#' @section Extract/List File Contents:
#' The following functions are defined to extract HDF5 file contents:
#' \describe{
#'   \item{list.groups}{List HDF5 groups in file.}
#'   \item{list.datasets}{List HDF5 datasets in file.}
#'   \item{list.attributes}{List Attributes of HDF5 object (file, group or dataset).}
#' }
#' @seealso \code{\link{CommonFG}} \code{\link{CommonFG-Group}} 
#'   \code{\link{CommonFG-DataSet}} \code{\link{H5Location-Attribute}}
#' @param .Object H5File; S4 object of class \code{H5File};
#' @param name character; File path pointing to H5File.
#' @param mode mode used for file
#' The following modes are supported by \code{h5file}:
#' \describe{
#'   \item{r}{Read only, file must exist.}
#'   \item{r+}{Read/write, file must exist.}
#'   \item{w}{Create file, truncate if exists.}
#'   \item{w-}{Create file, fail if exists.}
#'   \item{a}{Read/write if exists, create otherwise (default).}
#' }
#' @examples
#' # The following examples generates a HDF5 file with the different HDF5 
#' # Objects and shows its contents:
#' file <- h5file(name = "test1.h5", mode = "a")
#' file["testdataset"] <- 1:10
#' h5attr(file, "testattrib") <- LETTERS[1:10]
#' file["testgroup/testdataset2"] <- 1:10
#' file
#' # Close file and delete
#' h5close(file)
#' if(file.exists("test.h5")) file.remove("test.h5")
#' 
#' # The following example shows hdf5 file contents and how to use them to iterate over HDF5 elements:
#' file <- h5file(name = "test2.h5", mode = "a")
#' file["testgroup1/testset1"] <- 1:10
#' file["testgroup2/testset2"] <- 11:20
#' file["testgroup3/testset3"] <- 21:30
#' 
#' # Extract first 3 elements from each dataset and combine result to matrix
#' sapply(list.datasets(file, recursive = TRUE), function(x) file[x][1:3])
#' # Add new dataset to each group in HDF5 file
#' for(g in list.groups(file)) {
#'   file[paste(g, "testsetx", collapse = "/")] <- 1:10
#' }
#' list.datasets(file, recursive = TRUE)
#' # Close file
#' h5close(file)
#' @rdname H5File
#' @name H5File
#' @aliases H5File-class
#' @include H5Location-Attribute.R CommonFG.R
#' @export
setClass( "H5File", representation(mode = "character"), 
    contains = c("CommonFG"))

#' @importFrom tools file_path_as_absolute
setMethod( "initialize", "H5File", 
function(.Object, name, mode = "a") {
	stopifnot(is.character(name))
	stopifnot(is.character(mode))
	stopifnot(length(name) == 1)
	stopifnot(length(mode) == 1)
	
	if (!mode %in% c("r", "r+", "w", "w-", "a")) {
		stop(sprintf("Parameter mode must be either 'r', 'r+', 'w', 'w-' or 'a' - '%s' was given", mode))
	}
  
  name <- path.expand(name)
					
	.Object@pointer <- OpenFile(name, mode)
  .Object@location <- file_path_as_absolute(name)
	.Object@mode <- mode
	.Object
})

#' @rdname H5File
#' @importFrom methods new
#' @export
h5file <- function(name, mode = "a") {
  new("H5File", name, mode)
}

#' @rdname H5File
#' @importFrom methods new
#' @export
H5File <- function(name, mode = "a") {
  warning("This function is deprecated, use h5file instead")
  new("H5File", name, mode)
}

#' @rdname H5File
#' @aliases h5flush
#' @export
setGeneric("h5flush", function(.Object)
      standardGeneric("h5flush")
)

#' @rdname H5File
#' @export 
setMethod("h5flush", signature(.Object="H5File"), function(.Object) {
      invisible(FlushFile(.Object@pointer))
    })

setMethod("show", "H5File",
  function(object) {
    cat(sprintf("H5File '%s' (mode '%s')\n", basename(object@location), 
            object@mode))
    out <- paste(	c(sprintf("+ %s", sort(list.groups(object, full.names = FALSE, recursive = FALSE))), 
            sprintf("D %s", sort(list.datasets(object, full.names = FALSE, recursive = FALSE))),
            sprintf("A %s", sort(list.attributes(object)))), collapse = "\n")
    cat(out, "\n")
  })

#' @rdname H5File
#' @export
setMethod("h5close", "H5File", function(.Object) {
      invisible(CloseFile(.Object@pointer))
    })

#' @rdname H5File
#' @export
is.h5file <- function(name) {
  res <- FALSE
  if(file.exists(name)) {
    res <- IsHDF5File(name);
  } else {
    warning("File does not exist.")
  }
  res
}


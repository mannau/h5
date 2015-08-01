#' The H5File Class
#' 
#' Representation of an HDF5 file which can be either created or accessed if 
#' existing. In addition to File--specific capabilities listed below H5File 
#' shares common functionality with H5Group through the \code{\link{CommonFG}} 
#' base class.
#' 
#' HDF5 files can be opened or generated using the \code{H5File()} function and
#' a specified file access mode. \code{h5file()} returns a \code{H5File} object
#' which can be used to access \code{\link{H5Group}}s and \code{\link{DataSet}}s 
#' using subsetting parameters or functions accordingly.
#' 
#' HDF5 files which have been created or opened through \code{H5File()} need 
#' to be closed afterwards using \code{h5close()}. 
#' 
#' \code{h5flush()} can be used to flush unwritten data to an HDF5 file.
#' @seealso \code{\link{CommonFG}} \code{\link{CommonFG-Group}} 
#'   \code{\link{CommonFG-DataSet}} \code{\link{H5Location-Attribute}}
#' @param .Object H5File; S4 object of class \code{H5File};
#' @param name character; File path pointing to H5File.
#' @param mode mode used for file.
#' The following modes are supported by h5file:
#' \describe{
#'   \item{r}{Read only, file must exist.}
#'   \item{r+}{Read/write, file must exist.}
#'   \item{w}{Create file, truncate if exists.}
#'   \item{w-}{Create file, fail if exists.}
#'   \item{a}{Read/write if exists, create otherwise (default).}
#' }
#' @examples
#' # Create new file using mode 'a'
#' file <- h5file("test.h5")
#' h5close(file)
#' # Open File for read-only
#' file <- h5file("test.h5", "r")
#' h5close(file)
#' file.remove("test.h5")
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
    out <- paste(	c(sprintf("+ %s", sort(list.groups(object, recursive = FALSE))), 
            sprintf("D %s", sort(list.datasets(object, recursive = FALSE))),
            sprintf("A %s", sort(list.attributes(object)))), collapse = "\n")
    cat(out)
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


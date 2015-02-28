#' The H5File class
#'
#' Class to represent an HDF5 file.
#'
#' @param .Object H5File; S4 object of class \code{H5File};
#' @name H5File 
#' @rdname H5File
#' @aliases H5File-class
#' @include H5Location.R CommonFG.R
#' @export
setClass( "H5File", representation(mode = "character"), 
		contains = c("CommonFG", "H5Location"))

#' @rdname H5File
#' @export
setMethod("closeh5", signature(.Object="H5File"), function(.Object) {
			invisible(CloseFile(.Object@pointer))
		})

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
					
	.Object@pointer <- OpenFile(name, mode)
  .Object@location <- file_path_as_absolute(name)
	.Object@mode <- mode
	.Object
})

#' @rdname H5File
#' @param name character; File path
#' @param mode mode used for file.
#' @return H5File
#' @export
H5File <- function(name, mode = "a") {
  new("H5File", name, mode)
}

#' @rdname H5File
#' @export
setGeneric("flushh5", function(.Object)
      standardGeneric("flushh5")
)

#' @rdname H5File
#' @export
setMethod("flushh5", signature(.Object="H5File"), function(.Object) {
      invisible(FlushFile(.Object@pointer))
    })

setMethod("show", "H5File",
    function(object) {
      cat(sprintf("H5File '%s' (mode '%s')\n", basename(object@location), object@mode))
      GetFGInfo(object@pointer, "/")
    })

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
setClass( "H5File", representation(filePath = "character", mode = "character"), 
		contains = c("CommonFG", "H5Location"))

#' @rdname H5File
#' @export
setMethod("closeh5", signature(.Object="H5File"), function(.Object) {
			invisible(CloseFile(.Object@pointer))
		})

setMethod( "initialize", "H5File", 
function(.Object, filePath, mode = "a") {
	stopifnot(is.character(filePath))
	stopifnot(is.character(mode))
	stopifnot(length(filePath) == 1)
	stopifnot(length(mode) == 1)
	
	if (!mode %in% c("r", "r+", "w", "w-", "a")) {
		stop(sprintf("Parameter mode must be either 'r', 'r+', 'w', 'w-' or 'a' - '%s' was given", mode))
	}
					
	.Object@pointer <- OpenFile(filePath, mode)
	.Object@filePath <- filePath
	.Object@mode <- mode
	.Object
})

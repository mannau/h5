#' The DataSet class
#'
#' The DataSet class used to store data objects in HDF5 tree.
#' 
#' @param .Object DataSet; S4 object of class \code{DataSet};
#' @param data object; Object to be stored in HDF5 file, can be either of type vector, matrix or array.
#' @name DataSet 
#' @rdname DataSet
#' @aliases DataSet-class
#' @include H5Location.R
#' @export
setClass( "DataSet", representation( pointer = "externalptr", typechar = "character"), 
		contains = "H5Location")


#' @rdname DataSet
#' @export
setGeneric("writeDataSet", function(.Object, data)
			standardGeneric("writeDataSet")
)

#' @rdname DataSet
#' @export
setMethod("writeDataSet", signature(.Object="DataSet", data = "ANY"), 
		function(.Object, data) {
			invisible(WriteDataset(.Object@pointer, data, .Object@typechar))
		})

#' @rdname DataSet
#' @export
setGeneric("readDataSet", function(.Object)
			standardGeneric("readDataSet")
)

#' @rdname DataSet
#' @export
setMethod("readDataSet", signature(.Object="DataSet"), 
		function(.Object) {
			ReadDataset(.Object@pointer, .Object@typechar)
		})

setMethod( "initialize", "DataSet",
		function(.Object, location, typechar) {
			.Object@pointer = location
			.Object@typechar = typechar
			.Object
		})

#' @rdname DataSet
#' @export
setMethod("closeh5", signature(.Object="DataSet"), function(.Object) {
			invisible(CloseDataset(.Object@pointer))
		})

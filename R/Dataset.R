#' The DataSet class
#'
#' The DataSet class used to store data objects in HDF5 tree.
#' 
#' @param .Object DataSet; S4 object of class \code{DataSet};
#' @param data object; Object to be stored in HDF5 file, can be either of type vector, matrix or array.
#' @param offset numeric; Offset to be selected from Hyperslab.
#' @param count numeric; Count to be selected from Hyperslab.
#' @name DataSet 
#' @rdname DataSet
#' @aliases DataSet-class
#' @include H5Location.R
#' @export
setClass( "DataSet", representation( pointer = "externalptr", 
                                     datatype = "character",
                                     dim = "numeric", 
                                     maxdim = "numeric", 
                                     chunksize = "numeric",
                                     compression = "character"), 
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
			invisible(WriteDataset(.Object@pointer, data, .Object@datatype))
		})

#' @rdname DataSet
#' @export
setGeneric("readDataSet", function(.Object, offset = NA, count = NA)
			standardGeneric("readDataSet")
)

#' @rdname DataSet
#' @export
setMethod("readDataSet", signature(.Object="DataSet", offset = "ANY", count = "ANY"), 
		function(.Object, offset, count) {
			dset <- ReadDataset(.Object@pointer, offset - 1, count)
      if(is.matrix(dset)) {
        return(t(dset))
      }
      dset   
		})

setMethod( "initialize", "DataSet",
		function(.Object, location, 
                      datatype, 
                      dim = GetDataSetDimensions(location), 
                      maxdim = GetDataSetMaxDimensions(location), 
                      chunksize = GetDataSetChunksize(location), 
                      compression = GetDataSetCompression(location)) {
			.Object@pointer = location
			.Object@datatype = datatype
      .Object@dim = dim      
      .Object@maxdim = maxdim   
      .Object@chunksize = chunksize
      .Object@compression = compression
			.Object
		})

#' @rdname DataSet
#' @export
setMethod("closeh5", signature(.Object="DataSet"), function(.Object) {
			invisible(CloseDataset(.Object@pointer))
		})

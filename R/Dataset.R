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
setGeneric("readDataSet", function(.Object, 
        offset = rep(NA_integer_, length(.Object@dim)) , 
        count = rep(NA_integer_, length(.Object@dim)))
			standardGeneric("readDataSet")
)

#' @rdname DataSet
#' @export
setMethod("readDataSet", signature(.Object="DataSet", offset = "ANY", count = "ANY"), 
		function(.Object, offset, count) {
      
      ### Type checking
      if (!(is.numeric(offset) | is.integer(offset))) {
        stop("Parameter offset must be of type integer/numeric.")
      } 
      if (!(is.numeric(count) | is.integer(count))) {
        stop("Parameter count must be of type integer/numeric.")
      } 
      
      ### Length checking
      if (length(offset) != length(.Object@dim)) {
        stop(sprintf("Parameter offset must have length of dataset dimensions (%d)", 
                length(.Object@dim)))
      }
      if (length(count) != length(.Object@dim)) {
        stop(sprintf("Parameter count must have length of dataset dimensions (%d)", 
                length(.Object@dim)))
      }
      
      ### Check minimum values
      if (!all(offset > 0 | is.na(offset))) {
        stop(sprintf("Elements of parameter offset must be greater than zero or NA", 
                length(.Object@dim)))
      }
      
      if (!all(count > 0 | is.na(count))) {
        stop(sprintf("Elements of parameter count must be greater than zero or NA", 
                length(.Object@dim)))
      }
      
      ### Set NA values of offset to minimum values
      offset[is.na(offset)] <- 1
      
      ### Set NA values of count to residual maximum values
      count[is.na(count)] <- (.Object@dim - offset)[is.na(count)] + 1

      ### Boundaries check
      if (!all((offset + count - 1) <= .Object@dim)) {
        stop("subscript out of bounds")
      }

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

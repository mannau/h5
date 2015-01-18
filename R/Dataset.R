#' The DataSet class
#'
#' The DataSet class used to store data objects in HDF5 tree.
#' 
#' @param .Object,x DataSet; S4 object of class \code{DataSet};
#' @param data,y object; Object to be stored in HDF5 file, can be either of type vector, matrix or array.
#' @param offset numeric; Offset to be selected from Hyperslab.
#' @param count numeric; Count to be selected from Hyperslab.
#' @param dims numeric; Dimensions of DataSet.
#' @param recursive logical; Argument passed to \code{\link{c}}.
#' @param ... additional arguments passed to \code{\link{c}}.
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
setGeneric("checkParamBoundaries", function(.Object, offset, count)
      standardGeneric("checkParamBoundaries")
)

#' @rdname DataSet
#' @export
setMethod("checkParamBoundaries", signature(.Object="DataSet", 
        offset = "numeric", count = "numeric"), 
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
      
      list(offset, count)
    })


#' @rdname DataSet
#' @export
setGeneric("writeDataSet", function(.Object, data, 
        offset = rep(NA_integer_, length(.Object@dim)))
			standardGeneric("writeDataSet")
)

#' @rdname DataSet
#' @export
setMethod("writeDataSet", signature(.Object="DataSet", data = "ANY", 
        offset = "ANY"), 
		function(.Object, data, offset) {      
      count <- GetDimensions(data)
      
      out <- checkParamBoundaries(.Object, offset, count)
      offset <- out[[1]]
      count <- out[[2]]
      
      #      if(is.matrix(data)) {
      #        data <- t(data)
      #      }
      if(is.array(data)) {
        data <- aperm(data, rev(1:length(dim(data))))
      }
      
      dspace <- GetDataspace(.Object@pointer, offset - 1, count)
			res <- WriteDataset(.Object@pointer, dspace, data, .Object@datatype)
      CloseDataspace(dspace)
      invisible(res)
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
      out <- checkParamBoundaries(.Object, offset, count)
      offset <- out[[1]]
      count <- out[[2]]
      
      dspace <- GetDataspace(.Object@pointer, offset - 1, count)
			dset <- ReadDataset(.Object@pointer, dspace)
      CloseDataspace(dspace)
      
      if(is.matrix(dset)) {
        return(t(dset))
      }
      if(is.array(dset)) {
        return(aperm(dset, rev(1:length(dim(dset)))))
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

#' @rdname DataSet
#' @export
setGeneric("extendDataSet", function(.Object, dims)
      standardGeneric("extendDataSet")
)

#' @rdname DataSet
#' @export
setMethod("extendDataSet", signature(.Object="DataSet", dims = "numeric"), 
    function(.Object, dims) {
      # TODO: should be checked if necessary
      if(length(dims) != length(.Object@dim)) {
        stop("Number of extendible dimensions must agree with DataSet dimensions.")
      }
      if(!all(dims >= .Object@dim)) {
        stop("Number of extendible dimensions must be greater or equal than DataSet dimensions.")
      }
      
      if(!all(dims <= .Object@maxdim)) {
        stop("Number of extendible dimensions exceeds maximum dimensions of DataSet.")
      }

      ExtendDataset(.Object@pointer, dims)
      .Object@dim <- GetDataSetDimensions(.Object@pointer)
      .Object
    })


#' @rdname DataSet
#' @export
setMethod("rbind2", signature(x="DataSet", y = "matrix"), 
    function(x, y) {
      nrowx <- x@dim[1L]
      nrowy <- dim(y)[1L]
      ncolx <- x@dim[2L]
      ncoly <- dim(y)[2L]
      
      if(ncolx != ncoly) {
        stop(sprintf("Data to append does not match dataset dimensions (%d != %d).", 
                ncolx, ncoly))
      }
      
      dtype <- substr(typeof(y), 1, 1)
      if(x@datatype != dtype) {
        stop("Data to append does not match type of DataSet.")
      }
      
      newdims <- c(nrowx + nrowy, ncolx)
      x <- extendDataSet(x, newdims)
      writeDataSet(x, y, c(nrowx + 1, 1))
      x
})

#' @rdname DataSet
#' @export
setMethod("cbind2", signature(x="DataSet", y = "matrix"), 
  function(x, y) {
    nrowx <- x@dim[1L]
    nrowy <- dim(y)[1L]
    ncolx <- x@dim[2L]
    ncoly <- dim(y)[2L]
    
    if(nrowx != nrowy) {
      stop(sprintf("Data to append does not match dataset dimensions (%d != %d).", 
              nrowx, nrowy))
    }
    
    dtype <- substr(typeof(y), 1, 1)
    if(x@datatype != dtype) {
      stop("Data to append does not match type of DataSet.")
    }
    
    newdims <- c(nrowx, ncolx + ncoly)
    x <- extendDataSet(x, newdims)
    writeDataSet(x, y, c(1, ncolx + 1))
    x
  })

#' @rdname DataSet
#' @export
setMethod("c", "DataSet", 
    function(x, ..., recursive=FALSE) {
      
    y <- do.call(c, list(..., recursive = recursive))
    
    dtype <- substr(typeof(y), 1, 1)
    if(x@datatype != dtype) {
      stop("Data to append does not match type of DataSet.")
    }
    olddim <- x@dim[1L]
    newdims <- c(x@dim[1L] + length(y))
    x <- extendDataSet(x, newdims)
    writeDataSet(x, y, olddim + 1)
    x
  })
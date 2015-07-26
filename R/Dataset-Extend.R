#' Functions to Extend a DataSet
#' 
#' \code{\link{DataSet}}s can be extended with R--objects (e.g. vectors, 
#' matrices, arrays) if the following conditions are met:
#' \enumerate{
#'   \item{Datatype of \link{DataSet} and R-object are compatible.}
#'	 \item{Dimensions of \link{DataSet} and R-object match (no recycling).}
#'   \item{\link{DataSet} does not exceed maximum dimensions as specified at creation.}
#' }
#' 
#' Known base functions have been overloaded to extend vectors (\code{c}) and
#' matrices (\code{rbind}, \code{cbind}). Also the lower--level S4--method
#' \code{extendDataSet} can be used to extend existing \code{\link{DataSet}} 
#' objects.
#' 
#' @param .Object,x DataSet; S4 object of class \code{DataSet};
#' @param dims numeric; Dimensions of DataSet.
#' @param recursive logical; Argument passed to \code{\link{c}}.
#' @param ... additional arguments passed to \code{\link{c}}.
#' @rdname DataSet-Extend
#' @name DataSet-Extend
#' @aliases extendDataSet
#' @docType methods
#' @export
setGeneric("extendDataSet", function(.Object, dims)
      standardGeneric("extendDataSet")
)

#' @rdname DataSet-Extend
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

#' Additional function for cbind S4 dispatching
#' @noRd
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
    dspace <- selectDataSpace(x, offset = c(nrowx + 1, 1), count = GetDimensions(y))
    writeDataSet(x, y, dspace)
    x
  })

#' Additional function for cbind S4 dispatching
#' @noRd
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
    dspace <- selectDataSpace(x, offset = c(1, ncolx + 1), count = GetDimensions(y))
    writeDataSet(x, y, dspace)
    x
  })

#' @rdname DataSet-Extend
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
    dspace <- selectDataSpace(x, offset = olddim + 1, count = GetDimensions(y))
    writeDataSet(x, y, dspace)
    x
  })

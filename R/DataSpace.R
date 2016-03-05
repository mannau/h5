#' The DataSpace Class
#'
#' The DataSpace class used to select data elements from \code{\link{DataSet}}
#' objects. DataSpace objects can be generated using the S4--method 
#' \code{selectDataSpace} from DataSets objects in 3 ways:
#' \enumerate{
#'   \item{Without any parameters: Select all elements in \code{\link{DataSet}}}.
#'   \item{With \code{offset} and \code{count}: Select contiguous sub--region
#'   (hyperslab) from \code{\link{DataSet}}.}
#'   \item{With parameter \code{elem}: Select sub-elements specified by matrix 
#'     holding selected indices. This selection type can become slow for many 
#'     data points.}
#' }                             
#' @param .Object DataSet; S4 object of class \code{DataSet} 
#' (\code{DataSpace} for \code{h5close}).
#' @param offset numeric; Offset to be selected from Hyperslab.
#' @param count numeric; Count to be selected from Hyperslab.  
#' @param elem matrix; Matrix specifying element selection coordinates. 
#' Columns specify rank, rows specify different points.
#' @param ... additional arguments passed to \code{\link{c}}.
#' @rdname DataSpace
#' @name DataSpace
#' @aliases DataSpace-class
#' @include DataSet.R
#' @export
setClass( "DataSpace", representation( pointer = "externalptr", count = "numeric"))

setMethod( "initialize", "DataSpace", 
    function(.Object, pointer, count) { 
      .Object@pointer <- pointer
      .Object@count <- count
      .Object
    })

#' @rdname DataSpace
#' @export
setMethod("h5close", "DataSpace", function(.Object) {
      invisible(CloseDataspace(.Object@pointer))
    })

#' @rdname DataSpace
#' @export
setGeneric("selectDataSpace", function(.Object, 
        offset = rep(NA_integer_, length(.Object@dim)), 
        count = rep(NA_integer_, length(.Object@dim)),
        elem)
      standardGeneric("selectDataSpace")
)

#' @rdname DataSpace   
#' @importFrom methods new
#' @export 
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "missing", count = "missing", elem = "missing"), 
    function(.Object) {
      dspace <- GetDataspace(.Object@pointer)
      dspace <- SelectAll(dspace)
      new("DataSpace", dspace, .Object@dim)
    })

#' @rdname DataSpace
#' @importFrom methods new
#' @export 
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "ANY", count = "ANY", elem = "missing"), 
    function(.Object, offset, count) {
      out <- checkParamBoundaries(.Object, offset, count)
      offset <- out[[1]]
      count <- out[[2]]
      dspace <- GetDataspace(.Object@pointer)
      dspace <- SelectHyperslab(dspace, offset - 1, count)
      new("DataSpace", dspace, count)
    })

#' @rdname DataSpace
#' @importFrom methods new
#' @export 
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "missing", count = "missing", elem = "matrix"), 
    function(.Object, elem) {
      if(any(is.na(elem))) {
        stop("NAs are not allowed in element coordinates.")
      }
      if(any(elem < 1L)) {
        stop("Elements of parameter elem must be greater or equal than one.")
      }
      
      elemtype <- typeof(elem)
      if(!elemtype %in% c("double", "integer")) {
        stop("Element matrix must be of type double or integer.")
      }
      
      if(ncol(elem) != length(.Object@dim)) {
        stop("Number of elem matrix columns must equal length of dataset dimensions.")
      }
      
      dimmax <- apply(elem, 2, max)
      if (any(dimmax > .Object@dim)) {
        stop("subscript out of bounds")
      }
      dspace <- GetDataspace(.Object@pointer)
      dspace <- SelectElem(dspace, t(elem - 1))
      new("DataSpace", dspace, count = nrow(elem))
    })

setGeneric("checkParamBoundaries", function(.Object, offset, count)
      standardGeneric("checkParamBoundaries")
)

#' Check Parameter Boundaries of Offset/Count
#' @noRd
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
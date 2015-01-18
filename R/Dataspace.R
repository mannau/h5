#' The DataSpace class
#'
#' The DataSpace class used to store data objects in HDF5 tree.
#' 
#' @param .Object DataSpace; S4 object of class \code{DataSpace};
#' @param offset numeric; Offset to be selected from Hyperslab.
#' @param count numeric; Count to be selected from Hyperslab.
#' @param dims numeric; Dimensions of DataSpace.
#' @param recursive logical; Argument passed to \code{\link{c}}.
#' @param ... additional arguments passed to \code{\link{c}}.
#' @name DataSpace 
#' @rdname DataSpace
#' @aliases DataSpace-class
#' @export
setClass( "DataSpace", representation( pointer = "externalptr"))

setMethod( "initialize", "DataSpace", 
    function(.Object, pointer) { 
      .Object@pointer <- pointer
      .Object
    })

#' @rdname DataSet
#' @export
setMethod("closeh5", signature(.Object="DataSpace"), function(.Object) {
      invisible(CloseDataspace(.Object@pointer))
    })



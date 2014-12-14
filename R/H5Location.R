#' The H5Location class
#'
#' This class contains an example. This line goes into the description
#'
#' This line and the next ones go into the details.
#' This line thus appears in the details as well.
#' 
#' @param .Object H5Location; S4 object of class \code{H5Location};
#' @param attributename character; Name of attribute to be read/created.
#' @param data object; Data object to be used for attribute creation, 
#' can be either of type vector, matrix or array. 
#' @name H5Location 
#' @rdname H5Location
#' @aliases H5Location-class
#' @export
setClass( "H5Location", representation( pointer = "externalptr" ) )


# @rdname H5Location
# @export
#setGeneric("createAttribute", function(.Object, attributename, data)
#			standardGeneric("createAttribute")
#)

# @rdname H5Location
# @export
#setMethod("createAttribute", signature(.Object="H5Location", attributename = "character", 
#				data = "ANY"), 
#		function(.Object, attributename, data) {
#			dspace <- GetDataSpace(data)
#			invisible(WriteAttribute(.Object@pointer, attributename, data, 
#							dspace$typechar, dspace$dim, dspace$size))
#		})

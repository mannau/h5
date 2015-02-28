#' The H5Group class
#'
#' Class to represent an HDF5 Group.
#'
#' @param .Object H5Group; S4 object of class \code{H5Group};
#' @name H5Group 
#' @rdname H5Group
#' @aliases H5Group-class
#' @include H5Location.R CommonFG.R
#' @export
setClass( "H5Group",
    contains = c("CommonFG", "H5Location"))

#' @rdname H5Group
#' @export
setMethod("closeh5", signature(.Object="H5Group"), function(.Object) {
			invisible(CloseGroup(.Object@pointer))
		})

setMethod( "initialize", "H5Group",
		function(.Object, pointer, location) {
			.Object@pointer <- pointer
      .Object@location <- location
			.Object
		})

setMethod("show", "H5Group",
    function(object) {
      cat(sprintf("H5Group '%s'\n", object@location))
      GetFGInfo(object@pointer, object@location)
    })
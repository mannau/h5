#' The H5Group Class
#'
#' HDF5 Groups are represented by the H5Group class and are the building blocks
#' of the hierarchical organization of an H5File. They form containers for 
#' HDF5 objects and are therefore similar to file system folders. 
#' 
#' In addition to Group--specific capabilities listed below H5Group 
#' shares common functionality with H5File through the CommonFG base class.
#' @param .Object H5Group; S4 object of class \code{H5Group};
#' @rdname H5Group
#' @name H5Group
#' @aliases H5Group-class
#' @include H5Location-Attribute.R CommonFG.R
#' @references \url{https://www.hdfgroup.org/HDF5/doc/H5.intro.html#Intro-OGroups}
#' @export
setClass( "H5Group", contains = c("CommonFG"))

setMethod( "initialize", "H5Group",
		function(.Object, pointer, location) {
			.Object@pointer <- pointer
      .Object@location <- location
			.Object
		})

setMethod("show", "H5Group",
    function(object) {
      cat(sprintf("H5Group '%s'\n", object@location))
      out <- paste(	c(sprintf("+ %s", sort(list.groups(object, full.names = FALSE, recursive = FALSE))), 
              sprintf("D %s", sort(list.datasets(object, full.names = FALSE, recursive = FALSE))),
              sprintf("A %s", sort(list.attributes(object)))), collapse = "\n")
      cat(out, "\n")
    })

#' @rdname H5Group
#' @export
setMethod("h5close", "H5Group", function(.Object) {
      invisible(CloseGroup(.Object@pointer))
    })
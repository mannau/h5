#' Read and Create Attributes for H5Location Objects
#' 
#' \code{H5Location} is the base class of \code{\link{H5File}}, 
#' \code{\link{H5Group}} and \code{\link{DataSet}} and implements common
#' methods to create and access attributes for inherited classes.
#' @param .Object H5Location; S4 object of class \code{H5Location};
#' @param attributename character; Name of attribute to be read/created.
#' @param data object; Data object to be used for attribute creation, 
#' can be either of type vector, matrix or array.
#' @seealso \code{\link{Attribute}} \code{\link{H5File}} \code{\link{H5Group}}
#' \code{\link{DataSet}}
#' @rdname H5Location-Attribute
#' @name H5Location-Attribute
#' @aliases H5Location-Attribute H5Location H5Location-class
#' @examples
#' # Write Attributes for H5File, H5Group and DataSet
#' file <- h5file("test.h5")
#' h5attr(file, "fileattrib") <- 1:10
#' group <- file["testgroup"]
#' h5attr(group, "groupattrib") <- matrix(1:9, nrow = 3)
#' h5attr(group, "groupattrib")
#' group["testdataset"] <- 1:10
#' dset <- group["testdataset"]
#' h5attr(dset, "dsetattrib") <- LETTERS[1:10]
#' h5close(dset)
#' h5close(group)
#' h5close(file)
#' file.remove("test.h5")
#' @export 
setClass( "H5Location", representation( pointer = "externalptr" ) )

#' @rdname H5Location-Attribute
#' @param size numeric; Character length for fixed-length string data types.
#' Default value of -1 creates variable-length strings.
#' @export
setGeneric("createAttribute", function(.Object, attributename, data, size = -1)
			standardGeneric("createAttribute")
)

#' @rdname H5Location-Attribute
#' @importFrom methods new
#' @export
setMethod("createAttribute", signature(.Object="H5Location", 
        attributename = "character", data = "ANY", size = "ANY"), 
  function(.Object, attributename, data, size) {
    dspace <- GetDataSpace(data)
    FUN <- NULL
    if (inherits(.Object, "DataSet")) {
      FUN <- CreateAttribute_DataSet
    } else if (inherits(.Object, "CommonFG")) {
      FUN <- CreateAttribute_CommonFG
    } else {
      stop("Object type unknown.")
    }
    attrptr <- FUN(.Object@pointer, attributename, dspace$typechar, dspace$dim, 
        size)
    
    attrib <- new("Attribute", attrptr, attributename, dspace$typechar, dspace$dim)
    writeAttribute(attrib, data)
    CloseAttribute(attrib@pointer)
    invisible(TRUE)
  })



#' @rdname H5Location-Attribute
#' @export
setGeneric("openAttribute", function(.Object, attributename)
      standardGeneric("openAttribute")
)

#' @rdname H5Location-Attribute
#' @importFrom methods new
#' @export
setMethod("openAttribute", signature(.Object="H5Location", attributename = "character"), 
  function(.Object, attributename) {
    FUN <- NULL
    if (inherits(.Object, "DataSet")) {
      FUN <- OpenAttribute_DataSet
    } else if (inherits(.Object, "CommonFG")) {
      FUN <- OpenAttribute_CommonFG
    } else {
      stop("Object type unknown.")
    }
    pointer <- FUN(.Object@pointer, attributename)
    new("Attribute", pointer, attributename, 
        GetAttributeType(pointer), GetAttributeDimensions(pointer))
  })

#' @rdname H5Location-Attribute
#' @export                                                                      
setGeneric("h5attr", function(.Object, attributename)
      standardGeneric("h5attr")
)

#' @rdname H5Location-Attribute
#' @export
setMethod("h5attr", signature(.Object="H5Location", attributename = "character"),
  function(.Object, attributename) {
    attribute <- openAttribute(.Object, attributename)
    res <- readAttribute(attribute)
    CloseAttribute(attribute@pointer)
    res
  })

#' @rdname H5Location-Attribute
#' @param value object; Object to be stored in HDF5 Attribute, can be either of 
#' type vector, matrix or array.
#' @param ... Additional parameters passed to \code{\link{createAttribute}}. 
#' @export                                                                      
setGeneric("h5attr<-", function(.Object, attributename, ..., value)
      standardGeneric("h5attr<-")
)

#' @rdname H5Location-Attribute
#' @export
setMethod("h5attr<-", signature(.Object="H5Location", attributename = "character", 
        value = "ANY"),
  function(.Object, attributename, ..., value) {
    createAttribute(.Object, attributename, value, ...)
    .Object
  })

#' @rdname H5Location-Attribute
#' @export
setGeneric("list.attributes", function(.Object)
      standardGeneric("list.attributes")
)

#' @rdname H5Location-Attribute
#' @export
setMethod( "list.attributes", c("H5Location"), 
  function(.Object) {
    if (inherits(.Object, "CommonFG")) {
      res <- GetAttributeNames_CommonFG(.Object@pointer)
    } else if (inherits(.Object, "DataSet")) {
      res <- GetAttributeNames_DataSet(.Object@pointer)
    } else {
      stop("Object does not inherit H5File, H5Group or DataSet.")
    }
    res
  })    



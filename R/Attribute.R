#' The Attribute Class
#'
#' An HDF5 \code{Attribute} object is used to store meta data about a primary
#' data object like \code{\link{DataSet}}, \code{\link{H5Group}} or 
#' \code{\link{H5File}}. The functions described in this section show a 
#' low--level interface to read and write attributes. See 
#' \code{\link{H5Location}} for the easier-to-use \code{\link{h5attr}}
#' convenience functions.
#' 
#' \code{Attribute}s are assumed to be like small \code{\link{DataSet}}s since 
#' they can store the same data types in homogeneous objects like vectors, 
#' matrices and arrays. However, there are two important differences compared to 
#' \code{\link{DataSet}} objects:
#' \enumerate{
#'   \item{Attributes do not support compression or chunking.}
#'   \item{Subsetting is not possible for attribute data.}
#' }
#' @rdname Attribute
#' @name Attribute
#' @aliases Attribute-class
#' @include H5Location-Attribute.R
#' @seealso \code{\link{H5Location-Attribute}} 
#' @references \url{https://www.hdfgroup.org/HDF5/doc/H5.intro.html#Intro-OAttributes}
#' @export
setClass( "Attribute", representation( name = "character", 
        datatype = "character", dim = "numeric"), 
    contains = "H5Location")

setMethod( "initialize", "Attribute",
    function(.Object, location, attributename, datatype, dim) {
      .Object@pointer = location
      .Object@name = attributename
      .Object@datatype = datatype
      .Object@dim = dim
      .Object
    })

#' @rdname Attribute
#' @aliases writeAttribute
#' @param transpose logical; Determine if data object should be 
#' transposed.
#' @export
setGeneric("writeAttribute", function(.Object, data = GetDimensions(data),
        transpose = TRUE)
      standardGeneric("writeAttribute")
)

#' @rdname Attribute
#' @param .Object Attribute; S4 object of class \code{Attribute}.
#' @param data object; Object to be stored in HDF5 file, can be either of type 
#' vector, matrix or array.
#' @export
setMethod("writeAttribute", signature(.Object="Attribute", data = "ANY", 
        transpose = "ANY"), 
  function(.Object, data, transpose) {      
    if(prod(GetDimensions(data)) != prod(.Object@dim)) {
      stop("number of items to replace is not equal to replacement length")
    }
    if(transpose) {
      data <- transpose_data(data)
    }
    res <- WriteAttribute(.Object@pointer, data, .Object@datatype, 
        .Object@dim)
    invisible(res)
  })

#' @rdname Attribute
#' @export
setGeneric("readAttribute", function(.Object)
      standardGeneric("readAttribute")
)

#' @rdname Attribute
#' @export
setMethod("readAttribute", signature(.Object = "Attribute"), 
    function(.Object) {
      dset <- ReadAttribute(.Object@pointer, .Object@dim)
      transpose_data(dset)
    })

#setMethod("h5close", "Attribute", function(.Object) {
#      invisible(CloseAttribute(.Object@pointer))
#    })
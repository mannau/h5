#' The DataSet Class
#'
#' \code{DataSet}s are used to store data objects in the HDF5 tree. Data objects
#' contain homogeneous data of one type like numeric, integer or character and
#' can be subsetted, extended and enriched with Attributes (see 
#' \code{\link{DataSet-Subset}}, \code{\link{DataSet-Extend}} and 
#' \code{\link{H5Location-Attribute}}). Although subsetting operators provide a 
#' convenient way to handle \code{DataSet} objects the S4 methods described in 
#' this section are used under the hood and give more control. Especially
#' for big \code{DataSet}s it can be advantageous to use these methods with 
#' \code{\link{DataSpace}} objects including hyperslab selections.
#' @param .Object DataSet; S4 object of class \code{DataSet};
#' @param data object; Object to be stored in HDF5 file, can be either of type 
#' vector, matrix or array.
#' @param dspace DataSpace; Data space object used for data selection.
#' @param ... additional arguments passed to \code{\link{c}}.
#' @aliases DataSet-class
#' @rdname DataSet
#' @name DataSet
#' @include H5Location-Attribute.R
#' @references \url{https://www.hdfgroup.org/HDF5/doc/H5.intro.html#Intro-ODatasets}
#' @export
setClass( "DataSet", representation( name = "character",
                                     datatype = "character",
                                     dim = "numeric", 
                                     maxdim = "numeric", 
                                     chunksize = "numeric",
                                     compression = "character"), 
  contains = "H5Location")

#' @rdname DataSet
#' @param transpose logical; Determine if data object (if is array) should be 
#' transposed.
#' @export
setGeneric("writeDataSet", function(.Object, data, 
        dspace = selectDataSpace(.Object,  rep(NA_integer_, length(.Object@dim)), 
            GetDimensions(data)), 
        transpose = TRUE)
      standardGeneric("writeDataSet")
)

#' @rdname DataSet
#' @export
setMethod("writeDataSet", signature(.Object="DataSet", data = "ANY", dspace = "ANY", 
        transpose = "ANY"), 
  function(.Object, data, dspace, transpose) {      
    stopifnot(inherits(dspace, "DataSpace"))
    if(prod(GetDimensions(data)) != prod(dspace@count)) {
      stop("number of items to replace is not equal to replacement length")
    }
    if(transpose) {
      data <- transpose_data(data)
    }
    res <- WriteDataset(.Object@pointer, dspace@pointer, data, 
        .Object@datatype, dspace@count)
    h5close(dspace)
    invisible(res)
  })

#' @rdname DataSet
#' @export
setGeneric("readDataSet", function(.Object, 
        dspace = selectDataSpace(.Object))
			standardGeneric("readDataSet")
)

#' @rdname DataSet
#' @export
setMethod("readDataSet", signature(.Object = "DataSet", dspace = "ANY"), 
		function(.Object, dspace) {
      stopifnot(inherits(dspace, "DataSpace"))
			dset <- ReadDataset(.Object@pointer, dspace@pointer, dspace@count)
      dset <- transpose_data(dset)
      h5close(dspace)
      dset   
		})

setMethod( "initialize", "DataSet",
		function(.Object, location, 
                      datasetname,
                      datatype, 
                      dim = GetDataSetDimensions(location), 
                      maxdim = GetDataSetMaxDimensions(location), 
                      chunksize = GetDataSetChunksize(location), 
                      compression = GetDataSetCompression(location)) {
			.Object@pointer = location
      .Object@name = datasetname
      .Object@datatype = datatype
      .Object@dim = dim      
      .Object@maxdim = maxdim   
      .Object@chunksize = chunksize
      .Object@compression = compression
			.Object
		})

setMethod("show", "DataSet",
  function(object) {
    dimstring <- paste(sprintf("%.5g", object@dim), collapse = " x ")
    typestring <- switch(object@datatype, 
                    i = "integer", 
                    d = "numeric", 
                    c = "character", 
                    l = "logical", 
                    x = "vlen-double",
                    y = "vlen-integer",
                    z = "vlen-logical",
                    t = "compound",
                    m = "datetime",
                    f = "enum",
                    "unknown")
    maxdimstring <- ifelse(object@maxdim >= 1.844674e+19, "UNLIMITED", 
        sprintf("%.5g", object@maxdim))
    if(all(maxdimstring == "UNLIMITED")) maxdimstring <- maxdimstring[1]
    maxdimstring <- paste(maxdimstring, collapse = " x ")
    chunksizestring <- paste(sprintf("%.5g", object@chunksize), 
        collapse = " x ")
    cat(sprintf("DataSet '%s' (%s)\n", object@name, dimstring))
    cat(sprintf("type: %s\n", typestring))
    cat(sprintf("chunksize: %s\n", chunksizestring))
    cat(sprintf("maxdim: %s\n", maxdimstring))
    cat(sprintf("compression: %s\n", object@compression))
    
    at <- sort(list.attributes(object))
    if(length(at) > 0) {
      cat("Attributes:\n")
      cat(paste(sprintf("  A %s", at), collapse = "\n"))
    }
  })

#' @rdname DataSet
#' @export
setMethod("h5close", "DataSet", function(.Object) {
      invisible(CloseDataset(.Object@pointer))
    })





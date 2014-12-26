#' The CommonFG class
#'
#' The CommonFG class which links H5File with H5Group.
#'
#' @param .Object CommonFG; S4 object of class \code{CommonFG};
#' @param groupname character; HDF5 Group name to be used.
#' @param datasetname character; HDF5 DataSet name to be used.
#' @param data object; Object to be stored in HDF5 file, can be either of type vector, matrix or array.
#' @param type character; Character specifiying data type, can be either one of:
#' 	\describe{
#' 		\item{double}{Double precision real number.}
#' 		\item{integer}{32 Bit Integer}
#' 		\item{logical}{Boolean, which is mapped to 1/0 integer values.}
#' 		\item{character}{Varable--length character strings.}
#'  }
#' @param dimensions integer; Dimensions of dataset to be created.
#' @param chunksize integer; Chunksize to be used for dataset.
#' @param maxdimensions integer; Maximum dimensions used for dataset, NA is mapped to 'unlimited'.
#' @param compression integer; Default GZIP compression level to be used, from 0 (no compression) to 9 (maximum compression), 
#' defaults to \code{4}.
#' @param size integer; Datatype size to be used, only relevant for character strings.
#' @name CommonFG 
#' @rdname CommonFG
#' @aliases CommonFG-class
#' @export
setClass( "CommonFG", representation(pointer = "externalptr", name = "character") )

#' @rdname CommonFG
#' @export
setGeneric("createGroup", function(.Object, groupname)
			standardGeneric("createGroup")
)

#' @rdname CommonFG
#' @export
setMethod( "createGroup", signature(.Object="CommonFG", groupname = "character"), 
		function(.Object, groupname) {
			groupptr <- CreateGroup(.Object@pointer, groupname)
			new("H5Group", groupptr, groupname)
		})

#' @rdname CommonFG
#' @export
setGeneric("openGroup", function(.Object, groupname)
			standardGeneric("openGroup")
)

#' @rdname CommonFG
#' @export
setMethod( "openGroup", signature(.Object="CommonFG", groupname = "character"), 
		function(.Object, groupname) {
			groupptr <- OpenGroup(.Object@pointer, groupname)
			new("H5Group", groupptr, groupname)
		})

#' @rdname CommonFG
#' @export
setGeneric("existsGroup", function(.Object, groupname)
			standardGeneric("existsGroup")
)

#' @rdname CommonFG
#' @export
setMethod( "existsGroup", signature(.Object="CommonFG", groupname = "character"), 
		function(.Object, groupname) {
			ExistsGroup(.Object@pointer, groupname)
		})



#' @rdname CommonFG
#' @export
setGeneric("createDataSet", function(
				.Object, datasetname, data, type, dimensions, 
				chunksize = ChunkSize(data), 
				maxdimensions = rep(NA_integer_, length(GetDimensions(data))), 
				compression = 4L, size = -1)
			standardGeneric("createDataSet")
)

checkChunksize <- function(chunksize) {
  if (!(is.numeric(chunksize) | is.integer(chunksize))) {
    stop("Parameter chunksize must be of type integer/numeric.")
  }
  if (!all((chunksize > 0) | is.na(chunksize))) {
    stop("All elements of chunksize must be greater than zero or NA.")
  }
  invisible(TRUE)
}

checkMaxDimensions <- function(maxdimensions) {
  if (!(is.numeric(maxdimensions) | is.integer(maxdimensions))) {
    stop("Parameter maxdimensions must be of type integer/numeric.")
  }
  if (!all((maxdimensions > 0) | is.na(maxdimensions)) ) {
    stop("All elements of maxdimensions must be greater than zero or NA.")
  }
  invisible(TRUE)
}


#' @rdname CommonFG
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", datasetname = "character", 
				data = "missing", type = "character", dimensions = "integer", 
				chunksize = "ANY", maxdimensions = "ANY", 
				compression = "ANY", size = "ANY"), 
		function(.Object, datasetname, type, dimensions, chunksize, maxdimensions, 
				compression, size) {
			
      stopifnot(type %in% c("double", "integer", "logical", "character"))
      checkChunksize(chunksize)
      checkMaxDimensions(maxdimensions)
      stopifnot(length(maxdimensions) == length(GetDimensions(data)))
      chunksize <- ifelse(!is.na(maxdimensions) & (is.na(chunksize) | chunksize > maxdimensions), 
          maxdimensions, chunksize)
      
			typechar <- substr(type, 1, 1)	
			dsetptr <- CreateDataset(.Object@pointer, datasetname, typechar, dimensions, chunksize,
					maxdimensions, compression, size)
			new("DataSet", dsetptr, typechar)
		})


#' @rdname CommonFG
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", datasetname = "character", 
				data = "ANY", type = "missing", dimensions = "missing", chunksize = "ANY",
				maxdimensions = "ANY", compression = "ANY", size = "missing"), 
		function(.Object, datasetname, data, chunksize, maxdimensions, compression) {
			if(missing(data)) {
        stop("Parameter data must be specified.")
      }
      checkChunksize(chunksize)
      checkMaxDimensions(maxdimensions)
      stopifnot(length(maxdimensions) == length(GetDimensions(data)))
      chunksize <- ifelse(!is.na(maxdimensions) & (is.na(chunksize) | chunksize > maxdimensions), 
          maxdimensions, chunksize)
      
      if (!all(is.na(maxdimensions) | maxdimensions >= GetDimensions(data))) {
        stop("Parameter maxdimensions must be equal or exceed data dimension size.")
      } 
        
      dspace <- GetDataSpace(data)
			dsetptr <- CreateDataset(.Object@pointer, datasetname, dspace$typechar, dspace$dim,
					chunksize, maxdimensions, compression, dspace$size)
			dset <- new("DataSet", dsetptr, dspace$typechar)
			writeDataSet(dset, data)
			dset
		})

#' @rdname CommonFG
#' @export
setGeneric("openDataSet", function(.Object, datasetname, type)
			standardGeneric("openDataSet")
)

#' @rdname CommonFG
#' @export
setMethod("openDataSet", signature(.Object="CommonFG", datasetname = "character", type = "character"), 
		function(.Object, datasetname, type) {
			stopifnot(type %in% c("double", "integer", "logical", "character"))
			typechar <- substr(type, 1, 1)	
			dsetptr <- OpenDataset(.Object@pointer, datasetname)
			dset <- new("DataSet", dsetptr, typechar)
		})

#' @rdname CommonFG
#' @export
setGeneric("closeh5", function(.Object)
			standardGeneric("closeh5")
)



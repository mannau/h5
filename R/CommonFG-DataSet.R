#' Functions to Create/Open DataSets in \code{\link{CommonFG}} objects                               
#'                                                                              
#' Although \code{\link{DataSet}} objects can implicitly be created using 
#' subsetting operators (see \code{\link{CommonFG}}) \pkg{h5} implements more
#' explicit functions (used by subsetting operators under the hood) to create 
#' and open \code{\link{DataSet}s}.
#'                                         
#' @param .Object CommonFG; S4 object of class \code{CommonFG};                                       
#' @param datasetname character; HDF5 DataSet name to be used.                  
#' @param data object; Object to be stored in HDF5 file, can be either of type  
#' vector, matrix or array.                                                     
#' @param type character; Character specifying data type, can be either one of:
#' 	\describe{                                                                  
#' 		\item{double}{Double precision floating--point number.}                              
#' 		\item{integer}{32--Bit integer.}                                            
#' 		\item{logical}{Boolean, which is mapped to 1/0 integer values.}           
#' 		\item{character}{Variable--length character strings.}                      
#'  }                                                                           
#' @param dimensions integer; Dimensions of dataset to be created.              
#' @param chunksize integer; Chunksize to be used for dataset.                  
#' @param maxdimensions integer; Maximum dimensions used for dataset, NA is     
#' mapped to 'unlimited'.                                                       
#' @param compression integer; Default GZIP compression level to be used, from  
#' 0 (no compression) to 9 (maximum compression), defaults to \code{4}.         
#' @param size integer; Size of data type to be used, only relevant for character   
#' strings.                                                                     
#' @name CommonFG-DataSet                                                         
#' @rdname CommonFG-DataSet                                                             
#' @include CommonFG.R
#' @aliases createDataSet
#' @export                                                                      
setGeneric("createDataSet", function(
        .Object, datasetname, data, type, dimensions, 
        chunksize = ChunkSize(data), 
        maxdimensions = rep(NA_integer_, length(GetDimensions(data))), 
        compression = 4L, size = -1)
      standardGeneric("createDataSet")
)

#' @rdname CommonFG-DataSet
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", 
        datasetname = "character", 
        data = "missing", type = "character", dimensions = "integer", 
        chunksize = "ANY", maxdimensions = "ANY", 
        compression = "ANY", size = "ANY"), 
    function(.Object, datasetname, type, dimensions, chunksize, maxdimensions, 
        compression, size) {
      
      stopifnot(type %in% c("double", "integer", "logical", "character"))
      typechar <- substr(type, 1, 1)	
      
      createDataset_internal(.Object@pointer, datasetname, typechar, dimensions,
          chunksize, maxdimensions, compression, size) 
    })

#' @rdname CommonFG-DataSet
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", 
        datasetname = "character", data = "ANY", type = "missing", 
        dimensions = "missing", chunksize = "ANY",maxdimensions = "ANY", 
        compression = "ANY", size = "missing"), 
    function(.Object, datasetname, data, chunksize, maxdimensions, 
        compression) {
      if(missing(data)) {
        stop("Parameter data must be specified.")
      }
      stopifnot(length(maxdimensions) == length(GetDimensions(data)))
      
      dspace <- GetDataSpace(data)
      dset <- createDataset_internal(.Object@pointer, datasetname, 
          dspace$typechar, dspace$dim, chunksize, maxdimensions, compression, 
          dspace$size)
      
      writeDataSet(dset, data)
      dset
    })

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

checkCompression <- function(compression) {
  if (!(is.numeric(compression) | is.integer(compression))) {
    stop("Parameter compression must be of type integer/numeric.")
  } 
  if (!length(compression) == 1) {
    stop("Parameter compression must be of length one.")
  } 
  if (!((compression >= 0) & (compression <= 9))) {
    stop("Parameter compression must lie between 0 and 9.")
  }
  invisible(TRUE)
  
}

createDataset_internal <- function(loc, datasetname, typechar, dimensions, 
    chunksize, maxdimensions, compression, size) {
  checkChunksize(chunksize)
  checkMaxDimensions(maxdimensions)
  chunksize <- ifelse(!is.na(maxdimensions) & (is.na(chunksize) | 
            chunksize > maxdimensions), 
      maxdimensions, chunksize)
  if (!all(is.na(maxdimensions) | maxdimensions >= dimensions)) {
    stop("Parameter maxdimensions must be equal or exceed data dimension size.")
  } 
  checkCompression(compression)
  
  dsetptr <- CreateDataset(loc, datasetname, typechar, dimensions,
      chunksize, maxdimensions, compression, size)
  
  new("DataSet", dsetptr, datasetname, typechar)
}

#' @rdname CommonFG-DataSet
#' @export
setGeneric("openDataSet", function(.Object, datasetname, type)
      standardGeneric("openDataSet")
)

#' @rdname CommonFG-DataSet
#' @export
setMethod("openDataSet", signature(.Object="CommonFG", 
        datasetname = "character"), 
    function(.Object, datasetname, type) {
      dsetptr <- OpenDataset(.Object@pointer, datasetname)
      dset <- new("DataSet", dsetptr, datasetname, GetDataSetType(dsetptr))
    })

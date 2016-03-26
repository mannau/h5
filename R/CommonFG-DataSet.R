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
#' @param chunksize integer; Chunksize to be used for dataset. If set to 
#' \code{NA}, chunking is disabled for dataset; \code{maxdimensionions} and 
#' \code{compression} have no effect and extensions of DataSet (e.g. through 
#' \code{cbind}, \code{rbind}) are not possible.              
#' @param maxdimensions integer; Maximum dimensions used for dataset, \code{NA} 
#' sets maxdimensions to 'unlimited'.                                                       
#' @param compression integer; Default GZIP compression level to be used, from  
#' 0 (no compression) to 9 (maximum compression), defaults to \code{4}.         
#' @param size numeric; Character length for fixed-length string data types.
#' Default value of -1 creates variable-length strings.
#' @param path character; Relative path to .Object.
#' @param full.names character; Specify if absolute DataSet path names should be
#' returned.
#' @param recursive logical; Specify DatSets should be retrieved recursively
#' from .Object.  
#' @param follow.links logical; Specify if symbolic links should be followed 
#' (only applies if recursive = TRUE).                                              
#' @name CommonFG-DataSet                                                         
#' @rdname CommonFG-DataSet                                                             
#' @include CommonFG.R
#' @aliases createDataSet
#' @export                                                                      
setGeneric("createDataSet", function(
        .Object, datasetname, data, type, dimensions, 
        chunksize = -1, 
        maxdimensions = NA_integer_, 
        compression = 4L, size = -1)
      standardGeneric("createDataSet")
)

#' @rdname CommonFG-DataSet
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", 
        datasetname = "character", 
        data = "missing", type = "character", dimensions = "ANY", 
        chunksize = "ANY", maxdimensions = "ANY", 
        compression = "ANY", size = "ANY"), 
    function(.Object, datasetname, type, dimensions, chunksize, maxdimensions, 
        compression, size) {
      
      stopifnot(!missing(dimensions))
      stopifnot(type %in% c("double", "integer", "logical", "character", 
              "vlen-double", "vlen-integer", "vlen-logical", "vlen-character"))
      typechar <- if(type == "vlen-double") {
        "x"
      } else if (type == "vlen-integer") {
        "y"
      } else if (type == "vlen-logical") {
        stop("Type 'vlen-logical' not supported yet.")
      } else if (type == "vlen-character") {
        stop("Type 'vlen-character' not supported yet.")
      } else {
        substr(type, 1, 1)	
      }

      # Set ChunkSize to default
      if(!is.na(chunksize)[1] & chunksize[1] == -1 & length(chunksize) == 1) {
        chunksize = dimensions
      }
      
      createDataset_internal(.Object, datasetname, typechar, dimensions,
          chunksize, maxdimensions, compression, size) 
    })

#' @rdname CommonFG-DataSet
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", 
        datasetname = "character", data = "ANY", type = "missing", 
        dimensions = "missing", chunksize = "ANY", maxdimensions = "ANY", 
        compression = "ANY", size = "numeric"), 
  function(.Object, datasetname, data, chunksize, maxdimensions, 
      compression, size) {
    if(missing(data)) {
      stop("Parameter data must be specified.")
    }
    # workaround for Bug_AttributeGroupSubset
    if( inherits(data, "DataSet")) {
      return(data)
    }

    # Set Maxdimensions to default
    if(is.na(maxdimensions)[1] && length(maxdimensions) == 1) {
      maxdimensions = rep(NA_integer_, length(GetDimensions(data)))
    }
    # Set ChunkSize to default
    if(!is.na(chunksize)[1] & chunksize[1] == -1 & length(chunksize) == 1) {
      chunksize = ChunkSize(data)
    }

    stopifnot(length(maxdimensions) == length(GetDimensions(data)))
    
    dspace <- GetDataSpace(data)

    dset <- createDataset_internal(.Object, datasetname, 
        dspace$typechar, dspace$dim, chunksize, maxdimensions, compression, 
        size)
    
    writeDataSet(dset, data)
    dset
  })

#' @rdname CommonFG-DataSet
#' @export
setMethod("createDataSet", signature(.Object="CommonFG", 
        datasetname = "character", data = "ANY", type = "missing", 
        dimensions = "missing", chunksize = "ANY", maxdimensions = "ANY", 
        compression = "ANY", size = "missing"), 
  function(.Object, datasetname, data, chunksize, maxdimensions, 
      compression) {
    
    if(missing(data)) {
      stop("Parameter data must be specified.")
    }
    
    # workaround for Bug_AttributeGroupSubset
    if( inherits(data, "DataSet")) {
      return(data)
    }
    dspace <- GetDataSpace(data)
    size <- dspace$size
    createDataSet(.Object, datasetname, data, chunksize = chunksize, 
        maxdimensions = maxdimensions, compression = compression, size = size)
    
  })


checkChunksize <- function(chunksize) {
  if (!(is.numeric(chunksize) | is.integer(chunksize) | is.na(chunksize[1]))) {
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

#' @importFrom methods new
createDataset_internal <- function(.Object, datasetname, typechar, dimensions, 
    chunksize, maxdimensions, compression, size) {
  
  if(existsDataSet(.Object, datasetname)) {
    stop("Dataset with the same name existing at location.")
  }
  checkChunksize(chunksize)
  checkMaxDimensions(maxdimensions)
  chunksize <- ifelse(!is.na(maxdimensions) & (is.na(chunksize) | 
            chunksize > maxdimensions), 
      maxdimensions, chunksize)
  if (!all(is.na(maxdimensions) | maxdimensions >= dimensions)) {
    stop("Parameter maxdimensions must be equal or exceed data dimension size.")
  } 
  checkCompression(compression)
  dsetptr <- CreateDataset(.Object@pointer, datasetname, typechar, dimensions,
      chunksize, maxdimensions, compression, size)
  
  new("DataSet", dsetptr, datasetname, typechar)
}

#' @rdname CommonFG-DataSet
#' @export
setGeneric("openDataSet", function(.Object, datasetname, type)
      standardGeneric("openDataSet")
)

#' @rdname CommonFG-DataSet
#' @importFrom methods new
#' @export
setMethod("openDataSet", signature(.Object="CommonFG", 
        datasetname = "character"), 
    function(.Object, datasetname, type) {
      dsetptr <- OpenDataset(.Object@pointer, datasetname)
      dset <- new("DataSet", dsetptr, datasetname, GetDataSetType(dsetptr))
    })

#' @rdname CommonFG-DataSet
#' @export
setGeneric("list.datasets", function(.Object, path = "/", 
        full.names = TRUE, recursive = TRUE, follow.links = FALSE)
      standardGeneric("list.datasets")
)

#' @rdname CommonFG-DataSet
#' @export
setMethod("list.datasets", signature(.Object="CommonFG"), 
  function(.Object, path, full.names, recursive, follow.links) {
    if (!existsGroup(.Object, path)) {
      stop("Specified path does not exist")
    }
    # Set full path
    path.full <- path
    if(inherits(.Object, "H5Group")) {
      path.full <- paste(.Object@location, path.full, sep = "/")
      path.full <- gsub("/+", "/", path.full)
    }

    dsets <- GetDataSetNames(.Object@pointer, path.full, recursive)
    dsetnames <- rep(path.full, length(dsets))
    
    if (follow.links) {
      symlinks <- paste(path.full, GetSoftLinks(.Object@pointer, path.full), sep = "/")
      symlinks <- gsub("/+", "/", symlinks)
      dsetsymlinks <- lapply(symlinks, function(x) GetDataSetNames(.Object@pointer, x, recursive))
      dsetnames <- c(dsetnames, rep(symlinks, sapply(dsetsymlinks, length)))
      dsets <- c(dsets, do.call(c, dsetsymlinks))
    }
    
    if (length(dsets) > 0) {
      if (full.names) {
        dsets <- paste(dsetnames, dsets, sep="/")
      }
      else {
        dsets <- gsub(".*/(.*)$", "\\1" , dsets)
      }
      dsets <- gsub("/+", "/", dsets) # just to make sure...
    } else {
      dsets <- character(0)
    }
    dsets
  })

#' @rdname CommonFG-DataSet
#' @export
setGeneric("existsDataSet", function(.Object, datasetname)
      standardGeneric("existsDataSet")
)

#' @rdname CommonFG-DataSet
#' @export
setMethod( "existsDataSet", signature(.Object="CommonFG", 
        datasetname = "character"), 
  function(.Object, datasetname) {
    gname <- sub("^\\.$", "/", dirname(datasetname))
    if(!existsGroup(.Object, gname)) {
      return(FALSE)
    }
    basename(datasetname) %in% 
    list.datasets(.Object, gname, full.names = FALSE, recursive = FALSE)
  })


#' The DataSet class
#'
#' The DataSet class used to store data objects in HDF5 tree.
#' 
#' @param .Object,x DataSet; S4 object of class \code{DataSet};
#' @param data,y object; Object to be stored in HDF5 file, can be either of type vector, matrix or array.
#' @param offset numeric; Offset to be selected from Hyperslab.
#' @param count numeric; Count to be selected from Hyperslab.
#' @param dims numeric; Dimensions of DataSet.
#' @param recursive logical; Argument passed to \code{\link{c}}.
#' @param dspace DataSpace; Data space object used for data selection.
#' @param elem matrix; Matrix specifying element selection coordinates. Columns specify rank, rows specify different points.
#' @param ... additional arguments passed to \code{\link{c}}.
#' @name DataSet 
#' @rdname DataSet
#' @aliases DataSet-class
#' @include H5Location.R Dataspace.R
#' @export
setClass( "DataSet", representation( pointer = "externalptr", 
                                     datatype = "character",
                                     dim = "numeric", 
                                     maxdim = "numeric", 
                                     chunksize = "numeric",
                                     compression = "character"), 
  contains = "H5Location")


#' @rdname DataSet
#' @export
setGeneric("checkParamBoundaries", function(.Object, offset, count)
      standardGeneric("checkParamBoundaries")
)

#' @rdname DataSet
#' @export
setMethod("checkParamBoundaries", signature(.Object="DataSet", 
        offset = "numeric", count = "numeric"), 
    function(.Object, offset, count) {
      ### Type checking
      if (!(is.numeric(offset) | is.integer(offset))) {
        stop("Parameter offset must be of type integer/numeric.")
      } 
      if (!(is.numeric(count) | is.integer(count))) {
        stop("Parameter count must be of type integer/numeric.")
      } 
      
      ### Length checking
      if (length(offset) != length(.Object@dim)) {
        stop(sprintf("Parameter offset must have length of dataset dimensions (%d)", 
                length(.Object@dim)))
      }
      if (length(count) != length(.Object@dim)) {
        stop(sprintf("Parameter count must have length of dataset dimensions (%d)", 
                length(.Object@dim)))
      }
      
      ### Check minimum values
      if (!all(offset > 0 | is.na(offset))) {
        stop(sprintf("Elements of parameter offset must be greater than zero or NA", 
                length(.Object@dim)))
      }
      
      if (!all(count > 0 | is.na(count))) {
        stop(sprintf("Elements of parameter count must be greater than zero or NA", 
                length(.Object@dim)))
      }
      
      ### Set NA values of offset to minimum values
      offset[is.na(offset)] <- 1
      
      ### Set NA values of count to residual maximum values
      count[is.na(count)] <- (.Object@dim - offset)[is.na(count)] + 1
      
      ### Boundaries check
      if (!all((offset + count - 1) <= .Object@dim)) {
        stop("subscript out of bounds")
      }
      
      list(offset, count)
    })
    
#' @rdname DataSet
#' @export
setGeneric("selectDataSpace", function(.Object, 
        offset = rep(NA_integer_, length(.Object@dim)), 
        count = rep(NA_integer_, length(.Object@dim)),
        elem)
      standardGeneric("selectDataSpace")
)

#' @rdname DataSet
#' @export
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "missing", count = "missing", elem = "missing"), 
    function(.Object) {
      dspace <- GetDataspaceAll(.Object@pointer)
      new("DataSpace", dspace, .Object@dim)
    })
    
#' @rdname DataSet
#' @export
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "ANY", count = "ANY", elem = "missing"), 
  function(.Object, offset, count) {
    out <- checkParamBoundaries(.Object, offset, count)
    offset <- out[[1]]
    count <- out[[2]]
    dspace <- GetDataspace(.Object@pointer, offset - 1, count)
    new("DataSpace", dspace, count)
  })

#' @rdname DataSet
#' @export
setMethod("selectDataSpace", signature(.Object = "DataSet", 
        offset = "missing", count = "missing", elem = "matrix"), 
    function(.Object, elem) {
      if(any(is.na(elem))) {
        stop("NAs are not allowed in element coordinates.")
      }
      if(any(elem < 1L)) {
        stop("Elements of parameter elem must be greater or equal than one.")
      }
      
      elemtype <- typeof(elem)
      if(!elemtype %in% c("double", "integer")) {
        stop("Element matrix must be of type double or integer.")
      }
      
      if(ncol(elem) != length(.Object@dim)) {
        stop("Number of elem matrix columns must equal length of dataset dimensions.")
      }
      
      dimmax <- apply(elem, 2, max)
      if (any(dimmax > .Object@dim)) {
        stop("subscript out of bounds")
      }

      dspace <- GetDataspaceElem(.Object@pointer, t(elem - 1))
      new("DataSpace", dspace, count = nrow(elem))
    })

#' @rdname DataSet
#' @export
setGeneric("writeDataSet", function(.Object, data, 
        dspace = selectDataSpace(.Object,  rep(NA_integer_, length(.Object@dim)), 
            GetDimensions(data)), 
        transpose = TRUE)
      standardGeneric("writeDataSet")
)

#' @rdname DataSet
#' @param transpose logical; Determine if data object (if is array) should be transposed.
#' @export
setMethod("writeDataSet", signature(.Object="DataSet", data = "ANY", dspace = "ANY", 
        transpose = "ANY"), 
		function(.Object, data, dspace, transpose) {      
      stopifnot(inherits(dspace, "DataSpace"))
      if(prod(GetDimensions(data)) != prod(dspace@count)) {
        stop("number of items to replace is not equal to replacement length")
      }
      rank <- length(GetDimensions(data))
      if(transpose & rank > 1) {
        if(rank == 2) {
          data = t(data)
        } else if (rank > 2) {
          data <- aperm(data, rev(1:length(dim(data))))
        }
      }
			res <- WriteDataset(.Object@pointer, dspace@pointer, data, .Object@datatype, dspace@count)
      closeh5(dspace)
      invisible(res)
		})


#' @rdname DataSet
#' @export
setGeneric("readDataSet", function(.Object, 
        dspace = selectDataSpace(.Object, 
          offset = rep(NA_integer_, length(.Object@dim)), 
          count = rep(NA_integer_, length(.Object@dim))))
			standardGeneric("readDataSet")
)

#' @rdname DataSet
#' @export
setMethod("readDataSet", signature(.Object = "DataSet", dspace = "ANY"), 
		function(.Object, dspace) {
      stopifnot(inherits(dspace, "DataSpace"))
      
			dset <- ReadDataset(.Object@pointer, dspace@pointer, dspace@count)
      if(is.matrix(dset)) {
        return(t(dset))
      }
      if(is.array(dset)) {
        return(aperm(dset, rev(1:length(dim(dset)))))
      }
      closeh5(dspace)
      dset   
		})

setMethod( "initialize", "DataSet",
		function(.Object, location, 
                      datatype, 
                      dim = GetDataSetDimensions(location), 
                      maxdim = GetDataSetMaxDimensions(location), 
                      chunksize = GetDataSetChunksize(location), 
                      compression = GetDataSetCompression(location)) {
			.Object@pointer = location
			.Object@datatype = datatype
      .Object@dim = dim      
      .Object@maxdim = maxdim   
      .Object@chunksize = chunksize
      .Object@compression = compression
			.Object
		})

#' @rdname DataSet
#' @export
setMethod("closeh5", signature(.Object="DataSet"), function(.Object) {
			invisible(CloseDataset(.Object@pointer))
		})

#' @rdname DataSet
#' @export
setGeneric("extendDataSet", function(.Object, dims)
      standardGeneric("extendDataSet")
)

#' @rdname DataSet
#' @export
setMethod("extendDataSet", signature(.Object="DataSet", dims = "numeric"), 
    function(.Object, dims) {
      # TODO: should be checked if necessary
      if(length(dims) != length(.Object@dim)) {
        stop("Number of extendible dimensions must agree with DataSet dimensions.")
      }
      if(!all(dims >= .Object@dim)) {
        stop("Number of extendible dimensions must be greater or equal than DataSet dimensions.")
      }
      
      if(!all(dims <= .Object@maxdim)) {
        stop("Number of extendible dimensions exceeds maximum dimensions of DataSet.")
      }

      ExtendDataset(.Object@pointer, dims)
      .Object@dim <- GetDataSetDimensions(.Object@pointer)
      .Object
    })

#' @rdname DataSet
#' @param deparse.level numeric; Not implemented yet. 
#' @export
rbind.DataSet <- function (..., deparse.level = 1)  {
	args <- list(...)
	rbind2(args[[1]], do.call(rbind, args[-1]))
}

#' @rdname DataSet
#' @export
setMethod("rbind2", signature(x="DataSet", y = "matrix"), 
    function(x, y) {
      nrowx <- x@dim[1L]
      nrowy <- dim(y)[1L]
      ncolx <- x@dim[2L]
      ncoly <- dim(y)[2L]
      
      if(ncolx != ncoly) {
        stop(sprintf("Data to append does not match dataset dimensions (%d != %d).", 
                ncolx, ncoly))
      }
      
      dtype <- substr(typeof(y), 1, 1)
      if(x@datatype != dtype) {
        stop("Data to append does not match type of DataSet.")
      }
      
      newdims <- c(nrowx + nrowy, ncolx)
      x <- extendDataSet(x, newdims)
      dspace <- selectDataSpace(x, offset = c(nrowx + 1, 1), count = GetDimensions(y))
      writeDataSet(x, y, dspace)
      x
})

#' @rdname DataSet
#' @export
cbind.DataSet <- function (..., deparse.level = 1)  {
	args <- list(...)
	cbind2(args[[1]], do.call(cbind, args[-1]))
}

#' @rdname DataSet
#' @export
setMethod("cbind2", signature(x="DataSet", y = "matrix"), 
  function(x, y) {
    nrowx <- x@dim[1L]
    nrowy <- dim(y)[1L]
    ncolx <- x@dim[2L]
    ncoly <- dim(y)[2L]
    
    if(nrowx != nrowy) {
      stop(sprintf("Data to append does not match dataset dimensions (%d != %d).", 
              nrowx, nrowy))
    }
    
    dtype <- substr(typeof(y), 1, 1)
    if(x@datatype != dtype) {
      stop("Data to append does not match type of DataSet.")
    }
    
    newdims <- c(nrowx, ncolx + ncoly)
    x <- extendDataSet(x, newdims)
    dspace <- selectDataSpace(x, offset = c(1, ncolx + 1), count = GetDimensions(y))
    writeDataSet(x, y, dspace)
    x
  })

#' @rdname DataSet
#' @export
setMethod("c", "DataSet", 
    function(x, ..., recursive=FALSE) {
      
    y <- do.call(c, list(..., recursive = recursive))
    
    dtype <- substr(typeof(y), 1, 1)
    if(x@datatype != dtype) {
      stop("Data to append does not match type of DataSet.")
    }
    olddim <- x@dim[1L]
    newdims <- c(x@dim[1L] + length(y))
    x <- extendDataSet(x, newdims)
    dspace <- selectDataSpace(x, offset = olddim + 1, count = GetDimensions(y))
    writeDataSet(x, y, dspace)
    x
  })

dataSpaceFromIndex <- function(dset, indices) {
  stopifnot(inherits(dset, "DataSet"))
  stopifnot(is.list(indices))

  if(length(indices) == 0) {
    return(selectDataSpace(dset))
  }
  if(length(indices) != length(dset@dim)) {
    stop("incorrect number of dimensions") 
  }
  # TODO: Include support for logical indices
  if (!all(sapply(indices, function(dset) is.numeric(dset) | is.integer(dset)))) {
    stop("Subscript indices must be of type numeric or integer.") 
  }
  selectDataSpace(dset, elem = as.matrix(expand.grid(indices)))
}

subsetDataSet <- function(x, i, j, ..., drop = TRUE) {
  if(missing(i)) {
    i <- integer(0)
  }
  if(missing(j)) {
    j <- integer(0)
  }
  indices <- list(i, j, ...)
  indices <- indices[sapply(indices, length) > 0]
  dspace <- dataSpaceFromIndex(x, indices)
  
  vec <- readDataSet(x, dspace)
  setdim <- length(x@dim)
  stopifnot(setdim >= 1)
  if (setdim == 1) {
    return(vec)
  } else if (setdim == 2) {
    return(matrix(vec, nrow = length(i)))
  }
  # TODO: remove workaround
  adim <- NULL
  if (length(indices) == 0) {
    adim <- x@dim
  } else {
    adim <- sapply(indices, length)
  }
  array(vec, dim = adim)
# TODO: drop is not implemented yet
}

#' @rdname DataSet
#' @param i integer; row index
#' @param j integer; column index
#' @param drop logical; specify if 
#' @export
setMethod("[", c("DataSet", "ANY", "ANY", "ANY"),
  function(x, i, j, ..., drop=TRUE) {
    subsetDataSet(x, i, j, ..., drop = drop)
  })

#' @rdname DataSet
#' @export
setMethod("[", c("DataSet", "missing", "missing", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
      rank <- length(x@dim)
      stopifnot(rank >= 1)
      res <- NULL
      if(rank == 1) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        res <- x[1:x@dim[1], drop = drop]
      } else if (rank == 2) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        return(x[1:x@dim[1], 1:x@dim[2], drop = drop])
      } else {
        if(missing(...)) {
          res <- readDataSet(x)
        } else {
          addargs <- tryCatch({	test <- list(...)
                      TRUE}, error = function(e) FALSE)
          if(addargs) {
            res <- do.call("[", c(list(x), lapply(x@dim[1:2], 
                        function(x) 1:x), list(...), list(drop = drop)))
          } else {
            res <- readDataSet(x)
          }
        }
      }
      res
  })

#' @rdname DataSet
#' @export
setMethod("[", c("DataSet", "numeric", "missing", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
      rank <- length(x@dim)
      stopifnot(rank >= 1)
      res <- NULL
      if(rank == 1) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        res <- subsetDataSet(x, i = i, drop = drop)
      } else if (rank == 2) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        return(x[i, 1:x@dim[2], drop = drop])
      } else {
        if(missing(...)) {
          stop("incorrect number of dimensions")
        }
        addargs <- tryCatch({	test <- list(...)
              TRUE}, error = function(e) FALSE)
        if(addargs) {
          res <- do.call("[", c(list(x), list(i), list(1:x@dim[2]), 
                  list(...), list(drop = drop)))
        } else {
          res <- do.call("[", c(list(x), list(i), 
                  lapply(x@dim[-1], function(x) 1:x), list(drop = drop)))
        }
      }
      res
    })

#' @rdname DataSet
#' @export
setMethod("[", c("DataSet", "missing", "numeric", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
      rank <- length(x@dim)
      stopifnot(rank >= 1)
      res <- NULL
      if(rank == 1) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        res <- subsetDataSet(x, j = j, drop = drop)
      } else if (rank == 2) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        return(x[1:x@dim[1], j, drop = drop])
      } else {
        if(missing(...)) {
          stop("incorrect number of dimensions")
        }
        addargs <- tryCatch({	test <- list(...)
              TRUE}, error = function(e) FALSE)
        if(addargs) {
          res <- do.call("[", c(list(x), list(1:x@dim[1]), 
                  list(j), list(...), list(drop = drop)))
        } else {
          res <- do.call("[", c(list(x), list(1:x@dim[1]), 
                  list(j), lapply(x@dim[-c(1:2)], function(x) 1:x), list(drop = drop)))
        }
      }
      res
    })

writeSubsetDataSet <- function(x, i, j, ..., value) {
  if(missing(i)) {
    i <- integer(0)
  }
  if(missing(j)) {
    j <- integer(0)
  }
  indices <- list(i, j, ...)
  indices <- indices[sapply(indices, length) > 0]
  dspace <- dataSpaceFromIndex(x, indices)
  writeDataSet(x, value, dspace, transpose = FALSE) 
  closeh5(dspace)
  x
}


#' @rdname DataSet
#' @param value object; Value to be assigned to dataset
#' @export
setMethod("[<-", c("DataSet", "missing", "missing", "ANY"),
  function(x, i, j, ..., value) {
    rank <- length(x@dim)
    stopifnot(rank >= 1)
    if(rank == 1) {
      if(!missing(...) | !missing(j)) {
        stop("incorrect number of dimensions")
      }
      x[1:x@dim[1]] <- value
    } else if (rank == 2) {
      if(!missing(...)) {
        stop("incorrect number of dimensions")
      }
      x[1:x@dim[1], 1:x@dim[2]] <- value
    } else {
      if(missing(...)) {
        stop("incorrect number of dimensions")
      }
      addargs <- tryCatch({	test <- list(...)
            TRUE}, error = function(e) FALSE)
      if(addargs) {
        do.call("[<-", c(list(x), lapply(x@dim[1:2], 
                    function(x) 1:x), list(...), list(value = value)))
      } else {
        writeDataSet(x, value, selectDataSpace(x)) 
      }
    }
    x
  })

#' @rdname DataSet
#' @export
setMethod("[<-", c("DataSet", "numeric", "missing", "ANY"),
    function(x, i, j, ..., value) {
      rank <- length(x@dim)
      stopifnot(rank >= 1)
      if(rank == 1) {
        if(!missing(...) | !missing(j)) {
          stop("incorrect number of dimensions")
        }
        writeSubsetDataSet(x, i = i, value = value)
      } else if (rank == 2) {
        if(!missing(...)) {
          stop("incorrect number of dimensions")
        }
        x[i, 1:x@dim[2]] <- value
      } else {
        if(missing(...)) {
          stop("incorrect number of dimensions")
        }
        addargs <- tryCatch({	test <- list(...)
              TRUE}, error = function(e) FALSE)
        if(addargs) {
          res <- do.call("[<-", c(list(x), list(i), lapply(x@dim[2], 
                      function(x) 1:x), list(...), list(value = value)))
        } else {
          res <- do.call("[<-", c(list(x), list(i), lapply(x@dim[2], 
                      function(x) 1:x), list(value = value)))
        }
      }
      x
    })

#' @rdname DataSet
#' @export
setMethod("[<-", c("DataSet", "missing", "numeric", "ANY"),
  function(x, i, j, ..., value) {
    rank <- length(x@dim)
    if(rank < 2) {
        stop("incorrect number of dimensions")
    } else if (rank == 2) {
      if(!missing(...)) {
        stop("incorrect number of dimensions")
      }
      writeSubsetDataSet(x, i = 1:x@dim[1], j = j, value = value)
    } else {
      if(missing(...)) {
        stop("incorrect number of dimensions")
      }
      addargs <- tryCatch({	test <- list(...)
            TRUE}, error = function(e) FALSE)
      if(addargs) {
        res <- do.call("[<-", c(list(x), list(1:x@dim[1]), list(j), 
                list(...), list(value = value)))
      } else {
        res <- do.call("[<-", c(list(x), list(1:x@dim[1]), list(j), 
                list(value = value)))
      }
    }
    x 
  })

#' @rdname DataSet
#' @export
setMethod("[<-", c("DataSet", "ANY", "ANY", "ANY"),
  function(x, i, j, ..., value) {
    writeSubsetDataSet(x, i = i, j = j, ..., value = value)
  })

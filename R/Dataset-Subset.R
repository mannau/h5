#' Operators to Subset DataSet Objects
#'
#' \code{\link{DataSet}} objects can be subsetted to retrieve or write sub--regions
#' as defined by a \code{\link{DataSpace}}. The subsetting command should be 
#' the same to base R subsetting of vectors, matrices or arrays. Also missing 
#' subsetting parameters are supported. 
#' 
#' For subset--write operations it has to be noted that no recycling is done 
#' by the function and therefore the written object needs to have the same 
#' dimensions as the subsetted region.
#' 
#' @rdname DataSet-Subset
#' @name DataSet-Subset
#' @param x DataSet; S4 object of class \code{DataSet};
#' @param i integer; row index
#' @param j integer; column index
#' @param drop logical; specify if
#' @param ... additional arguments for subsetting
#' @note Subsetting \code{\link{DataSet}}s can become slow for many selection
#' points since element--wise selection is used, see also \code{\link{DataSpace}}.
#' @examples
#' # Write submatrix to sub-region of DataSet
#' testmat_n <- matrix(as.integer(1:90), ncol = 9)
#' file <- h5file("test.h5", "a")
#' file["testgroup/testmat_n2"] <- testmat_n
#' submat <- matrix(-1L:-9L, nrow = 3)
#' dset2 <- file["testgroup/testmat_n2"]
#' dset2[c(1, 3, 5), c(1, 3, 5)] <- submat
#' h5close(dset2)
#' h5close(file)
#' file.remove("test.h5")
NULL

#' @rdname DataSet-Subset
#' @export
setMethod("[", c("DataSet", "ANY", "ANY", "ANY"),
  function(x, i, j, ..., drop=TRUE) {
    subsetDataSet(x, i, j, ..., drop = drop)
  })

#' @rdname DataSet-Subset
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
      res <- readDataSet(x)
    } else if (rank == 2) {
      if(!missing(...)) {
        stop("incorrect number of dimensions")
      }
      res <- readDataSet(x)
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

#' @rdname DataSet-Subset
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

#' @rdname DataSet-Subset
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
  writeDataSet(x, value, dspace)#, transpose = FALSE) 
  h5close(dspace)
  x
}

#' @rdname DataSet-Subset
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

#' @rdname DataSet-Subset
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

#' @rdname DataSet-Subset
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

#' @rdname DataSet-Subset
#' @param value object; Value to be assigned to dataset
#' @export
setMethod("[<-", c("DataSet", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., value) {
      writeSubsetDataSet(x, i = i, j = j, ..., value = value)
    })

#' @importFrom methods new
dataSpaceFromIndex <- function(dset, indices, maxspaces = 100) {
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
  if (any(is.na(unlist(indices)))) {
    stop("NAs are not allowed in element coordinates.")
  }
  if(any(unlist(indices) < 1L)) {
    stop("Elements of parameter elem must be greater or equal than one.")
  }
  # Calculate contiguous spaces in selection
  startidx <- lapply(indices, function(x) 
        if(length(x) > 1) which(c(TRUE, diff(x) > 1)) else 1)
  count <- lapply(1:length(startidx), function(i) 
        diff(c(startidx[[i]], length(indices[[i]]) + 1)))
  countsum <- sapply(count, sum)
  
  startidx <- as.matrix(expand.grid(startidx))
  count <- as.matrix(expand.grid(count))
  offset <- do.call(cbind, lapply(1:length(indices), 
          function(i) indices[[i]][startidx[, i]]))
  
  # check if entire space is selected
  if (nrow(count) == 1) {
    if (all(count == dset@dim) & all(offset == 1)) {
      dspace <- selectDataSpace(dset)
      return(dspace)
    }
  }

  dspace <- NULL
  if(nrow(offset) > maxspaces) {
    dspace <- selectDataSpace(dset, elem = as.matrix(expand.grid(indices)))
  } else {
    dspaceptr <- GetDataspace(dset@pointer)
    ops <- c("SET", rep("OR", nrow(offset) - 1))
    for(i in 1:nrow(offset)) {
      out <- checkParamBoundaries(dset, offset[i,,drop = TRUE], 
          count[i,,drop = TRUE])
      dspaceptr <- SelectHyperslab(dspaceptr, out[[1]] - 1, out[[2]], ops[i])
    }
    dspace <- new("DataSpace", dspaceptr, countsum)
  }
  dspace
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
  h5close(dspace)
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

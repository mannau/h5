#' Functions to Create/Open Groups in \code{\link{CommonFG}} objects
#' 
#' Although \code{\link{H5Group}} objects can implicitly be created using 
#' subsetting operators (see \code{\link{CommonFG}}) \pkg{h5} implements more
#' explicit functions (used by subsetting operators under the hood) to create 
#' and open \code{\link{H5Group}s}.
#' @param .Object CommonFG; S4 object of class \code{CommonFG};
#' @param groupname character; HDF5 Group name to be used.
#' @param path character; Relative path to .Object.
#' @param full.names character; Specify if absolute DataSet path names should be
#' returned.
#' @param recursive logical; Specify DatSets should be retrieved recursively
#' from .Object.
#' @rdname CommonFG-Group
#' @name CommonFG-Group
#' @aliases createGroup
#' @include CommonFG.R
#' @export
setGeneric("createGroup", function(.Object, groupname)
      standardGeneric("createGroup")
)

getGroupLocation <- function(x, groupname) {
  location <- if(inherits(x, "H5File")) {
        paste0("/", groupname)
      } else {
        file.path(x@location, groupname, fsep = "/")
      } 
  gsub("/+", "/", location)
}

#' @rdname CommonFG-Group
#' @importFrom methods new
#' @export
setMethod( "createGroup", signature(.Object="CommonFG", 
        groupname = "character"), 
    function(.Object, groupname) {
      groupptr <- CreateGroup(.Object@pointer, sub("^/+", "", groupname))
      new("H5Group", groupptr, getGroupLocation(.Object, groupname))
    })

#' @rdname CommonFG-Group
#' @export
setGeneric("openGroup", function(.Object, groupname)
      standardGeneric("openGroup")
)

#' @rdname CommonFG-Group
#' @importFrom methods new
#' @export
setMethod( "openGroup", signature(.Object="CommonFG", groupname = "character"), 
    function(.Object, groupname) {
      groupptr <- OpenGroup(.Object@pointer, sub("^/+", "", groupname))
      new("H5Group", groupptr, getGroupLocation(.Object, groupname))
    })

#' @rdname CommonFG-Group
#' @export
setGeneric("existsGroup", function(.Object, groupname)
      standardGeneric("existsGroup")
)

#' @rdname CommonFG-Group
#' @export
setMethod( "existsGroup", signature(.Object="CommonFG", 
        groupname = "character"), 
    function(.Object, groupname) {
      ExistsGroup(.Object@pointer, groupname)
    })

#' @rdname CommonFG-Group
#' @export
setGeneric("getH5Group", function(.Object, groupname)
      standardGeneric("getH5Group")
)

#' @rdname CommonFG-Group
#' @export
setMethod( "getH5Group", signature(.Object="CommonFG", groupname = "character"), 
    function(.Object, groupname) {
      stopifnot(length(groupname) == 1)
      if(groupname == "/") {
        return(.Object)
      }
      gnames <- strsplit(groupname, "/")[[1]]
      gnames <- gnames[nchar(gnames) > 0]
      ex <- sapply(1:length(gnames), function(i) 
            existsGroup(.Object, paste(gnames[1:i], collapse = "/")))
      
      if(!all(ex)) {
        excreate <- which(!ex)
        for(i in excreate) {
          gr <- createGroup(.Object, paste(gnames[1:i], collapse = "/"))
          h5close(gr)
        }       
      } 
      openGroup(.Object, groupname)
    })

#' @rdname CommonFG-Group
#' @export
setGeneric("list.groups", function(.Object, path = "/", 
        full.names = TRUE, recursive = TRUE)
      standardGeneric("list.groups")
)

#' @rdname CommonFG-Group
#' @export
setMethod( "list.groups", signature(.Object="CommonFG"), 
  function(.Object, path, full.names, recursive) {
    if ((path != "/") & (!existsGroup(.Object, path))) {
      stop("Specified path does not exist")
    }
   
    if(inherits(.Object, "H5Group")) {
      path <- paste(path, .Object@location, sep = "/")
      path <- gsub("/+", "/", path)
    }
 
    res <- character(0)
    if(full.names) {
      res <- paste(path, GetGroupNames(.Object@pointer, path, recursive), sep = "/")
    }
    else
    {
      res <- GetGroupNames(.Object@pointer, path, recursive)
      res <- sub("/+$", "", res)
      res <- gsub(".*/(.*)$", "\\1" , res)
    }
    res <- unlist(gsub("/+", "/", res))
    res <- sub("/+$", "", res)
    res <- res[!res %in% c(path, "")]
    res
  }
)


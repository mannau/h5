#' The CommonFG Class
#'
#' \code{CommonFG} is the base class of \code{\link{H5File}} and \code{\link{H5Group}} 
#' and represents common  functionality of these two classes. The CommonFG base 
#' class supports various subsetting operators to easily access and manipulate 
#' \code{\link{H5Group}} and \code{\link{DataSet}} objects 
#' (see also \code{\link{CommonFG-Group}} and \code{\link{CommonFG-DataSet}}).
#' 
#' Subsetting operators on \code{CommonFG} objects represent a convenient way
#' to create/access \code{\link{H5Group}} and \code{\link{DataSet}} objects.
#' Currently, only character arguments are supported whereas the first argument
#' specifies the group to be created/accesses and the second the dataset name.
#' 
#' Groups can be created/accessed by simply using one character parameter, e.g.
#' \code{group <- obj["groupname"]}.
#' 
#' DataSets can be either accessed by using  
#' \code{dset <- obj["groupname", "datasetname"]} if existing or initialized by
#' using \code{obj["groupname", "datasetname"] <- value}.
#' 
#' All created objects e.g. \code{group} or \code{dset} should be closed in the
#' end using \code{h5close}.
#' @param .Object CommonFG; S4 object of class \code{CommonFG};
#' @param i character; Name of \code{\link{H5Group}}
#' @param j character; Name of \code{\link{DataSet}}
#' @param x CommonFG; object to be subsetted
#' @param drop logical; specify if class of result set should be dropped (not 
#' implemented yet).
#' @param ... Additional arguments passed to \code{\link{createDataSet}}; only 
#' relevant for assignment operator.
#' @rdname CommonFG
#' @name CommonFG
#' @aliases CommonFG-class
#' @seealso \code{\link{CommonFG-Group}} \code{\link{CommonFG-DataSet}} 
#' \code{\link{H5Location-Attribute}}
#' @examples
#' file <- H5File("test.h5")
#' # Create new DataSet 'testset' in H5Group 'testgroup'
#' file["testgroup", "testset"] <- matrix(1:9, nrow = 3)
#' # Create new DataSet 'testset2' in file root
#' file[, "testset2"] <- 1:10
#' # Retrieve H5Group 'testgroup'
#' group <- file["testgroup"]
#' # Retrieve H5Group 'testset'
#' dset <- group[,"testset"]
#' h5close(dset)
#' h5close(group)
#' h5close(file)
#' file.remove("test.h5")
#' @export
setClass( "CommonFG", representation(location = "character"), 
    contains = "H5Location")

#' @rdname CommonFG
#' @export
setGeneric("h5close", function(.Object)
			standardGeneric("h5close")
)

#' @rdname CommonFG
#' @export
setMethod("[", c("CommonFG", "character", "character", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
      group <- getH5Group(x, i)
      ds <- openDataSet(group, j)
      h5close(group)
      ds
    })

#' @rdname CommonFG
#' @export
setMethod("[", c("CommonFG", "character", "missing", "missing"),
    function(x, i, j, ..., drop=TRUE) {
      getH5Group(x, i)
    })

#' @param value vector/matrix/array; Value to be assigend to dataset
#' @rdname CommonFG
#' @export
setMethod("[<-", c("CommonFG", "character", "character", "ANY"),
    function(x, i, j, ..., value) {
      group <- getH5Group(x, i)
      ds <- createDataSet(group, j, value, ...)
      h5close(group)
      h5close(ds)
      x
    })

#' @rdname CommonFG
#' @export
setMethod("[", c("CommonFG", "missing", "character", "ANY"),
    function(x, i, j, ..., drop=TRUE) {
      openDataSet(x, j)
    })

#' @rdname CommonFG
#' @export
setMethod("[<-", c("CommonFG", "missing", "character", "ANY"),
    function(x, i, j, ..., value) {
      ds <- createDataSet(x, j, value, ...)
      h5close(ds)
      x
    })
GetDimensions <- function(data) {
	datadim <- NULL
	if(is.vector(data)) {
		datadim <- length(data)
	} else if (is.matrix(data)) {
		datadim <- dim(data)
	} else if (is.array(data)) {
		datadim <- dim(data)
	} else {
		stop("Argument data must be of type vector, matrix or array.")
	}
	datadim
}

# Automatically set chunk size
ChunkSize <- function(data) {
	datadim <- GetDimensions(data)
	datadim
}

GetDataSpace <- function(data) {
	mattype <- typeof(data)
	stopifnot(mattype %in% c("double", "integer", "logical", "character"))
	size <- -1
	if(mattype == "character") {
		size = max(nchar(data)) + 1
	}
	typechar <- substr(mattype, 1, 1)
	
	list(typechar = typechar, dim = GetDimensions(data), size = size)
}

transpose_data <- function(dset) {
  if(is.matrix(dset)) {
    return(t(dset))
  }
  if(is.array(dset)) {
    return(aperm(dset, rev(1:length(dim(dset)))))
  }
  dset   
}
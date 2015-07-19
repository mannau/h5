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
  size <- -1
	list(typechar = GetTypechar(data), dim = GetDimensions(data), size = size)
}

GetTypechar <- function(data) {
  mattype <- typeof(data)
  stopifnot(mattype %in% c("double", "integer", "logical", "character", "list"))	
  typechar = NA_character_
  if(mattype == "list") {
    elems <- unique(sapply(data, typeof))
    if(length(elems) > 1 & 
        all(sapply(data, function(x) length(GetDimensions(x))) == 1)) {
      stop("All elements of list must be vectors of the same data type.")
    }
    typechar <- substr(typeof(data[[1]]), 1, 1)
    typechar <- 
        if (typechar == 'd') {
          "x"
        } else if (typechar == 'i') {
          "y"
        } else if (typechar == 'l') {
          stop("Type 'vlen-logical' not supported yet.")
        } else if (typechar == 'c') {
          stop("Type 'vlen-character' not supported yet.")
        } else {
          stop("Data type unknown.")
        }
    
  } else {
    typechar <- substr(mattype, 1, 1)
  }
  typechar
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
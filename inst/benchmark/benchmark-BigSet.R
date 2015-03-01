require(h5)
require(testthat)
fname <- "test.h5"

if(file.exists(fname)) file.remove(fname)
fname.rda <- "test.rda"
if(file.exists(fname.rda)) file.remove(fname.rda)
file <- H5File(fname, "a")

dates <- seq(as.POSIXct("2005-01-01"), as.POSIXct("2015-01-01"), by = "mins")
dat <- cbind(unclass(dates), 
    matrix(as.numeric(seq(length.out = length(dates)*4)), ncol = 4))

time.saveh5 <- system.time(    
    file["/bigset", "bigset", 
        chunksize = c(nrow(dat), 1), 
        compression = 6] <- dat
)

time.saverda <- system.time(    
    save(dat, file = fname.rda, compress = "gzip", compression_level = 6)
)

cbind(time.saveh5, time.saverda)

time.subseth5 <- system.time({
      vec <- seq(1, nrow(dat), by = 60)
      out.subseth5 <- file["/bigset", "bigset"][vec, 2]
    })

time.subseth52 <- system.time({
      vec <- seq(1, nrow(dat), by = 60)
      out.subseth52 <- file["/bigset", "bigset"][]
      out.subseth52 <- out.subseth52[vec, 2, drop = FALSE]
    })

# use hyperslabs
time.subseth53 <- system.time({
      vec <- seq(1, nrow(dat), by = 60)
      dset <- file["/bigset", "bigset"]
      out.subseth53 <- readDataSet(dset, 
          selectDataSpace(dset, offset = c(1, 2), count = c(nrow(dat), 1)))
      out.subseth53 <- out.subseth53[vec,, drop = FALSE]
    })

time.subsetrda <- system.time({
      load(file = "test.rda")
      out.subsetrda <- dat[vec, 2, drop = FALSE]
    })

identical(out.subseth5, out.subseth52)
identical(out.subseth5, out.subseth53)  
identical(out.subseth5, out.subsetrda)  

cbind(time.subseth5, time.subseth52, time.subseth53, time.subsetrda)

closeh5(file)

### Benchmark of save/read times vs save/load

require(h5)
require(testthat)
fname <- "test.h5"

if(file.exists(fname)) file.remove(fname)
fname.rda <- "test.rda"
if(file.exists(fname.rda)) file.remove(fname.rda)

dates <- seq(as.POSIXct("2005-01-01"), as.POSIXct("2015-01-01"), by = "mins")
dat <- cbind(unclass(dates), 
    matrix(as.numeric(seq(length.out = length(dates)*4)), ncol = 4))

if(file.exists(fname)) file.remove(fname)
time.saveh5 <- system.time({   
	file <- h5file(fname, "a")
	file["/bigset", "bigset", 
        chunksize = c(nrow(dat), 1), 
        compression = 6] <- dat
	h5close(file)
})

if(file.exists(fname.rda)) file.remove(fname.rda)
time.saverda <- system.time(    
    save(dat, file = fname.rda, compress = "gzip", compression_level = 6)
)

benchmark.save <- cbind(time.saveh5, time.saverda)
#            time.saveh5 time.saverda
# user.self         0.35        20.39
# sys.self          0.07         0.11
# elapsed           0.75        20.51

time.subseth5 <- system.time({
	  file <- h5file(fname, "r")
      vec <- seq(1, nrow(dat), by = 60)
      dset <- file["/bigset", "bigset"]
	  out.subseth5 <- dset[vec, 2]
	  h5close(dset)
	  h5close(file)
    })

# read entire set
time.subseth52 <- system.time({
	  file <- h5file(fname, "r")
      vec <- seq(1, nrow(dat), by = 60)
	  dset <- file["/bigset", "bigset"]
      out.subseth52 <- dset[]
      out.subseth52 <- out.subseth52[vec, 2, drop = FALSE]
	  h5close(dset)
	  h5close(file)
    })

# use hyperslabs
time.subseth53 <- system.time({
	  file <- h5file(fname, "r")
      vec <- seq(1, nrow(dat), by = 60)
      dset <- file["/bigset", "bigset"]
      out.subseth53 <- readData(dset, 
          selectDataSpace(dset, offset = c(1, 2), count = c(nrow(dat), 1)))
      out.subseth53 <- out.subseth53[vec,, drop = FALSE]
	  h5close(dset)
	  h5close(file)
    })

time.subsetrda <- system.time({
      load(file = "test.rda")
      out.subsetrda <- dat[vec, 2, drop = FALSE]
    })

file.remove(fname)
file.remove(fname.rda)

identical(out.subseth5, out.subseth52)
identical(out.subseth5, out.subseth53)  
identical(out.subseth5, out.subsetrda)  

benchmark.read <- cbind(time.subseth5, time.subseth52, time.subseth53, time.subsetrda)
#            time.subseth5 time.subseth52 time.subseth53 time.subsetrda
# user.self          15.97           0.42           0.02           0.67
# sys.self            0.01           0.11           0.04           0.06
# elapsed            15.99           0.53           0.05           0.73

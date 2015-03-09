context("DataSet-createDataset")

fname <- "test.h5"

test_that("DataSet-createDataset",{	
	if(file.exists(fname)) file.remove(fname)
	file <- new( "H5File", fname, "a")
  
  f <- function() dset1 <- createDataSet(file, "testmat_n")
  expect_that(f(), throws_error("Parameter data must be specified"))
  h5close(file)
})  

test_that("DataSet-createDataset-chunksize",{	
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  
  f <- function() dset1 <- createDataSet(file, "testmat_n", 1:10, chunksize = "test")
  expect_that(f(), throws_error("Parameter chunksize must be of type integer"))
  
  dset_c9 <- createDataSet(file, "test_chunk_9", 1:10, chunksize = 9)
  h5close(dset_c9)
  
  dset_c1 <- createDataSet(file, "test_chunk_1", 1:10, chunksize = 1)
  h5close(dset_c1)
  
  f <- function() dset_c0 <- createDataSet(file, "test_chunk_0", 1:10, chunksize = 0)
  expect_that(f(), throws_error("All elements of chunksize must be greater than zero"))
  
  h5close(file)
})  

test_that("DataSet-createDataset-maxdimensions",{	
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  
  f <- function() dset1 <- createDataSet(file, "testmat_n", 1:10, maxdimensions = "test")
  expect_that(f(), throws_error("Parameter maxdimensions must be of type integer"))
  
  f <- function() dset_wrongdim <- createDataSet(file, "test_md_wrongdim", 1:10, 
        maxdimensions = c(NA_integer_, NA_integer_))
  expect_that(f(), throws_error("length\\(maxdimensions\\) == length\\(GetDimensions\\(data\\)\\)"))
  
  f <- function() dset_wrongdim <- createDataSet(file, "test_md_wrongdim", 1:10, 
        maxdimensions = 9)
  expect_that(f(), throws_error("Parameter maxdimensions must be equal or exceed data dimension size"))
  
  dset_md_10 <- createDataSet(file, "test_md_10", 1:10, maxdimensions = 10)
  h5close(dset_md_10)
 
  testmat <- matrix(rep(1:10, 10), nrow = 10)
  f <- function() dset_wrongdim_9_10 <- createDataSet(file, "test_md_100", testmat, 
        maxdimensions = c(9, 10))
  expect_that(f(), throws_error("Parameter maxdimensions must be equal or exceed data dimension size"))
 
  dset_md_10_10 <- createDataSet(file, "test_md_10_10", testmat, maxdimensions = c(10, 10))
  h5close(dset_md_10_10)
  
  h5close(file)
})  

test_that("DataSet-createDataset-compression",{	
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  
  f <- function() dset_cp_type <- createDataSet(file, "cp_type_n", 1:10, compression = "test")
  expect_that(f(), throws_error("Parameter compression must be of type integer"))
  
  f <- function() dset_cp_-1 <- createDataSet(file, "cp_-1", 1:10, compression = -1)
  expect_that(f(), throws_error("Parameter compression must lie between 0 and 9"))
  
  f <- function() dset_cp_10 <- createDataSet(file, "cp_10", 1:10, compression = 10)
  expect_that(f(), throws_error("Parameter compression must lie between 0 and 9"))
  
  for(i in 0:9) {
    dset <- createDataSet(file, sprintf("cp_%d", i), 1:10, compression = i)
    h5close(dset)
  }
  
  h5close(file)
})  

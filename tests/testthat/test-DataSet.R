context("DataSet-createDataset")

fname <- "test.h5"

test_that("DataSet-createDataset",{	
	if(file.exists(fname)) file.remove(fname)
	file <- h5file(fname, "a")
  
  f <- function() dset1 <- createDataSet(file, "testmat_n")
  expect_that(f(), throws_error("Parameter data must be specified"))
  
# TODO: Reference to file still exists for lines below
#  # Provoke Error in internal function
#  f <- function() h5:::CreateDataset(file@pointer, datasetname = "test1", datatype = "i", 
#      dimensions = c(10, 10), chunksize = c(10, 10), maxshape = c(100, 100), 
#      compressionlevel = 0, size = -1)
#  ds <- f()
#  expect_that(f(), throws_error("Creation of DataSet failed"))

  h5close(file)
  expect_that(file.remove(fname), is_true())
})  

test_that("DataSet-createDataset-chunksize",{	
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  
  f <- function() dset1 <- createDataSet(file, "testmat_n", 1:10, chunksize = "test")
  expect_that(f(), throws_error("Parameter chunksize must be of type integer"))
  
  dset_c9 <- createDataSet(file, "test_chunk_9", 1:10, chunksize = 9)
  h5close(dset_c9)
  
  dset_c1 <- createDataSet(file, "test_chunk_1", 1:10, chunksize = 1)
  h5close(dset_c1)
  
  f <- function() dset_c0 <- createDataSet(file, "test_chunk_0", 1:10, chunksize = 0)
  expect_that(f(), throws_error("All elements of chunksize must be greater than zero"))
  
  ds_nochunk <- createDataSet(file, datasetname = "dset", data = 1:3, chunksize = NA)
  expect_that(ds_nochunk@chunksize, is_identical_to(NA_real_))
  expect_that(ds_nochunk@maxdim, is_identical_to(3))
  expect_that(ds_nochunk@compression, is_identical_to(character(0)))
  h5close(ds_nochunk)
  
  ds_chunk <- createDataSet(file, datasetname = "dset2", data = 1:3)
  expect_that(ds_chunk@chunksize, is_identical_to(3))
  expect_that(ds_chunk@maxdim, is_more_than(1e+19))
  expect_that(ds_chunk@compression , is_identical_to("H5Z_FILTER_DEFLATE"))
  expect_that(ds_chunk@datatype, is_identical_to("i"))
  h5close(ds_chunk)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})  

test_that("DataSet-createDataset-maxdimensions",{	
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  
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
  expect_that(file.remove(fname), is_true())
})  

test_that("DataSet-createDataset-compression",{	
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  
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
  expect_that(file.remove(fname), is_true())
})  




test_that("DataSet-list-dataset",{	
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  
  f <- function() list.datasets(file, path = "a/be/bu")
  expect_that(f(), throws_error("Specified path does not exist"))
  
  expect_that(list.datasets(file), is_identical_to(character(0)))
  
  file["testgroup/testset"] <- 1:3
  expect_that(list.datasets(file), is_identical_to(c("/testgroup/testset")))

  # TODO: Fix Bug implicit group extract/create
  #expect_that(list.datasets(file["/testgroup"], recursive = FALSE), 
  #    is_identical_to(c("/testgroup/testset")))
  testgroup <- file["/testgroup"]
  expect_that(list.datasets(testgroup, recursive = FALSE),          
      is_identical_to(c("/testgroup/testset")))
  h5close(testgroup)
  expect_that(list.datasets(file, full.names = FALSE), is_identical_to(c("testset")))

  file["testgroup/testgroup1/testset1"] <- 1:3
  file["testgroup/testgroup2/testset2"] <- 1:3
  file["testgroup3/testgroup3/testset3"] <- 1:3
  group <- file["testgroupN"]
  h5close(group)
  
  ex <- c("/testgroup/testgroup1/testset1", "/testgroup/testgroup2/testset2",
          "/testgroup/testset", "/testgroup3/testgroup3/testset3")
  expect_that(list.datasets(file), is_identical_to(ex))
  
  ex <- c("testset1", "testset2", "testset", "testset3")
  expect_that(list.datasets(file, full.names = FALSE), is_identical_to(ex))
  
  ex <- c("/testgroup/testgroup1/testset1", "/testgroup/testgroup2/testset2",
          "/testgroup/testset")
  testgroup <- file["testgroup"]
  
  # TODO: Fix Bug implicit group extract/create
  #expect_that(list.datasets(file["testgroup"]), is_identical_to(ex))
  expect_that(list.datasets(testgroup), is_identical_to(ex))
     
  ex <- c("testset1", "testset2", "testset")
  #expect_that(list.datasets(file["testgroup"], full.names = FALSE), is_identical_to(ex))
  expect_that(list.datasets(testgroup, full.names = FALSE), is_identical_to(ex))
  h5close(testgroup)
  
  h5close(file)
  expect_that(file.remove(fname), is_true())
})  

test_that("DataSet-list-dataset",{	
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  
  file["ABC/1A"] <- 1:3
  file["ABC/1B"] <- 1:3
  file["ABC/1C"] <- 1:3
  file["ABC/1D"] <- 1:3
  file["ABC/1E"] <- 1:3
  file["ABC/1F"] <- 1:3
  
  ex <- c("/ABC/1A", "/ABC/1B", "/ABC/1C", "/ABC/1D", "/ABC/1E", "/ABC/1F")
  expect_that(list.datasets(file), is_identical_to(ex))
  
  ex <- c("1A", "1B", "1C", "1D", "1E", "1F")
  expect_that(list.datasets(file, full.names = FALSE), is_identical_to(ex))

  expect_that(list.datasets(file, recursive = FALSE), is_identical_to(character(0)))
  
  h5close(file)
  expect_that(file.remove(fname), is_true())

})

test_that("DataSet-list-dataset-link",{	
  fname <- system.file("test-h5link.h5", package = "h5", mustWork = TRUE)
  file <- h5file(fname, "r")
  
  expect_that(file["hardlink/test2"][], is_identical_to(as.numeric(1:3)))
  expect_that(file["softlink/test3/test"][], is_identical_to(as.numeric(1:3)))
  
  expect_that(list.datasets(file["softlink"]), is_identical_to(character(0)))
  
  exp1 <- c("/softlink/test3/subgroup/test-sub", "/softlink/test3/test")
  expect_that(list.datasets(file["softlink"], follow.link = TRUE), is_identical_to(exp1))
  
  exp2 <- c("/hardlink/test2", "/testgroup/subgroup/test-sub", "/testgroup/test")
  expect_that(list.datasets(file), is_identical_to(exp2))
  
  exp3 <- "/softlink/test3/test"
  expect_that(list.datasets(file["/softlink/test3"], recursive = FALSE), is_identical_to(exp3))

  h5close(file)
})  

test_that("DataSet-Bug-F32-Issue10",{	
  fname <- system.file("test-f32.h5", package = "h5", mustWork = TRUE)
  file <- h5file(fname, "r")
  expect_that(file["floats"][], is_identical_to(c(1, 2, 3)))
  h5close(file)
})

